##################################
#                                #
#   Common reading utilities     #
#                                #
##################################


#' Parse data files for a given participant ID
#'
#' Iterates over one or more paths, discovers all contained files (including
#' inside ZIP archives), and dispatches each file to the appropriate
#' format-specific reader. Supported formats are Glooko, CareLink, mylife,
#' Tidepool, YourLoops and Tandem Source.
#'
#' @param id Character or numeric participant identifier. Used for logging and
#'   stored in the \code{id} column of the cleaned data.
#' @param paths Character vector. One or more file or directory paths to search
#'   for data files. Directories are traversed recursively; ZIP archives are
#'   extracted automatically.
#' @param clean Logical, if \code{TRUE} (the default), the main data types (CGM,
#'   basal, bolus, carbohydrates and SMBG) are cleaned and standardized. If
#'   \code{FALSE}, all data is returned as found in the export.
#'
#' @return A list with all data found for one individual, merged per data type.
#'   \code{NULL} if no valid data files were found.
#'
#' @export
parse_data <- function(id, paths, clean = TRUE) {
  cat(paste0("Parsing data for id ", id, "...\n"))
  data <- list()
  for (path in paths) {
    # Get all filenames contained within path
    # Note: this works recursively and deals with zipped files
    filenames <- .get_all_files_in_path.aidR(path)
    for (filename in filenames) {
      data[[filename]] <- .process_file.aidR(filename, id, clean)
    }
  }
  # No valid files
  if (length(data) == 0) {
    return(NULL)
  }
  
  # Now merge across all filanames
  data <- .merge_per_id.aidR(data)
  return(data)
}

#-------------------------------
# Generic functions
#-------------------------------

#' Generic function for cleaning data
#'
#' @param data The data to clean, an instance of one of the format-specific
#'   classes (\code{glooko}, \code{carelink}, \code{mylife}, \code{tidepool},
#'   \code{yourloops} or \code{tandem_source}).
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, see the format-specific methods
#'   (e.g. \code{\link{clean.glooko}}).
#'
#' @export
clean <- function(data, id, ...) UseMethod("clean")

#-------------------------------
# General helper functions
#-------------------------------

#' Read a single file and dispatch it to the matching format
#'
#' Checks whether the file has a recognized extension, then determines which
#' format it comes from and calls the corresponding \code{read_*()} function.
#' Files with non-data extensions (e.g. PNG, PDF, TXT) are silently ignored.
#' Unrecognized data files produce a console message.
#'
#' @param filename Character string with the filename to parse.
#' @param id Character or numeric participant identifier.
#' @param clean Logical, if \code{TRUE}, the data will be cleaned and standardized.
#'
#' @return A list with the data found in the file. \code{NULL} if the file was
#'   ignored or could not be parsed.
#'
#' @keywords internal
.process_file.aidR <- function(filename, id, clean = TRUE) {
  # Silently ignore non-data files
  filename <- .filter_relevant_files.aidR(filename)
  if (length(filename) == 0) {
    return(NULL)
  }
  if (.is_glooko_format.aidR(filename = filename)) {
    data <- read_glooko(id = id, filename = filename)
  } else if (.is_mylife_format.aidR(filename = filename)) {
    data <- read_mylife(id = id, filename = filename)
  } else if (.is_carelink_format.aidR(filename = filename)) {
    data <- read_carelink(id = id, filename = filename)
  } else if (.is_tidepool_format.aidR(filename = filename)) {
    data <- read_tidepool(id = id, filename = filename)
  } else if (.is_yourloops_format.aidR(filename = filename)) {
    data <- read_yourloops(id = id, filename = filename)
  } else if (.is_tandem_source_format.aidR(filename = filename)) {
    data <- read_tandem_source(id = id, filename = filename)
  } else {
    cat(paste0("Id ", id, ": Failed to parse file '", filename, "'.\n"))
    data <- NULL
  }
  if (clean && !is.null(data)) {
    data <- clean(data, id)
  }
  return(data)
}

#' Filter a character vector to retain only recognized data file extensions
#'
#' Removes file paths whose extensions match a list of known non-data formats
#' (PNG, PDF, BIB, JPG, JPEG, TXT, JSON). The check is case-insensitive.
#'
#' @param filenames Character vector. File paths to filter.
#'
#' @return A character vector containing only the paths whose extensions are
#'   \emph{not} in the ignored list. May be of length 0 if all inputs are
#'   filtered out.
#'
#' @keywords internal
.filter_relevant_files.aidR <- function(filenames) {
  ignored_extensions <- c("png", "pdf", "bib", "jpg", "jpeg", "txt", "json", "numbers")
  pattern <- paste0("\\.(", paste(ignored_extensions, collapse = "|"), ")$")
  exclude <- grepl(pattern, filenames, ignore.case = TRUE)
  return(filenames[!exclude])
}

#' Recursively traverse a directory and return all contained files
#'
#' ZIP archives encountered at any level are automatically extracted to
#' temporary directories and traversed recursively, including nested ZIP files.
#'
#' macOS metadata files and folders such as __MACOSX, .DS_Store,
#' and ._* files are ignored automatically.
#'
#' @param path Character string. Path to a file or directory.
#'
#' @return A character vector containing normalized paths to all discovered
#'   non-ZIP files.
#'
#' @details
#' ZIP archives are extracted into temporary directories created via
#' \code{tempfile()}. Invalid or corrupted ZIP archives are skipped with a warning.
#'
#' @keywords internal
.get_all_files_in_path.aidR <- function(path) {
  if (!file.exists(path) && !dir.exists(path)) {
    stop("Path does not exist: ", path)
  }
  tmp_root <- tempfile("aidR_unzip_")
  dir.create(tmp_root, recursive = TRUE)
  is_ignored <- function(x) {
    bn <- basename(x)
    startsWith(bn, "._") ||
      bn == ".DS_Store" ||
      bn == ".Rapp.history" ||
      grepl("__MACOSX", x, fixed = TRUE)
  }
  sanitize_filename <- function(x) {
    if (.Platform$OS.type == "windows") {
      # Replace characters illegal in Windows filenames: \ / : * ? " < > |
      # but preserve directory separators (forward slash handled separately)
      parts <- strsplit(x, "/", fixed = TRUE)[[1]]
      parts <- gsub('[\\:*?"<>|]', "_", parts, perl = TRUE)
      x <- paste(parts, collapse = "/")
    }
    x
  }
  extract_zip_safely <- function(zip_path, unzip_dir) {
    # List zip contents without extracting
    zip_contents <- tryCatch(
      unzip(zip_path, list = TRUE),
      error = function(e) NULL
    )
    if (is.null(zip_contents)) {
      warning("Skipping invalid zip: ", zip_path)
      return(character())
    }
    extracted_paths <- character()
    for (i in seq_len(nrow(zip_contents))) {
      original_name <- zip_contents$Name[i]
      # Skip directories
      if (grepl("/$", original_name)) next
      sanitized_name <- sanitize_filename(original_name)
      dest_file <- file.path(unzip_dir, sanitized_name)
      # Create subdirectory if needed
      dest_dir <- dirname(dest_file)
      if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
      # Extract single file to temp location, then move to sanitized path
      tryCatch(
        {
          if (original_name == sanitized_name) {
            # Name is safe: extract directly
            unzip(zip_path,
                  files = original_name, exdir = unzip_dir,
                  junkpaths = FALSE
            )
          } else {
            # Name is unsafe: extract to temp file, then rename
            tmp_extract <- tempfile(tmpdir = unzip_dir)
            unzip(zip_path,
                  files = original_name, exdir = tmp_extract,
                  junkpaths = TRUE
            )
            tmp_file <- list.files(tmp_extract,
                                   full.names = TRUE,
                                   recursive = FALSE
            )[1]
            file.rename(tmp_file, dest_file)
          }
          extracted_paths <- c(extracted_paths, dest_file)
        },
        error = function(e) {
          warning(
            "Failed to extract '", original_name, "' from zip: ",
            zip_path, "\nReason: ", conditionMessage(e)
          )
        }
      )
    }
    extracted_paths
  }
  recurse <- function(x) {
    if (is_ignored(x)) {
      return(character())
    }
    if (dir.exists(x)) {
      children <- list.files(x,
                             full.names = TRUE, recursive = FALSE,
                             all.files = TRUE, no.. = TRUE
      )
      return(unlist(lapply(children, recurse), use.names = FALSE))
    }
    if (grepl("\\.zip$", x, ignore.case = TRUE)) {
      unzip_dir <- tempfile("unzipped_", tmpdir = tmp_root)
      dir.create(unzip_dir)
      extracted <- extract_zip_safely(x, unzip_dir)
      return(unlist(lapply(extracted, recurse), use.names = FALSE))
    }
    normalizePath(x)
  }
  unique(recurse(path))
}

#-------------------------------
# Daily insulin totals
#-------------------------------

#' Sum a vector, ignoring missing values
#'
#' Unlike \code{sum(x, na.rm = TRUE)}, a vector without a single non-missing
#' value yields \code{NA} rather than zero.
#'
#' @param x A numeric vector.
#'
#' @return The sum over all non-missing values of \code{x}, or \code{NA} if
#'   \code{x} holds no non-missing value.
#'
#' @keywords internal
.sum_or_na.aidR <- function(x){
  if (all(is.na(x))){ return(NA_real_) }
  
  return(sum(x, na.rm = TRUE))
}

#' Aggregate a standardized data frame into one value per day
#'
#' @param df A standardized data frame, with columns \code{id}, \code{format}
#'   and \code{timestamp}.
#' @param values A numeric vector of the same length as \code{nrow(df)}, holding
#'   the value to be summed per day.
#' @param name Character string with the name of the resulting value column.
#' @param source Character string describing where the daily total comes from,
#'   either \code{"reported"} (given by the export) or \code{"summed"}
#'   (aggregated by aidR).
#'
#' @return A data frame with columns \code{id}, \code{format}, \code{date},
#'   \code{name} and \code{source}, holding one row per day. \code{NULL} if
#'   \code{df} is empty.
#'
#' @keywords internal
.aggregate_per_day.aidR <- function(df, values, name, source){
  if (is.null(df) || nrow(df) == 0){ return(NULL) }
  
  day <- as.character(date(df$timestamp))
  total <- tapply(values, day, .sum_or_na.aidR)
  
  res <- data.frame(
    id     = df$id[1],
    format = df$format[1],
    date   = as.Date(names(total)),
    total  = as.numeric(total),
    source = source
  )
  names(res)[names(res) == "total"] <- name
  rownames(res) <- NULL
  
  return(res[order(res$date), ])
}

#' Total basal insulin delivered per day, summed over the recorded basal rates
#'
#' Used by all formats that do not report a daily basal aggregate themselves.
#' Note that the resulting total is only complete if the export records the
#' basal rates of the entire day.
#'
#' @param basal A standardized basal data frame, with columns \code{id},
#'   \code{format}, \code{timestamp}, \code{rate} and \code{duration}.
#'
#' @return A data frame with columns \code{id}, \code{format}, \code{date},
#'   \code{total_basal} and \code{source}, holding one row per day.
#'   \code{NULL} if \code{basal} is empty.
#'
#' @keywords internal
.total_basal_per_day.aidR <- function(basal){
  if (is.null(basal) || nrow(basal) == 0){ return(NULL) }
  
  .aggregate_per_day.aidR(basal, as.numeric(basal$rate) * as.numeric(basal$duration), "total_basal", "summed")
}

#' Total bolus insulin delivered per day, summed over the recorded boluses
#'
#' Used by all formats that do not report a daily bolus aggregate themselves.
#'
#' @param bolus A standardized bolus data frame, with columns \code{id},
#'   \code{format}, \code{timestamp} and \code{total}.
#'
#' @return A data frame with columns \code{id}, \code{format}, \code{date},
#'   \code{total_bolus} and \code{source}, holding one row per day.
#'   \code{NULL} if \code{bolus} is empty.
#'
#' @keywords internal
.total_bolus_per_day.aidR <- function(bolus){
  if (is.null(bolus) || nrow(bolus) == 0){ return(NULL) }
  
  .aggregate_per_day.aidR(bolus, bolus$total, "total_bolus", "summed")
}
