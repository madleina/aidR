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
#' Tidepool, YourLoops, Tandem Source and Dexcom Clarity.
#'
#' @param id Character or numeric participant identifier. Used for logging and
#'   stored in the \code{id} column of the cleaned data.
#' @param paths Character vector. One or more file or directory paths to search
#'   for data files. Directories are traversed recursively; ZIP archives are
#'   extracted automatically.
#' @param clean Logical, if \code{TRUE} (the default), the main data types (CGM,
#'   basal, bolus, carbohydrates and SMBG) are cleaned and standardized. If
#'   \code{FALSE}, all data is returned as found in the export.
#' @param ... Additional arguments passed on to the format-specific cleaning
#'   methods, e.g. \code{merge_slow_carbs} for Glooko (see 
#'   \code{\link{clean.glooko}}). Arguments that do not apply to a format are 
#'   skipped. Ignored if \code{clean = FALSE}.
#'
#' @return A list with all data found for one individual, merged per data type
#'   and sorted by \code{timestamp}. Duplicate rows are removed. Rows removed
#'   during cleaning are stored in the attribute \code{removed}, see
#'   \code{\link{removed_entries}}. \code{NULL} if no valid data files were found.
#'
#' @examples
#' path <- system.file("extdata", "glooko_example.zip", package = "aidR")
#'
#' # Raw data, as found in the export
#' data_raw <- parse_data(id = "1", paths = path, clean = FALSE)
#' names(data_raw)
#' head(data_raw$cgm)
#'
#' # Cleaned and standardized data
#' data <- parse_data(id = "1", paths = path)
#' names(data)
#' head(data$cgm)
#'
#' @export
parse_data <- function(id, paths, clean = TRUE, ...) {
  message("Parsing data for id ", id, "...")
  data <- list()
  for (path in paths) {
    # Get all filenames contained within path
    # Note: this works recursively and deals with zipped files
    filenames <- .get_all_files_in_path.aidR(path)
    for (filename in filenames) {
      data[[filename]] <- .process_file.aidR(filename, id, clean, ...)
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
#'   \code{yourloops}, \code{tandem_source} or \code{dexcom_clarity}).
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @return A named list with the cleaned data, see the format-specific methods
#'   (e.g. \code{\link{clean.glooko}}).
#'
#' @keywords internal
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
#' @param ... Additional arguments passed on to \code{clean()}.
#'
#' @return A list with the data found in the file. \code{NULL} if the file was
#'   ignored or could not be parsed.
#'
#' @keywords internal
.process_file.aidR <- function(filename, id, clean = TRUE, ...) {
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
  } else if (.is_dexcom_clarity_format.aidR(filename = filename)) {
    data <- read_dexcom_clarity(id = id, filename = filename)
  } else {
    warning("Id ", id, ": could not recognize the format of file '", filename,
            "'. It does not match any supported platform and is skipped.")
    data <- NULL
  }
  if (clean && !is.null(data)) {
    data <- clean(data, id, ...)
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
  ignored_extensions <- c("png", "pdf", "bib", "jpg", "jpeg", "txt", "json", "numbers", "html")
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

#-------------------------------
# Linking bolus and carbohydrates
#-------------------------------

#' Create a key linking bolus and carbohydrate entries of the same event
#'
#' Some exports record which carbohydrate entry triggered which bolus based on 
#' information from the bolus calculator. To keep this link after splitting the
#' data when cleaning, a unique identifier is created that is stored in both 
#' \code{bolus} and \code{carbs} (no link is encoded as \code{NA}).
#'
#' The key is built from the content of the event and consists of the 
#' participant id and the fields passed in \code{...} 
#' (e.g. timestamp and amounts), separated by \code{"|"}. Events that are
#' identical in all fields are numbered in the order they appear in the export,
#' which makes the key unique by construction. Identifiers given by the export
#' are only used to find out which entries belong together, never as the key
#' itself.
#'
#' @param id Participant identifier.
#' @param ... One or more vectors of equal length, with one element per event,
#'   which together describe the event.
#' @param linked Logical vector with one element per event (or a single value),
#'   \code{TRUE} if the event links a bolus to a carbohydrate entry. All other
#'   events get \code{NA}.
#'
#' @return A character vector with one key per event, \code{NA} where
#'   \code{linked} is not \code{TRUE}.
#'
#' @keywords internal
.create_link_id.aidR <- function(id, ..., linked = TRUE){
  # Format: timestamp as Y-m-d H:M:S and everything else as character
  fields <- lapply(list(...), function(x){
    if (inherits(x, "POSIXt")) format(x, "%Y-%m-%d %H:%M:%S") else as.character(x)
  })
  n <- unique(lengths(fields))
  if (length(fields) == 0 || length(n) != 1){
    stop("All fields describing the event must have the same length.")
  }
  if (n == 0){ return(character(0)) }

  # Create key: paste all info together
  key <- paste(id, do.call(paste, c(fields, sep = "|")), sep = "|")
  # Make sure it is unique: if key from before is duplicated, this will add |1, |2 etc to it
  key <- paste(key, ave(seq_len(n), key, FUN = seq_along), sep = "|")
  # Everything else is NA
  key[!rep_len(linked %in% TRUE, n)] <- NA
  
  return(key)
}

#-------------------------------
# Logging for filters
#-------------------------------

#' Harmonise column types across a list of data frames
#'
#' Prepares a list of data frames for row-binding by resolving column type
#' mismatches. For every column present in more than one data frame, the
#' classes are compared: if they all agree the column is left untouched,
#' otherwise every occurrence of the column is coerced to the most general 
#' common type.
#'
#' @param dfs A list of data frames.
#'
#' @return The input list with mismatched columns coerced to a common type.
#'
#' @keywords internal
.harmonise_column_types.aidR <- function(dfs) {
  all_cols <- unique(unlist(lapply(dfs, names)))
  
  for (col in all_cols) {
    # first class of this column in each df that actually has it
    classes <- vapply(dfs, function(df) {
      if (col %in% names(df)) class(df[[col]])[1] else NA_character_
    }, character(1))
    present <- classes[!is.na(classes)]
    
    # all files agree (or only one file has it) -> leave untouched
    if (length(unique(present)) <= 1) next
    
    target <- .most_general_class.aidR(present)
    
    dfs <- lapply(dfs, function(df) {
      if (col %in% names(df)) df[[col]] <- .coerce_to.aidR(df[[col]], target)
      df
    })
  }
  
  dfs
}

#' Store removed rows with a reason
#'
#' @param data_id A list with all data found for one individual.
#' @param type The data type the rows were removed from (e.g. \code{"carbs"}).
#' @param rows A data frame with the removed rows.
#' @param reason Why the rows were removed.
#'
#' @return \code{data_id} with \code{rows} added to the attribute
#'   \code{removed}, with the column \code{removed_reason}.
#'
#' @keywords internal
.log_removed.aidR <- function(data_id, type, rows, reason){
  if (is.null(rows) || nrow(rows) == 0) return(data_id)
  rows$removed_reason <- reason
  removed <- attr(data_id, "removed")
  if (is.null(removed)) removed <- list()
  # Types may differ (e.g. rows logged during cleaning vs. after merging) -> harmonise
  removed[[type]] <- bind_rows(.harmonise_column_types.aidR(c(list(removed[[type]]), list(rows))))
  
  attr(data_id, "removed") <- removed
  data_id
}
