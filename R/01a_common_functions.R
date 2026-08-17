##################################
#                                #
#   Common reading utilities     #
#                                #
##################################


#' Parse data files for a given participant ID
#'
#' Iterates over one or more paths, discovers all contained files (including
#' inside ZIP archives), and dispatches each file to the appropriate
#' device-specific reader based on its format.
#'
#' @param id Character or numeric. Participant identifier used for logging and
#'   passed through to the format-specific readers.
#' @param paths Character vector. One or more file or directory paths to search
#'   for data files. Directories are traversed recursively; ZIP archives are
#'   extracted automatically.
#' @param clean_files Logical, if \code{TRUE}, files will be cleaned and formatted.
#'
#' @return  A list with all data found for one individual, merged per data type. 
#'   Returns \code{NULL} if no valid data files are found.
#'
#' @export
parse_data <- function(id, paths, clean_files = TRUE) {
  cat(paste0("Parsing data for id ", id, "...\n"))
  data <- list()
  for (path in paths) {
    # Get all filenames contained within path
    # Note: this works recursively and deals with zipped files
    filenames <- .get_all_files_in_path.aidR(path)
    for (filename in filenames) {
      data[[filename]] <- .process_file.aidR(filename, id, clean_files)
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
#' @param data The data to clean.
#' @param id Character or numeric participant identifier.
#' @param ... Additional arguments passed to methods.
#'
#' @export
clean <- function(data, id, ...) UseMethod("clean")

#-------------------------------
# General helper functions
#-------------------------------

#' Check whether the file has a recognized extension, then determine its
#' device format and call the corresponding \code{read_*_clean()} function.
#' Files with non-data extensions (e.g. PNG, PDF, TXT) are silently ignored.
#' Unrecognized data files produce a console message.
#'
#' @param filename Character string, with the filename to parse.
#' @param id Character or numeric participant identifier.
#' @param clean_files Logical, if \code{TRUE}, files will be cleaned and formatted.
#'
#' @return A list with the data found in the file.
#'
#' @keywords internal
.process_file.aidR <- function(filename, id, clean_files = TRUE) {
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
  if (clean_files && !is.null(data)) {
    data <- clean(data, id)
  }
  return(data)
}

#' Filter a character vector to retain only recognized data file extensions
#' Removes file paths whose extensions match a list of known non-data formats
#' (PNG, PDF, BIB, JPG, JPEG, TXT, JSON, etc). The check is case-insensitive.
#'
#' @param filenames Character vector. File paths to filter.
#'
#' @return A character vector containing only the paths whose extensions are
#'   \emph{not} in the ignored list. May be of length 0 if all inputs are
#'   filtered out.
#'
#' @keywords internal
.filter_relevant_files.aidR <- function(filenames) {
  ignored_extensions <- c("png", "pdf", "bib", "jpg", "jpeg", "txt", "json")
  pattern <- paste0("\\.(", paste(ignored_extensions, collapse = "|"), ")$")
  exclude <- grepl(pattern, filenames, ignore.case = TRUE)
  return(filenames[!exclude])
}

#' Recursively traverse a directory and return all contained files.
#' Zip archives encountered at any level are automatically extracted to
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
#' [tempfile()]. Invalid or corrupted ZIP archives are skipped with a warning.
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