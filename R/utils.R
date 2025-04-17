#' Create a Psych-DS directory structure
#'
#' @param base_dir Path to the base directory
#' @param opt_dirs Optional directories to create
#' @return Invisible NULL
#' @keywords internal
create_psych_ds_structure <- function(base_dir, opt_dirs = NULL) {
  # Create the data directory if it doesn't exist
  data_dir <- file.path(base_dir, "data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }
  
  # Create optional directories
  if (!is.null(opt_dirs) && length(opt_dirs) > 0) {
    for (dir in opt_dirs) {
      dir_path <- file.path(base_dir, dir)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE)
      }
    }
  }
  
  invisible(NULL)
}

#' Copy data files to the Psych-DS data directory
#'
#' @param files Vector of file paths to copy
#' @param dest_dir Destination directory
#' @return Character vector of copied file paths
#' @keywords internal
copy_data_files <- function(files, dest_dir) {
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  copied_files <- character(length(files))
  
  for (i in seq_along(files)) {
    file_path <- files[i]
    if (file.exists(file_path)) {
      dest_file <- file.path(dest_dir, basename(file_path))
      file.copy(file_path, dest_file, overwrite = TRUE)
      copied_files[i] <- dest_file
    }
  }
  
  copied_files
}