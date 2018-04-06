unzip_temp <- function(zipfile) {
  # Function unzips zipfile into a temporary directory and returns file path
  
  # Create a temporary directory
  temp_dir <- tempdir()
  if (!file.exists(temp_dir)) {
    dir.create(temp_dir)
  } else {
    file.remove(list.files(temp_dir, full.names=TRUE))
  }
  
  files.old <- list.files(temp_dir, full.names=TRUE)
  
  # unzip
  decompression <-
    system2("unzip",
            args = c(
              paste("-d", temp_dir), # Extract to temp_dir
              "-o", # include override flag
              zipfile # File to unzip
            ),
            stdout = TRUE)
  # unzip(zipfile, exdir=tempdir())
  
  files.new <- list.files(temp_dir, full.names=TRUE)
  
  # file names of unzipped files
  files.unzipped <- setdiff(files.new, files.old)
  
  # Return errors
  if (grepl("Warning message", tail(decompression, 1))) {
    print(decompression)
  }
  
  return(files.unzipped)
  
}