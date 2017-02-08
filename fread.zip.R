fread.zip <- function(zipfile, ...) {
  # Function reads data from a zipped csv file. Requires package data.table
  ## Flush or create a temporary directory
  if (!file.exists(tempdir())) {dir.create(tempdir())
  } else {file.remove(list.files(tempdir(), full = T, pattern = "*.csv"))
  }
  ## Unzip the file into the dir
  unzip(zipfile, exdir=tempdir())
  ## Get the files into the dir
  file <- list.files(tempdir(), pattern = "*.csv", full.names = T)
  ## Throw an error if there's more than one
  if(length(file)>1) stop("More than one data file inside zip")
  ## Read the file
  fread(file,
      na.strings = c("","NA"), # read empty strings as NA
	  ...
    )
}