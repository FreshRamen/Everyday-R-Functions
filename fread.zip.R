fread.zip <- function(zipfile, na.strings=c("","NA"), ...) {
  # Function reads data from a zipped csv file. Requires package data.table
  
  # To speed things up, the file will not be unzipped twice in the same R session. This has the downside that the name of the zipfile and the containing csv files need to follow the eact pattern: filename.csv filename.csv.zip
  
  # Create a temporary directory, flush if exists
  temp_dir <- tempdir()
  if (!file.exists(temp_dir)) {dir.create(temp_dir)}
  
  # Extract csvfile name (zipfile can be full path)
  csvfile <- substr(zipfile, 1, nchar(zipfile)-4)
  csvfile <- unlist(stri_split_fixed(csvfile, "/"))
  csvfile <- csvfile[length(csvfile)]
  csvfile <- file.path(tempdir(), csvfile) # full path to csv file
  
  # Check if file is already unzipped, if not, unzip the file into tempdir
  if (!file.exists(csvfile)){
    unzip(zipfile, exdir=tempdir())    
  }

  # Read the file
  fread(csvfile,
      na.strings = na.strings, # read empty strings as NA
	  ...
    )
}