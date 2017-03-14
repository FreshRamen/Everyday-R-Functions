fwrite.zip <- function(x, file, path = getwd(), ...) {
	## Function writes data to a zipped csv file, requires data.table package
	# x -- data.table object to be written to CSV, then zipped
	# file -- File name of the csv file
	# ... -- Other options passed to "fwrite()"
  require(data.table)
	## Parse file name
  if (substr(file,nchar(file)-7,nchar(file)) == ".csv.zip") {
    csvFile <- substr(file,1,nchar(file)-4)
    zipFile <- file
  } else if (substr(file,nchar(file)-3,nchar(file)) == ".csv") {
    csvFile <- file
    zipFile <- paste0(csvFile,".zip")
  } else {
    csvFile <- paste0(paste0(file,".csv"))
    zipFile <- paste0(csvFile,".zip")
  }
	oldPath <- path	# Store old path
	## Create temporary directory, or flush if exists
	if (!file.exists(tempdir())) {
    dir.create(tempdir())
  } else {
    file.remove(list.files(tempdir(), full = T, pattern = "*.csv"))
  }	
	## Write file to tempdir, zip and move to oldPath
  setwd(tempdir())
	fwrite(x, file = csvFile, ...) 
	zip(zipFile, files = csvFile)
	file.copy(zipFile, file.path(oldPath,zipFile), overwrite = T)
	file.remove(csvFile,zipFile)
	## Return to old path
	setwd(oldPath)
}