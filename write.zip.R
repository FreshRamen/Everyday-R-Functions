write.zip <- function(x, file, path = getwd(), ...) {
	## Function writes data to a zipped csv file. ##
	# x -- Object to be written to CSV, then zipped
	# file -- File name of the csv file (including ".csv")
	# path (optional) -- part to write zip file to
	# ... -- Other options passed to "write.csv()"
	## Parse file name
	zipFile <- paste0(file,".zip")
	## Store old path
	oldPath <- getwd()
	## Create temporary directory, or flush if exists
	if (!file.exists(tempdir())) {dir.create(tempdir())
    } else {file.remove(list.files(tempdir(), full = T, pattern = "*.csv"))}	
	setwd(tempdir())
	## Write file to tempdir
	write.csv(x, file = file, ...)
	## Zip the file
	zip(zipFile, files = file)
	## Move file to working directry
	file.copy(zipFile, file.path(path,zipFile), overwrite = T)
	## Remove old file
	file.remove(file.path(tempdir(),file))
	## Return to old path
	setwd(oldPath)
}