
fwrite.zip <- function(x, file, ...) {
	## Function writes data to a zipped csv file, requires data.table package
	# x -- data.table object to be written to CSV, then zipped
	# file -- File name of the csv file
	# ... -- Other options passed to "fwrite()"
	require(data.table)

	# Parse file name (without path)
	destFile <- file
	zipFile <- tail(stri_split_fixed(destFile, "/")[[1]], 1)
	csvFile <- substr(zipFile, 1, nchar(zipFile) - 4)

	# Create temporary directory
	if (!file.exists(tempdir())) {
		dir.create(tempdir())
	}

	# Set working directory to tempdir (otherwise zip() includes all the folders...)
	oldwd <- getwd()
	setwd(tempdir())
	
	# Write file to tempdir
	fwrite(x, file = csvFile, ...) 
	
	# Zip tempfile
	zip(destFile, files = csvFile)
		
	# Remove csv file
	quietly <- file.remove(csvFile)
	
	# Change back working directory
	setwd(oldwd)
}
	
	
