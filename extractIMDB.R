library(httr)
library(googleCloudStorageR)

gcs_auth()

# Type in the bucket you want to send data to in Google Cloud Storage
bucket_name <- ""


imdbToBQConnector <- function(fileName){
  # Use the parameter fileName to create the correct download URL from IMDB
  url <- paste0("https://datasets.imdbws.com/",fileName,".tsv.gz")
  
  # Download the compressed file to the subfolder "data". Make sure this subfolder exists!
  filePath <- paste0("data/",gsub("\\.","_",fileName),".tsv.gz")
  GET(url, write_disk(filePath, overwrite = TRUE))
  
  # Read the downloaded file. Notice that it's tab separated and that we are identifying NAs.
  data <- as.data.frame(
    readr::read_tsv(
      file = gzfile(filePath), progress = TRUE, na = "\\N"
    )
  )
  file.remove(filePath)
  
  
  # Replace all NA values with empty strings. This make it to null in Google Cloud.
  data[is.na(data)] <- ""
  
  # Remove columns that you don't need
  data <- data[!(colnames(data) %in% c("knownForTitles"))]
  
  
  # Write a csv file in the data folder
  cloudName <- paste0(gsub("\\.","_",fileName),".csv")
  write.csv(data, paste0("data/",cloudName), row.names = FALSE)
  rm(data)
  
  # Upload the csv file to Google Cloud Storage
  gcs_upload(file = paste0("data/",cloudName), bucket = bucket_name, name = cloudName)
  file.remove(paste0("data/",cloudName))
}

imdbToBQConnector("name.basics")
imdbToBQConnector("title.akas")
imdbToBQConnector("title.basics")
imdbToBQConnector("title.crew")
imdbToBQConnector("title.episode")
imdbToBQConnector("title.principals")
imdbToBQConnector("title.ratings")
