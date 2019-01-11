# Created by Linus Larsson
# 2019-01-11
# https://lynuhs.com

#install.packages("bigQueryR")
#install.packages("googleAnalyticsR")
#install.packages("googleAuthR")

if(exists("hasRun") == FALSE){
  library(bigQueryR)
  library(googleAnalyticsR)
  library(googleAuthR)
  
  
  # You will have to include all scopes below and make sure to put BigQuery first.
  # There's a problem in a function for BigQuery that will only check the first element in scopes to verify credentials.
  # You can ignore that warning when running the script.
  
  options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/bigquery",
                                          "https://www.googleapis.com/auth/devstorage.full_control",
                                          "https://www.googleapis.com/auth/cloud-platform",
                                          "https://www.googleapis.com/auth/analytics",
                                          "https://www.googleapis.com/auth/analytics.readonly", 
                                          "https://www.googleapis.com/auth/analytics.manage.users.readonly",
                                          "https://www.googleapis.com/auth/analytics.edit",
                                          "https://www.googleapis.com/auth/analytics.manage.users",
                                          "https://www.googleapis.com/auth/analytics.provision"
  ))
  
  gar_auth()
  hasRun <- TRUE
}

gaGetData <- function(id, start, end, dimensions, metrics, filter="ga:pagePath=~.*", segment = "gaid::-1"){
  df <- google_analytics(id,
                         start = start,
                         end = end,
                         metrics = metrics,
                         dimensions = dimensions,
                         filters = filter,
                         segment = segment,
                         samplingLevel = "WALK",
                         max_results = 999999999999)
  
  return (df)
}


###################################################################################
# Set global variables
###################################################################################

# Put your BigQuery project ID here
bq_global_project("PROJECT_ID") 

# Put your BigQuery dataset ID here 
bq_global_dataset("DATASET_ID") 

# Put your Google Analytics View ID here
ga_id <- "XXXXXXXXXX" 

# Name the Table you want to create and send data to in BigQuery.
tableName <- "ga_import"

###################################################################################


# Enter the dimensions and metrics you want to use in the data upload. YOU HAVE TO USE DATE!
dimensions <- c("date", "dimension1","source", "medium", "deviceCategory")
metrics <- c("sessions","transactions","pageviews","uniquePageviews","bounces","itemQuantity")


# Set the date to import from by checking the last date in the table if it exists
if(tableName %in% bqr_list_tables()$tableId){
  start <- as.Date(bqr_query(query =  paste0("SELECT max(date) as date FROM ",tableName))[1,1])+1
  if(start < Sys.Date()-1){
    runScript <- TRUE
  } else {
    runScript <- FALSE
  }
} else {
  start <- Sys.Date()-30
  runScript <- TRUE
}

if(runScript){
  ga <- gaGetData(id = ga_id,
                  start = start,
                  end = Sys.Date()-1,
                  dimensions = dimensions,
                  metrics = metrics)
  
  
  
  # If the table has already been created this function will not try to create another one. 
  bqr_create_table(tableId = tableName, template_data = ga, timePartitioning = TRUE)
  
  # Run this to append your data frame to the table. 
  bqr_upload_data(upload_data = ga, tableId = tableName, overwrite = FALSE)
} else {
  cat("\014")
  print("Table is already up to date!")
}
