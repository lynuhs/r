# Created by Linus Larsson
# 2019-01-31 (edited 2019-04-04)
# https://lynuhs.com

library(bigQueryR)
library(googleCloudStorageR)
library(googleAuthR)
library(dplyr)
library(tidyr)

# Set the relevant scopes
options(googleAuthR.scopes.selected = 
          c("https://www.googleapis.com/auth/bigquery",
            "https://www.googleapis.com/auth/devstorage.full_control",
            "https://www.googleapis.com/auth/devstorage.read_write", 
            "https://www.googleapis.com/auth/cloud-platform"))
bqr_auth()
bqr_global_project("YOUR-BQ-PROJECT-ID")                      # Replace with your own project ID
bqr_global_dataset("YOUR-BQ-DATSET-ID")                       # Replace with your own dataset ID to store table

# This query will be used to extract data from Google BigQuery
query <- "
    SELECT
    CONCAT('id_', CAST(fullvisitorId AS STRING)) AS fullVisitorId,
    MAX(transactionId) AS transactionId,
    date,
    visitStartTime,
    channel
    FROM (
    SELECT
        fullVisitorId,
        transactionId,
        date,
        visitStartTime,
        CASE
        WHEN sm LIKE '%google%cpc%' AND (adn != 'content' OR adn IS NULL) THEN 'Google - Paid'
        WHEN sm LIKE '%bing%cpc%' THEN 'Bing - Paid'
        WHEN sm LIKE 'google%organic' THEN 'Google - Organic'
        WHEN sm LIKE 'bing%organic' THEN 'Bing - Organic'
        ELSE 'Other'
        END AS channel,
        CASE
        WHEN MAX(transactionId) OVER (PARTITION BY fullVisitorId) IS NOT NULL THEN 1
        ELSE 0
        END AS convertingCustomer
    FROM (
        SELECT
        fullVisitorId,
        date,
        visitStartTime,
        CONCAT(trafficSource.source, ' / ', trafficSource.medium) AS sm,
        trafficSource.adwordsClickInfo.adNetworkType AS adn,
        hit.transaction.transactionId AS transactionId
        FROM
        `YOUR-PROJECT-NAME.YOUR_DATASET_ID.ga_sessions_20*` ga,           # Write your own IDs 
        UNNEST(ga.hits) hit
        WHERE
        parse_DATE('%y%m%d',
            _TABLE_SUFFIX) BETWEEN DATE('2018-01-01')
        AND DATE('2018-12-31')
        GROUP BY
        fullVisitorId,
        date,
        trafficSource.source,
        trafficSource.medium,
        visitStartTime,
        trafficSource.adwordsClickInfo.adNetworkType,
        hit.transaction.transactionId ))
    WHERE
    convertingCustomer = 1
    GROUP BY
    fullvisitorId,
    date,
    visitStartTime,
    channel
    ORDER BY
    fullVisitorId,
    visitStartTime
" 

# Since the data will be too large to import directly to R, you will have to store it in a bucket in Google Storage
bqr_query_asynch(query = query, 
                 destinationTableId = "GA_MCF", 
                 writeDisposition = "WRITE_TRUNCATE", 
                 useLegacySql = FALSE)

job <- bqr_extract_data(projectId = "YOUR-PROJECT-ID",                # Write your own ID
                 datasetId = "YOUR_DATASET_ID",                       # Write your own ID
                 tableId = "GA_MCF", 
                 cloudStorageBucket = "YOUR_BUCKET_ID",               # Write your own ID
                 filename = "GA_MCF.csv", 
                 fieldDelimiter = ",",
                 destinationFormat = "CSV")

bqr_wait_for_job(job)

# Now that the data has been stored you can download it to your project workspace, just make sure to download it as a csv file
gcs_get_object("GA_MCF.csv", bucket = "YOUR_BUCKET_ID", saveToDisk = "GA_MCF.csv", overwrite = TRUE)    # Write your own ID
mcf <- read.csv("GA_MCF.csv")

# Replace all factors to characters
mcf$fullVisitorId <- as.character(mcf$fullVisitorId)
mcf$transactionId <- as.character(mcf$transactionId)
mcf$channel <- as.character(mcf$channel)
mcf$date <- as.Date(as.character(mcf$date), '%Y%m%d')

# Remove duplicated transactions
mcf <-mcf[order(mcf$fullVisitorId, mcf$visitStartTime),]
mcf <- subset(mcf, !(duplicated(transactionId)) | is.na(transactionId))

# Remove sessions that don't lead to a conversion
keys <- mcf[which(!(is.na(mcf$transactionId))),c('fullVisitorId','transactionId','visitStartTime')]
getLastTransaction <- function(keys){
  df <- group_by(keys, fullVisitorId) %>%
    summarise(lastTransaction = max(visitStartTime)) %>%
    as.data.frame()
  
  keys <- merge(keys, df, by = "fullVisitorId", all.x = TRUE)
  return (keys[-3])
}
keys <- getLastTransaction(keys)

mcf <- merge(mcf, keys, by = "fullVisitorId", all.x = TRUE)
mcf <- subset(mcf, visitStartTime <= lastTransaction)

# Now sort the data frame in ascending order by fullVisitorId and visitStartTime
mcf <- mcf[order(mcf$fullVisitorId, mcf$visitStartTime),]

# Since the data is sorted correctly you can now fill in all NAs in transactionId with the correct one.
# This will help grouping the data correctly in the next step
mcf <- mcf %>% fill(transactionId, .direction = "up")

# Group the data and apply all the relevant columns
mcf <- group_by(mcf, fullVisitorId, transactionId) %>%
          summarise(visitStartTime = min(visitStartTime),
                    firstTouchpointDate = min(date),
                    lastTouchpointDate = max(date),
                    multiChannelFunnel = paste(channel, collapse = ", "),
                    acqusitionChannel = min(channel),
                    conversionChannel = max(channel)) %>%
          as.data.frame()

# Save the data frame as a csv file in your project directory
write.csv(mcf, "GA_MCF_Calculations.csv", row.names = FALSE)
