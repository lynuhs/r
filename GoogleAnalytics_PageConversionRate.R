# Created by Linus Larsson
# 2019-01-07
# https://lynuhs.com

#install.packages("googleAuthR")
#install.packages("googleAnalyticsR")
library(googleAnalyticsR)
library(googleAuthR)

# Create connection to Google Analytics
ga_auth()

# Get all views and select the id you want to use
account_list <- ga_account_list()
ga_id <- account_list[which(account_list$webPropertyName=="PROPERTY NAME" & account_list$viewName == "VIEW NAME"),'viewId']

# If you know the view id you could just type it in instead of the above code
#ga_id <-9387XXXX

# Select which dates the report should include
start <- "2018-11-11"
end <- "2018-12-22"

# A function to collect data from GA and exporting as a data frame
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

# A function that will create a data frame containing unique pageviews for all pages in the chosen vector (or all if empty) 
# for both all users and only for those who have made a transaction and then calculates the conversion rate based on viewed page
gaCalculatePageCR <- function(id, start, end, pages = "ALL"){
  # COLLECT ALL PAGEVIEWS
  if (pages[1] == "ALL"){
    pages <- "ga:pagePath=~.*"
  } else {
    pages <- paste0("ga:pagePath=~^", paste(pages, collapse = "$|^"), "$")
  }
  
  
  all <- gaGetData(id, start, end, c("date","pagePath"), c("uniquePageviews"), pages)
  con <- gaGetData(id, start, end, c("date","pagePath"), c("uniquePageviews"), pages, "gaid::-10")
  
  colnames(all) <- c("date","pagePath","uniquePageviews_all")
  colnames(con) <- c("date","pagePath","uniquePageviews_customers")
  
  data <- merge(all, con, by = c("date","pagePath"), all.x = TRUE)
  data$conversionRate <- round(data$uniquePageviews_customers / data$uniquePageviews_all, 4)
  
  return (data)
}
