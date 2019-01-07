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
gaGetData <- function(id, start, end, dimensions, metrics, dimensionFilter="not set", segment = "All Users"){
  if(!(is.list(dimensionFilter))){
    df <- dim_filter(dimensions[1], "REGEXP",".*")
    fc <- filter_clause_ga4(list(df), operator = "OR")
  } else {
    fc <- dimensionFilter
  }
  
  if(segment == "All Users"){
    seg <- segment_ga4("All Users", "gaid::-1")
  } else {
    seg <- segment_ga4("All Users", segment)
  }
  
  df <- google_analytics(id,
                         date_range = c(start,end),
                         metrics = metrics,
                         dimensions = dimensions,
                         dim_filters = fc,
                         segments = seg,
                         max = -1,
                         anti_sample = TRUE)
  
  if(is.null(df)){
    df <- data.frame(matrix(ncol = (length(dimensions)+(length(metrics))), nrow = 1))
    colnames(df) <- c(dimensions,metrics)
    df <- NA
  }
  
  return (df)
}

# A function that will create a data frame containing unique pageviews for all pages in the chosen vector (or all if empty) 
# for both all users and only for those who have made a transaction and then calculates the conversion rate based on viewed page
gaCalculatePageCR <- function(id, start, end, pages = "ALL"){
  # COLLECT ALL PAGEVIEWS
  if (pages == "ALL"){
    pages <- ".*"
  } else {
    pages <- paste0("^", paste(pages, collapse = "$|^"), "$")
  }
  
  dimf <- dim_filter("pagePath","REGEXP",pages)
  fc <- filter_clause_ga4(list(dimf), operator = "AND")
  
  all <- gaGetData(id, start, end, c("date","pagePath"), c("uniquePageviews"), fc)
  con <- gaGetData(id, start, end, c("date","pagePath"), c("uniquePageviews"), fc, "gaid::-10")
  
  all <- all[,colnames(all) != "segment"]
  colnames(all) <- c("date","pagePath","uniquePageviews_all")
  con <- con[,colnames(con) != "segment"]
  colnames(con) <- c("date","pagePath","uniquePageviews_customers")
  
  data <- merge(all, con, by = c("date","pagePath"), all.x = TRUE)
  data$conversionRate <- round(data$uniquePageviews_customers / data$uniquePageviews_all, 4)
  
  return (data)
}
