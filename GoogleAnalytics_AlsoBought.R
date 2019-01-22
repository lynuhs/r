# Created by Linus Larsson
# 2019-01-22
# https://lynuhs.com

# MAKE SURE TO INSTALL NECESSARY PACKAGES BEFORE YOU RUN THE SCRIPT!
#install.packages("googleAnalyticsR")
#install.packages("googleAuthR")
#install.packages("dplyr")
library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)

ga_auth()

# This function will create a data frame containing all unique product SKUs combined with a paired product SKU 
# that was bought in the same purchase together with a column for transactionID. This makes it possible to calculate
# valuable metrics from the table. 
alsoBoughtTable <- function(id, start, end){
  ga <- google_analytics_3(id = id,
                           start = start,
                           end = end,
                           dimensions = c("date","transactionId","productSku"), 
                           metrics = c("itemQuantity"),
                           samplingLevel = "WALK",
                           max_results = 999999999)
  
  
  ga <- ga[1:3]
  ga <- subset(ga, !(duplicated(ga[2:3])))
  
  cross <- matrix(nrow=0, ncol=4)
  colnames(cross) <- c("date","productSku","alsoBought","transactionId")
  
  dates <- unique(ga$date)
  
  for(d in 1:(length(dates))){
    products <- unique(ga[which(ga$date == dates[d]),'productSku'])
    
    cr <- matrix(nrow=0, ncol=3)
    colnames(cr) <- c("productSku","alsoBought","transactionId")
    
    
    for (i in 1:(length(products))){
      receipts <- ga[which(ga$productSku == products[i] & ga$date == dates[d]),'transactionId']
      
      bp <- subset(ga, transactionId %in% receipts & !(productSku == products[i]))
      
      if (nrow(bp) > 0){
        bp <- data.frame(productSku = products[i],
                         alsoBought = bp$productSku,
                         transactionId = bp$transactionId)
        cr <- rbind(cr, bp)
      } 
      
      
      if(i == 1 | i%%10 == 0 | i == length(products)){
        cat("\014")
        print(
          paste0(
            d, " of ", length(dates)," dates running: ",round(i*100/(length(products)),1), "% computed"
          )
        ) 
      }
    }
    
    cross <- rbind(cross, cbind(data.frame(date = dates[d]),cr))
  }
  cross$productSku <- as.character(cross$productSku)
  cross$alsoBought <- as.character(cross$alsoBought)
  return (cross)
}


# This function calculates the unique purchases and the share of all receipts containing a specific product
# that ALSO contained the paired product in the table.
calculateReceiptShare <- function(productDf){
  share <- group_by(productDf, productSku, alsoBought) %>%
    summarise(uniquePurchases = n_distinct(transactionId)) %>%
    as.data.frame()
  
  receipts <- group_by(productDf, productSku) %>%
    summarise(allReceipts = n_distinct(transactionId))
  
  share <- merge(share,receipts, by = "productSku", all.x = TRUE)
  
  share$shareOfAllReceipts <- round(share$uniquePurchases / share$allReceipts, 2)
  
  return (share)
}

# This function will gather the table data for all product SKUs and then calculate the metrics
alsoBought <- function(id, start, end){
  ga <- alsoBoughtTable(id, start, end)
  
  ga <- calculateReceiptShare(ga)
  ga <- ga[order(-ga$uniquePurchases),]
  return (ga)
}

#######################################################################################
#  CONSOLE FUNCTIONS                                                                  
#######################################################################################

#Type in your GA View ID
ga_id <- XXXXXXX    

# Collect data from last 30 days in a format that can be used as a table
# that you can use at a data source in BI tools
df <- alsoBoughtTable(ga_id, Sys.Date()-30, Sys.Date()-1)

# If you only want to collect the statistics you can run the following command:
df <- alsoBought(ga_id, Sys.Date()-30, Sys.Date()-1)
