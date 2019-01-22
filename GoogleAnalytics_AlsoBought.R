# Created by Linus Larsson
# 2019-01-22
# https://lynuhs.com

#install.packages("googleAnalyticsR")
#install.packages("googleAuthR")
#install.packages("dplyr")
library(googleAnalyticsR)
library(googleAuthR)
library(dplyr)

ga_auth()

alsoBought <- function(id, start, end){
  ga <- google_analytics_3(id = id,
              start = start,
              end = end,
              dimensions = c("transactionId","productSku"), 
              metrics = c("itemQuantity"),
              samplingLevel = "WALK",
              max_results = 999999999)

  ga <- subset(ga, !(duplicated(ga)))
  ga <- ga[1:2]
  
  products <- unique(ga$productSku)
  
  cross <- matrix(nrow=0, ncol=4)
  colnames(cross) <- c("productSku","alsoBought","uniquePurchases","shareOfAllReceipts")
  
  
  for (i in 1:(length(products))){
    receipts <- ga[which(ga$productSku == products[i]),'transactionId']
    
    bp <- subset(ga, transactionId %in% receipts & !(productSku == products[i]))
    
    if (nrow(bp) > 0){
      bp <- group_by(bp, productSku) %>%
        summarise(uniquePurchases = n_distinct(transactionId)) %>%
        as.data.frame()
      
      bp <- data.frame(productSku = products[i],
                       alsoBought = bp$productSku,
                       uniquePurchases = bp$uniquePurchases,
                       shareOfAllReceipts = round(bp$uniquePurchases / length(receipts),2))
      
      cross <- rbind(cross, bp)
    }
    cat("\014")
    print(
      paste0(
        round(i*100/(length(products)),1), "% computed"
      )
    )
  }
  
  
  return (cross)
}

#Type in your GA View ID
ga_id <- XXXXXXX    

# Collect data from last 30 days
df <- alsoBought(ga_id, Sys.Date()-30, Sys.Date()-1)
