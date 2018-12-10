#install.packages("gtrendsR")
library(gtrendsR)


gTrendOverTime <- function(keyword, time = "today 12-m", channel = "web", country = "SE"){
  if(length(keyword)>5){
    loops <- as.integer(length(keyword)/5)
    df <- NULL
    j <- 1
    
    for(i in 1:(loops)){
      list <- gtrends(keyword = keyword[j:(j+4)],
                      channel = cahnnel,
                      time = time,
                      geo = country) 
      df <- rbind(df, list[[1]])
      j <- j+5
    }
    if (length(keyword)%%5 != 0){

      list <- gtrends(keyword = keyword[j:(length(keyword))],
                      channel = channel,
                      time = time,
                      geo = country) 
      df <- rbind(df, list[[1]])
    }
    
  } else {
    list <- gtrends(keyword = keyword,
                    channel = channel,
                    time = time,
                    geo = country) 
    df <- list[[1]]
  }
  
  return (df)
}
