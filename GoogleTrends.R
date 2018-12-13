#install.packages("gtrendsR")
library(gtrendsR)
library(ggplot2)
library(dplyr)


# A function that returns all month names with three letters in the locale language in upper case
# --------------------------------------------------------------------------------------------------------------
monthNames <- function(){
  return(toupper(unique(format(seq.Date(as.Date("2018-01-01"), as.Date("2018-12-31"), by = 31), "%b"))))
}


# A function that return a character string with the first letter in upper case
# --------------------------------------------------------------------------------------------------------------
firstUpper <- function(string){
  return(paste0(toupper(substr(string,1,1)),substr(string,2,nchar(string))))
}


# My own customized theme
# --------------------------------------------------------------------------------------------------------------
lynuhs_theme <- function(){
  bg <- "#b5f5ff"
  lineCol <- "#7ec8d3"
  
  theme_bw() +
    theme(plot.margin = unit(c(.5, .5, .5, .5),"cm")) +
    theme(plot.background = element_rect(fill = bg)) + 
    theme(panel.background = element_rect(fill = bg)) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.border = element_blank()) +
    theme(panel.grid = element_line(colour = lineCol, linetype = "dotted")) +
    theme(panel.grid.major.x = element_line(linetype = 0)) +
    theme(strip.background = element_blank()) +
    theme(strip.text = element_text(size = 14, color = "black", face = "bold")) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank()) + 
    theme(axis.text.x = element_text(angle = 30)) +
    theme(plot.title = element_text(margin = unit(c(0.1,0.1,0.1,0.1),"cm"))) +
    theme(plot.subtitle = element_text(margin = unit(c(0,0,1,0),"cm")))
}


# A function that will return a data frame for the chosen keywords. The function will handle more requests than 
# Google Trend's limit for 5 keywords by slitting them into subqueries.
# --------------------------------------------------------------------------------------------------------------
gTrendOverTime <- function(keyword, time = "2010-01-01 2017-12-31", channel = "web", country = "SE"){
  trend_df <- NULL
  for(c in 1:(length(country))){ 
    if(length(keyword)>5){
      loops <- as.integer(length(keyword)/5)
      df <- NULL
      j <- 1
      
      for(i in 1:(loops)){
        list <- gtrends(keyword = keyword[j:(j+4)],
                        gprop = channel,
                        time = time,
                        geo = country[c]) 
        df <- rbind(df, list[[1]])
        j <- j+5
      }
      if (length(keyword)%%5 != 0){
        
        list <- gtrends(keyword = keyword[j:(length(keyword))],
                        gprop = channel,
                        time = time,
                        geo = country[c]) 
        df <- rbind(df, list[[1]])
      }
      
    } else {
      list <- gtrends(keyword = keyword,
                      gprop = channel,
                      time = time,
                      geo = country[c]) 
      df <- list[[1]]
    }
    trend_df <- rbind(trend_df, df) 
  }
  
  # In some cases you will get <1 as a value for hits. Convert that to a 0 instead
  trend_df$hits <- as.numeric(gsub("<1","0",as.character(trend_df$hits)))
  
  # Create a new column by converting the dates to month names with three letters
  trend_df$month <- toupper(format(trend_df$date, "%b"))
  
  trend_df$monthPeriod <- gsub("-","",substr(as.character(trend_df$date),1,7))
  trend_df$year <- format(trend_df$date, "%Y")
  
  return (trend_df)
}


# Group the data frame by a specific time dimension and calculate the mean value for that dimension
# --------------------------------------------------------------------------------------------------------------
trendSummary <- function(groupby = "month", df=df){
  if (groupby == "month"){
    df <- group_by(df, keyword, month, geo, gprop)
  } else if (groupby == "monthPeriod"){
    df <- group_by(df, keyword, monthPeriod, month, geo, gprop)
  } else if (groupby == "year"){
    df <- group_by(df, keyword, year, geo, gprop)
  } else {
    cat("\14")
    stop("Function must contain a valid groupby format (month, monthPeriod)")
  }
  
  df <- summarise(df, hits = as.integer(mean(hits),0))
  return (as.data.frame(df))
}


# Dowload the trend data for specific keywords and plot them out using ggplot2
# --------------------------------------------------------------------------------------------------------------
plotTrend <- function(keyword, groupby = "month", time = "2010-01-01 2017-12-31", channel = "web", country = "SE"){
  if (!(groupby %in% c("month","monthPeriod","year"))){
    stop("Function must contain a valid groupby format (month, monthPeriod or year)")
  }
  
  df <- gTrendOverTime(keyword, time, channel, country)
  df <- trendSummary(groupby, df)
  plotTitle <- ifelse(substr(time,1,4)==substr(time,12,15),substr(time,12,15), paste0(substr(time,1,4)," to ",substr(time,12,15)))
  plotTitle <- paste0(plotTitle," by ", firstUpper(groupby))
  
  x_axis <- switch(groupby, 
                   "month" = monthNames(),
                   "monthPeriod" = unique(df$monthPeriod),
                   "year" = unique(df$year))
  
  interval <- as.integer(length(x_axis)/15)+1
  
  ggplot(df, aes_string(x=groupby, y="hits")) +
    geom_bar(stat='identity', fill = "#126672", color = "black") +
    scale_x_discrete(limits=x_axis, breaks=x_axis[seq(1,length(x_axis), by=interval)]) +
    lynuhs_theme() + 
    labs(x=toupper(groupby), y="INTEREST") +
    guides(fill = FALSE) +
    facet_grid(geo~toupper(keyword)) +
    ggtitle(label = paste0("Trend data from Google Trends | ",plotTitle),
            subtitle = "An interest of 100 represents the highes point of popularity. 50 means half as high popularity.\nValues are presented as means of time period.")
}


# What to use in console
# --------------------------------------------------------------------------------------------------------------

# Collect the Google Trend data in a data frame in order to save it or use for later
trendDataFrame <- gTrendOverTime(keyword = c("Google Tag Manager", "Google Analytics"),
                                 time = "2010-01-01 2017-12-31",
                                 channel = "web",
                                 country = "US")

# Summarise the data frame and calculate the means
summarisedTrendDataFrame <- trendSummary(groupby = "month", df = trendDataFrame)

# Collect Google Trend data and plot it without saving it
plotTrend(keyword = c("Google Tag Manager", "Google Analytics"),
          time = "2010-01-01 2017-12-31",
          channel = "web",
          country = "US",
          groupby = "month")
