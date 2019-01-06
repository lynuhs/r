# WEB SCRAPING
#install.packages('rvest')
#install.packages('lubridate')
library(rvest)
library(lubridate)
Sys.setlocale(category = "LC_TIME", locale = "English_US")

getHolidays <- function (years) {
  url <- "https://www.kalender.se/helgdagar/"
  holidays <- NULL
  holidayNames <- NULL
  holidayTable <- NULL
  for (i in 1:length(years)){
    h <- read_html(paste0(url,years[i]))
    
    h <- h %>%
      html_node(".table-striped") %>%
      html_table()
    
    holidayTable <- rbind(holidayTable,as.data.frame(h))
  }
  
  holidayTable <- holidayTable[1:2]
  colnames(holidayTable) <- c('date','holiday')
  holidayTable$date <- as.Date(holidayTable$date)
  holidayTable$is_holiday <- TRUE
  holidayTable$is_day_off <- TRUE
  
  return(holidayTable)
}


getPayDays <- function(date, day_off){
  df <- data.frame(date=date, day_off=day_off)
  df <- subset(df, format(date,'%d') %in% c('20','21','22','23','24','25'))
  
  rows <- seq(1,nrow(df), by=6)
  paydays <- NULL
  
  for(i in 1:(length(rows))){
    if (all(df[(rows[i]+1):(rows[i]+5),'day_off'])){
      paydays <- rbind(paydays, df[rows[i],])
    } else if (all(df[(rows[i]+2):(rows[i]+5),'day_off'])){
      paydays <- rbind(paydays, df[(rows[i]+1),])
    } else if (all(df[(rows[i]+3):(rows[i]+5),'day_off'])){
      paydays <- rbind(paydays, df[(rows[i]+2),])
    } else if (all(df[(rows[i]+4):(rows[i]+5),'day_off'])){
      paydays <- rbind(paydays, df[(rows[i]+3),])
    } else if (df[(rows[i]+5),'day_off']){
      paydays <- rbind(paydays, df[(rows[i]+4),])
    } else {
      paydays <- rbind(paydays, df[(rows[i]+5),])
    }
  }
  
  paydays$is_payday <- TRUE
  
  return (paydays[-2])
}


createCalendar <- function(startDate = "2018-01-01", endDate = "2030-12-31"){
  calendar <- data.frame(date = seq(as.Date(startDate),as.Date(endDate),'day'))
  calendar$year <- format(calendar$date, '%Y')
  calendar$month_name <- format(calendar$date, '%B')
  calendar$month_name_short <- format(calendar$date, '%b')
  calendar$month_no <- format(calendar$date, '%m')
  calendar$month <- paste0(calendar$month_no, ' ', calendar$month_name_short)
  calendar$month_period <- paste0(calendar$year, calendar$month_no)
  calendar$day <- format(calendar$date, '%d')
  calendar$week_no_sun <- lubridate::isoweek(calendar$date+1)
  calendar$week_no_sun <- ifelse(calendar$week_no_sun < 10, paste0('0', calendar$week_no_sun), as.character(calendar$week_no_sun))
  calendar$week_no_mon <- lubridate::isoweek(calendar$date)
  calendar$week_no_mon <- ifelse(calendar$week_no_mon < 10, paste0('0', calendar$week_no_mon), as.character(calendar$week_no_mon))
  calendar$weekday_long <- format(calendar$date, '%A')
  calendar$weekday_short <- format(calendar$date, '%a')
  calendar$weekday_sun <- paste0(as.integer(format(calendar$date, '%w'))+1, ' ', calendar$weekday_short)
  calendar$weekday_mon <- paste0(as.integer(format(calendar$date-1, '%w'))+1, ' ', calendar$weekday_short)
  
  
  holidayTable <- getHolidays(unique(calendar$year))
  calendar <- merge(calendar, holidayTable, by = 'date', all.x = TRUE)
  
  calendar[which(calendar$weekday_short %in% c('Sat', 'Sun') | (calendar$month_no == '12' & calendar$day == '24')), 'is_day_off'] <- TRUE
  calendar[which(is.na(calendar$is_day_off)),'is_day_off'] <- FALSE
  calendar[which(is.na(calendar$is_holiday)),'is_holiday'] <- FALSE
  
  paydays <- getPayDays(calendar$date,calendar$is_day_off)
  paydays$salary_period <- as.character(as.integer(paste0(format(paydays$date, '%Y'),format(paydays$date, '%m')))+1)
  paydays[which(substr(paydays$salary_period,5,6) == '13'),'salary_period'] <- 
    as.character(as.integer(paydays[which(substr(paydays$salary_period,5,6) == '13'),'salary_period'])+88)
  
  calendar <- merge(calendar, paydays, by = 'date', all.x = TRUE)
  calendar[which(is.na(calendar$is_payday)),'is_payday'] <- FALSE
  
  sal <- calendar[1,'month_period']
  
  for(n in 1:nrow(calendar)){
    if(is.na(calendar[n,'salary_period'])){
      calendar[n,'salary_period'] <- sal
    } else {
      sal <- calendar[n,'salary_period']
    }
  }

  return (calendar)
}

calendar <- createCalendar()
