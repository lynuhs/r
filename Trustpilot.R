# Created by Linus Larsson
# 2019-02-01
# https://lynuhs.com

library(rvest)

trim <- function(str){
  gsub("^\\s+|\\s+$", "", str)
} 

factorToCharacter <- function(df){
  for (i in 1:ncol(df)){
    if(is.factor(df[,i])){
      df[,i] <- as.character(df[,i])
    }
  }
  return(df)
}

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
    #theme(axis.text.x = element_text(angle = 30)) +
    theme(plot.title = element_text(margin = unit(c(0.1,0.1,0.1,0.1),"cm"))) +
    theme(plot.subtitle = element_text(margin = unit(c(0,0,1,0),"cm")))
}

trustpilot <- function(domain){
  url <- paste0("https://www.trustpilot.com/review/", domain, "?languages=all")
  
  
  totalReviews <- read_html(url) %>%
    html_node(".headline__review-count") %>%
    html_text()
  totalReviews <- as.integer(gsub(",","", totalReviews))
  
  reviews <- NULL
  cat("\014")
  cat(paste0("The script will run on ", ceiling(totalReviews/20), " pages!\n"))
  Sys.sleep(2)
  
  for (i in 1:ceiling(totalReviews/20)){
    page <- read_html(paste0(
      url,
      "&page=",
      i
    ))
    review_card <- page %>%
      html_nodes(".review-card")
    
    name <- review_card %>%
      html_nodes(".consumer-information__name") %>%
      html_text() %>%
      trim()
    
    image <- review_card %>%
      html_node(".consumer-information__picture") %>%
      html_attr("consumer-image-url")
    image[which(regexpr("https", image) < 0)] <- paste0("https:", image[which(regexpr("https", image) < 0)])
    
    reviewCount <- review_card %>%
      html_nodes(".consumer-information__review-count") %>%
      html_text() %>%
      trim()
    reviewCount <- as.integer(gsub(" review","",gsub(" reviews","",as.character(reviewCount))))
    
    rating <- review_card %>%
      html_node(".star-rating") %>%
      html_attr("class")
    rating <- as.integer(gsub("[^[:digit:].,]", "",rating))
    
    published <- html_text(review_card)
    published <- as.Date(substr(published,
                                unlist(gregexpr("publishedDate.*upda", published, perl=TRUE))+16,
                                unlist(gregexpr("publishedDate.*upda", published, perl=TRUE))+25))
    
    respondDate <- review_card %>%
      html_node(".ndate") %>%
      html_attr("date") %>%
      substr(1,10) %>%
      as.Date()
    
    verified <- review_card %>%
      html_node(".review-content-header__review-verified") %>%
      html_text()
    verified <- regexpr("isVerified", verified) > 0
    
    
    title <- review_card %>%
      html_nodes(".review-content__title") %>%
      html_text() %>%
      trim()
    
    content <- review_card %>%
      html_nodes(".review-content__text") %>%
      html_text() %>%
      trim()
    
    haveReply <- html_children(review_card) %>%
      html_text()
    haveReply <- unlist(gregexpr("Reply from", test, perl=TRUE)) > 0
    
    reply <- review_card %>%
      html_nodes(".brand-company-reply__content") %>%
      html_text() %>%
      trim()
    
    replies <- NULL
    k <- 1
    for(j in 1:(length(name))){
      if (haveReply[j]){
        replies <- c(replies, reply[k])
        k <- k+1
      } else {
        replies <- c(replies, NA)
      }
    }
    
    
    reviews <- rbind(reviews, data.frame(
      name = name, 
      image = image, 
      reviewCount = reviewCount, 
      rating = rating,
      publishedDate = published,
      respondDate = respondDate,
      verifiedOrder = verified,
      title = title, 
      content = content, 
      reply = replies
    ))
    print(paste0(url, "&page=", i, " has been scraped"))
  }
  
  reviews <- factorToCharacter(reviews)
  
  return(reviews)
}

hak <- trustpilot("huntakiller.com")

ggplot(subset(hak, rating > 0), aes(x=as.factor(rating), fill=verifiedOrder)) + 
  geom_bar(stat="count") + 
  xlab("Rating") +
  labs(fill = "Verified Order") + 
  scale_fill_manual(values = c("#051d30","#3a6587")) +
  lynuhs_theme()
