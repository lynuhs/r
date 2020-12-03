partitionByOver <- function(x, calcColumn, partitionBy, newColumn, FUN){
  originalColOrder <- colnames(x)
  x$originalRowOrder <- 1:nrow(x)
  rowNames <- rownames(x)
  
  partitionBy <- deparse(substitute(partitionBy))
  partitionBy <- strsplit(gsub("c\\(|\\)| ", "", partitionBy), ",")[[1]]
  
  newColumn <- deparse(substitute(newColumn))
  
  if(!all(partitionBy %in% colnames(x))){
    stop("One or more of the columns specified in partitionBy does not exist in the data frame", call. = FALSE)
  }

  
  col <- deparse(substitute(calcColumn))

  options(dplyr.summarise.inform = FALSE)
  pdf <- x %>% 
    group_by(x[,partitionBy]) %>%
    summarise(
     temp = FUN(get(col))
    ) %>%
    as.data.frame()
  options(dplyr.summarise.inform = TRUE)
  
  colnames(pdf) <- c(partitionBy, newColumn)

  
  x <- merge(x, pdf, partitionBy, all.x=TRUE)
  x <- x[order(x$originalRowOrder), c(originalColOrder, newColumn)]
  rownames(x) <- rowNames
  
  return(x)
}
