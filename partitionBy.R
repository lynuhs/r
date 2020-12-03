partitionBy <- function(x, calcColumn, partitionBy, newColumn, FUN){
  arguments <- as.list(match.call())
  if(length(arguments) < 5){
    stop("Too few arguments!")
  }
  
  tryCatch({
    eval(arguments$partitionBy, x)
  }, error = function(err){
    err$message <- paste0("No column called \"", arguments$partitionBy, "\" in ", arguments$x)
    err$call = "partitionBy"
    stop(err)
  })
  
  col <- deparse(substitute(calcColumn))

  partitionBy <- strsplit(gsub("c\\(|\\)| ", "", deparse(substitute(partitionBy))), ",")[[1]]
  pdf <- x %>% 
    group_by(x[,partitionBy]) %>%
    summarise(
     temp = FUN(get(col))
    ) %>%
    as.data.frame()
  
  colnames(pdf) <- c(partitionBy, deparse(substitute(newColumn)))
  
  x <- merge(x, pdf, partitionBy, all.x=TRUE)
  
  return(x)
}
