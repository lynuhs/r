# Created by Linus Larsson
# 2018-12-10
# https://lynuhs.com

# A simple function to put in the start of your script which will ony run the first time you run the script.
# The if statement looks for a variable that does not exist but after the code has run it will define the variable
# and therefore not run again. 

if(exists("hasRun") == FALSE){
  # Examples of what you only want to run once
  source("functions.R") 
  library(ggplot2)
  
  hasRun <- TRUE 
}
