### Final Project: Group BD5 ###
## Variable cbt: checkouts by title (dataframe)

library(tidyverse)
library(tidyr)
library(scales)

cbt <- read_csv("Checkouts_by_Title.csv")


#displaying some of the data to check what to reformat
cbt %>% 
  head(10)

 cbt$PublicationYear <- trimws(cbt$PublicationYear, which = c("left"))  #gets rid of leading spaces
 
#Extracting years and getting rid of any extra years
 cbt <- cbt %>% 
   extract(PublicationYear, into = "secYear", regex = "\\d{4}.*(\\d{4})", remove = FALSE, convert = TRUE) %>%
   extract(PublicationYear,into = "UpdatedYear", regex ="(\\d{4})", remove = FALSE, convert = TRUE)  
 
cbt <- cbt %>% 
  filter(!is.na(secYear)) %>% 
  mutate(UpdatedYear = replace(UpdatedYear, !UpdatedYear <= 2023 | !UpdatedYear >= 1863, ""), 
         secYear = replace(secYear, !secYear <= 2023, ""))

#Replaces an empty spaces in firstYear with secYear data
cbt <- cbt %>%
 mutate(UpdatedYear = ifelse(UpdatedYear == "",secYear, UpdatedYear ))

#Gets rid of PublicationYear and secYear to leave UpdatedYear by itself
cbt <-  cbt[,-12]
cbt <- cbt[,-13]
   

  
