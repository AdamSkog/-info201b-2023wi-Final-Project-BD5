### Final Project: Group BD5 ###
## Variable cbt: checkouts by title (dataframe)

library(tidyverse)
library(tidyr)

cbt <- read_csv("Checkouts_by_Title.csv")


#displaying some of the data to check what to reformat
cbt %>% 
  head(10)

 cbt$PublicationYear <- trimws(cbt$PublicationYear, which = c("left"))  #gets rid of leading spaces
 
#Extracting years and getting rid of any extra years
 cbt <- cbt %>% 
   extract(PublicationYear, into = "secYear", regex = "\\d{4}.*(\\d{4})", remove = FALSE, convert = TRUE) %>%
   extract(PublicationYear,into = "firstYear", regex ="(\\d{4})", remove = FALSE, convert = TRUE)  
 
cbt <- cbt %>% 
  filter(!is.na(secYear)) %>% 
  mutate(firstYear = replace(firstYear, !firstYear <= 2023 | !firstYear >= 1863, ""), 
         secYear = replace(secYear, !secYear <= 2023, ""))

#Replaces an empty spaces in firstYear with secYear data
cbt <- cbt %>%
 mutate(firstYear = ifelse(firstYear == "",secYear, firstYear ))


   
 
#checks to see if there are any out of place years
cbt %>% 
  filter(!is.na(PublicationYear)) %>% 
  summarise(latest = max(PublicationYear), oldest = min(PublicationYear))

#checks to see if years look right or not
cbt %>% 
  select(PublicationYear) %>% 
  head()

cbt %>% 
  summarise(min(firstYear), max(firstYear), min(secYear), max(secYear))







  
