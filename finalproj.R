### Final Project: Group BD5 ###
## Variable cbt: checkouts by title (dataframe)

library(tidyverse)

cbt <- read_csv("Checkouts_by_Title.csv")


#displaying some of the data to check what to reformat
cbt %>% 
  head(10)

#replacing extra characters and extra letters with nothing
 cbt$PublicationYear <- gsub("[[:punct:]]","", as.character(cbt$PublicationYear))  #gets rid of the puncuation in the PublicationYear column
 cbt$PublicationYear <- gsub("[^0-9]", " ", as.character(cbt$PublicationYear))  #keeps the space between years so they aren't squished together
 
 cbt$PublicationYear <- trimws(cbt$PublicationYear, which = c("left"))  #gets rid of leading spaces
 
#WIP extracting years and getting rid of any extra years
# cbt <- cbt %>% 
#   mutate(PublicationYear = str_sub(PublicationYear, 1, 4))  
 
#checks to see if there are any out of place years
cbt %>% 
  filter(!is.na(PublicationYear)) %>% 
  summarise(latest = max(PublicationYear), oldest = min(PublicationYear))

#checks to see if years look right or not
cbt %>% 
  select(PublicationYear) %>% 
  head()
  
