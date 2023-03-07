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
 
 #filtering out out of place numbers from UpdatedYear
cbt <- cbt %>% 
  mutate(UpdatedYear = replace(UpdatedYear, !UpdatedYear <= 2024 | !UpdatedYear >= 1863, ""), 
         secYear = replace(secYear, !secYear <= 2024, ""))

#Replaces an empty spaces in firstYear with secYear data
cbt <- cbt %>%
 mutate(UpdatedYear = ifelse(UpdatedYear == "",secYear, UpdatedYear ))

#Changing UpdatedYear to be numeric instead of a character
cbt$UpdatedYear <- as.numeric(as.character(cbt$UpdatedYear))

#Gets rid of PublicationYear and secYear to leave UpdatedYear by itself
cbt <-  cbt[,-12]
cbt <- cbt[,-13]


#finding the years with the most amount of media releases
cbt %>% 
  group_by(UpdatedYear) %>% 
  filter(!is.na(UpdatedYear)) %>% 
  summarise(media_count = length(Title)) %>% 
  filter(rank(desc(media_count)) < 6) %>% 
  arrange(desc(media_count))

#finding publication year with most releases of media
cbt %>% 
  group_by(UpdatedYear) %>% 
  filter(!is.na(UpdatedYear)) %>%
  summarise(media_count = length(Title)) %>% 
  ggplot(aes(x = UpdatedYear, y = media_count, fill = factor(UpdatedYear)))+
  geom_col(width = .8)+
  scale_color_brewer(palette = "Set2")+
  geom_line(group = 1)+
  scale_x_continuous(breaks = seq(0, 2023, 6))+
  theme(axis.text = element_text(size = 10, hjust = 1, angle = 45), legend.key.size = unit(0.3, "line"))+
  labs(title = "Amount of Media Released Yearly",x = "Year",y = "Amount of Media Released",fill = "Year")




  
  

  

   

  
