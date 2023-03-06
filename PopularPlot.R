library(tidyverse)

data <- read_delim("./Checkouts_by_Title.csv")

data %>% group_by(MaterialType) %>%
  summarize(totalCheckouts = sum(Checkouts)) %>%
  arrange(desc(totalCheckouts))
popular5 <- data %>% filter(MaterialType %in% c("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK"), CheckoutYear != 2023)

types_by_year <- popular5 %>% group_by(CheckoutYear, MaterialType) %>% 
  summarize(totalCheckouts = sum(Checkouts)) %>%
  arrange(desc(totalCheckouts))
ggplot(data=types_by_year, aes(x=CheckoutYear, y=totalCheckouts)) +
  geom_point(aes(color=MaterialType)) +
  geom_line(aes(color=MaterialType)) +
  labs(x = "Year", y = "Total Checkouts", title = "Scatterplot of 5 most popular types of media")