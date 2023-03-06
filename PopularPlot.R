library(tidyverse)

# Load dataset
data <- read_delim("./Checkouts_by_Title.csv")

# Find 5 most popular types of media
data %>% group_by(MaterialType) %>%
  summarize(totalCheckouts = sum(Checkouts)) %>%
  arrange(desc(totalCheckouts))

# Filter to only include entries where material type is one of the five most popular types.
popular5 <- data %>% filter(MaterialType %in% c("BOOK", "VIDEODISC", "EBOOK", "SOUNDDISC", "AUDIOBOOK"), CheckoutYear != 2023)

# Group popular5 by CheckoutYear, and MaterialType. Get total checkouts for each group and arrange in descending order.
types_by_year <- popular5 %>% group_by(CheckoutYear, MaterialType) %>% 
  summarize(totalCheckouts = sum(Checkouts)) %>%
  arrange(desc(totalCheckouts))

# Plot types_by_year as a scatterplot with a line connecting the points.
ggplot(data=types_by_year, aes(x=CheckoutYear, y=totalCheckouts)) +
  geom_point(aes(color=MaterialType)) +
  geom_line(aes(color=MaterialType)) +
  labs(x = "Year", y = "Total Checkouts", title = "Scatterplot of 5 most popular types of media")