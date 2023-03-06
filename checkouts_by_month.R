library(tidyverse)

# Load dataset
cbt <- read_delim("Checkouts_by_Title.csv")

#Which month has the most/least checkouts
count_checkouts_by_month <- cbt %>% group_by(CheckoutMonth) %>%
  summarise(total_checkouts = sum(Checkouts))
count_checkouts_by_month

##print most checkouts month
most_checkouts_month <- 
  count_checkouts_by_month[which.max(count_checkouts_by_month$total_checkouts), "CheckoutMonth"]
cat("Month with the most checkouts:", as.character(most_checkouts_month), "\n")

##print least checkouts month
least_checkouts_month <- 
  count_checkouts_by_month[which.min(count_checkouts_by_month$total_checkouts), "CheckoutMonth"]
cat("Month with the least checkouts:", as.character(least_checkouts_month), "\n")

