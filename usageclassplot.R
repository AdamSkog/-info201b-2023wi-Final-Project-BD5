
library(tidyverse)

# Load dataset
cbt <- read_delim("Checkouts_by_Title.csv")

# Usage class changing over time
cbt %>% 
  mutate(time = CheckoutYear + CheckoutMonth/12) %>% 
  group_by(time, UsageClass) %>% 
  summarize(checkoutmean = mean(Checkouts)) %>% 
  ggplot() + geom_line(aes(time, checkoutmean, col = UsageClass)) +
  labs(x = "Time", y = "Number of Checkouts", col = "Type")
