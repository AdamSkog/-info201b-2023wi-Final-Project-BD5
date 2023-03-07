
library(tidyverse)

# Load dataset
cbt <- read_delim("Checkouts_by_Title.csv")

# Usage class changing over time
cbt %>% 
  filter(CheckoutYear != 2023) %>% 
  mutate(time = CheckoutYear + CheckoutMonth/12) %>% 
  group_by(CheckoutYear, UsageClass) %>% 
  summarize(checkoutsum = sum(Checkouts)) %>%
  ggplot(aes(CheckoutYear, checkoutsum, col = UsageClass)) + geom_point() + geom_line() +
  geom_smooth(method = lm, se = F) +
  labs(x = "Time", y = "Number of Checkouts", col = "Type")
