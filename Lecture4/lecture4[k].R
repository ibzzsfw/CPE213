library(tidyverse)
library(zoo)

korea <- read.csv("Korea Income and Welfare.csv")

korea %>%
  distinct() %>%
  filter(year==2018 & !is.na(occupation))  %>%
  mutate(occupation = substr(as.character(occupation), 1,2)) %>%
  group_by(occupation) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=occupation, y=n, fill=occupation)) +
  geom_col() #T
  
korea %>% 
  distinct() %>%
  select(year_born, year, region) %>%
  filter(year==2018) %>%
  mutate(age = year-year_born) %>%
  ggplot(aes(age)) +
  geom_histogram(binwidth=5, colour="black", fill="white") + 
  geom_density(alpha=.2, fill="#FF6666") +
  facet_grid(region ~ .)
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

korea %>% 
  group_by(occupation) %>%
  filter(!is.na(occupation)) %>%
  mutate(occupation = substr(as.character(occupation), 1,2)) %>%
  group_by(occupation) %>%
  ggplot(aes(occupation)) +
  geom_bar()
  