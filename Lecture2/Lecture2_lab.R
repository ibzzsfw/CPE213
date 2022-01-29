flights <- read.csv("flights.csv")
airlines <- read.csv("airlines.csv")
library(dplyr)
flights %>%
  select(carrier, dep_delay) %>%
  filter(!is.na(dep_delay)) %>%
  group_by(carrier) %>%
  summarise(mean_delay = mean(dep_delay)) %>%
  arrange(desc(mean_delay)) %>%
  left_join(airlines, flights, by = "carrier") %>%
  select(-X) %>%
  format.data.frame(digits = 7)
