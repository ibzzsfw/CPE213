superstore <- read.csv("superstore.csv")
map <- map_data("state")

library(tidyverse)

superstore <- superstore %>%
  group_by(State) %>%
  summarise(sumP = sum(Profit))

data.frame(Profit = superstore$sumP, State = tolower(superstore$State)) %>%
  ggplot(aes(fill = Profit)) + 
  geom_map(aes(map_id = State), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  scale_fill_gradient(
    low = "#b2b7f7", 
    high = "#0b19db", 
  )