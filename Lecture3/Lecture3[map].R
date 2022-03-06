#> ----------------option#1------------------

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
# + coord_cartesian(xlim = c(-80, -70), ylim = c(40, 45))

#> ----------------option#2------------------

superstore <- read.csv("superstore.csv")

library(tidyverse)

superstore_simp <- superstore %>%
  group_by(State) %>%
  summarise(sumP = sum(Profit))

qa <- quantile(superstore_simp$sumP, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
qa

superstore_simp$Profit_q <- cut(superstore_simp$sumP, qa,
                                labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                                include.lowest = TRUE)
superstore_simp

pal <- colorRampPalette(c("#559999", "grey80", "#BB650B"))(5)
pal

states_map <- map_data("state")

ggplot(superstore_simp, aes(map_id = tolower(State), fill = Profit_q)) +
  geom_map(map = states_map, colour = "black") +
  scale_fill_manual(values = pal) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic") +
  labs(fill = "Profit\nPercentile") 
#+ coord_cartesian(xlim = c(-80, -70), ylim = c(40, 45))
