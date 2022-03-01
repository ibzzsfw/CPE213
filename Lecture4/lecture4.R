library(tidyverse)

netflix <- read.csv('netflix.csv')

netflix <- netflix %>%
  distinct() %>%
  drop_na() %>%
  filter(duration > 0)

netflix %>%
  group_by(title) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

prop <- 0.8

netflix %>% 
  filter(title == 'Black Mirror: Bandersnatch') %>%
  select(duration) %>%
  arrange(desc(duration)) %>% 
  slice_min(duration, prop = prop) %>%
  ggplot(aes(duration)) +
  geom_density(alpha=.2, fill="#FF6666")

max <- 13964
  
netflix %>% 
  filter(title == 'Black Mirror: Bandersnatch') %>%
  select(duration) %>%
  slice_min(duration, prop = prop) %>%
  ggplot(aes(duration)) +
  geom_histogram(binwidth=(max/(1 + 3.322*log10(350382*prop))), colour="black", fill="white") +
    labs(
    title = 'Black Mirror: Bandersnatch',
    x = 'Duration between this click and the user\'s next click (second)', y = 'Count'
  )

s <- strsplit(netflix$genres, split = ", ")
movie <- data.frame(movie_id = rep(netflix$movie_id, sapply(s, length)), genres = unlist(s))

movie %>%
  group_by(genres) %>%
  filter(genres != 'NOT AVAILABLE') %>%
  summarise(n = n()) %>%
  ggplot(aes(y=reorder(genres, n), x=n, fill=genres)) +
  geom_col() +
  labs(
    title = 'Netflix audience behaviour - UK movies by Genre',
    x = 'Clicks', y = 'Genre'
  )

