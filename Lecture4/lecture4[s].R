library(tidyverse)

tweet <- read.csv("tweets_v8.csv")

tweet <- tweet %>%
  select(user_name, user_location, date, source) %>%
  distinct() %>%
  rename(platform = source) %>%
  mutate(platform = tolower(gsub(" ", "", platform)))

tweet %>%
  group_by(platform) %>%
  summarise(tweets = n()) %>%
  mutate(platform = ifelse(tweets < 1000,'other', platform)) %>%
  group_by(platform) %>%
  summarise(tweets = sum(tweets)) %>%
  ggplot(aes(x=reorder(platform, -tweets), y=tweets, fill=platform)) +
  geom_col() +
  scale_x_discrete(limits = c("twitterforiphone", "twitterforandroid", "twitterwebapp", "twitterforipad", "tweetdeck", "other")) +
  labs(x = "Platform", y = "Tweets", title ="Squid Game Netflix Tweet platform")
                        
tweet %>%
  mutate(date = substr(date, 6,13)) %>%
  group_by(date)
  summarise(times = n(date))
  ggplot(aes(date)) +
  geom_line()



