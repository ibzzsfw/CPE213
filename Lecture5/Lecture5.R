library(tidyverse)
library(eeptools)
library(ggpubr)
library(corrplot)
library('car')

netflix <- read.csv('netflix.csv')

netflix <- netflix %>%
  distinct() %>%
  drop_na() %>%
  filter(duration > 0)

today <- Sys.Date()

netflix %>%
  mutate(datetime = as.Date(datetime)) %>%
  mutate(release_date = as.Date(release_date)) %>%
  filter(!is.na(release_date)&!is.na(datetime)) %>%
  mutate(age = age_calc(dob = release_date, enddate = today, units = "years")) %>%
  mutate(last_watch = age_calc(dob = datetime, enddate = today, units = "years")) %>%
  mutate(age = age - last_watch) -> netflix

netflix %>%
  group_by(movie_id) %>%
  summarise(age = mean(age)) %>%
  filter(age >= 0) -> a

netflix %>%
  group_by(movie_id) %>%
  summarise(views = n()) -> v

netflix %>%
  group_by(movie_id) %>%
  summarise(duration = mean(duration)) -> d

netflix %>%
  group_by(movie_id) %>%
  summarise(med = median(duration)) -> med

rel <- inner_join(d, v, by='movie_id') %>%
  distinct()
rel <- inner_join(rel, a, by='movie_id') %>%
  distinct()

cor(rel$duration, rel$med, method = 'pearson')

head(rel, 6)
 
ggqqplot(rel$views, ylab = "views") + theme(aspect.ratio=1)
ggqqplot(rel$age, ylab = "age")
ggqqplot(rel$duration, ylab = "duration")

res <- rel %>%
  select(views, age, duration) %>%
  cor()

cor.test(rel$age, rel$duration, method = "pearson")

corrplot(res,type = 'upper', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('PiYG'))

rel %>%
  sample_frac(1) %>%
  ggscatter(x = 'age', y = 'views', size = 1,
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Age of Movie", ylab = "Views")

scatterplot(duration ~ age, data = rel, 
            smoother = FALSE, grid = FALSE, frame = FALSE)
            
# -------------------------------------------------------

n <- read.csv('netflix.csv')

n <- n %>%
  distinct() %>%
  drop_na()

n %>%
  mutate(watch = if_else(duration==0, 'no', 'yes')) %>%
  select(X, watch) -> w

s <- strsplit(n$genres, split = ", ")
movie <- data.frame(X = rep(n$X, sapply(s, length)), genres = unlist(s))

rel <- merge(w, movie[, c("X", "genres")], by="X")

rel %>%
  ggplot(aes(x = genres, fill = watch)) +
  geom_bar(position = "fill")

chisq.test( table(rel$genres, rel$watch) )
library("zoo")
netflix %>%
  select(movie_id, release_date) %>%
  distinct() %>%
  mutate(month = as.yearmon(release_date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(n = n()) %>%
  ggplot(aes(x=month,y=n)) +
  geom_col()
