library(tidyverse)

ggplot(economics, aes(x = date, y = unemploy)) + geom_line()

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_ribbon(aes(ymin = unemploy - 900, ymax = unemploy + 900))

ggplot(economics, aes(x = date, y = unemploy)) +
  geom_step(direction = 'hv') +
  xlim(as.Date(c('1/1/1970', '1/1/1980'), format="%d/%m/%Y") )