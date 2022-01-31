library(tidyverse)
data(mpg)
p <- ggplot(data = mpg)
p + geom_col(mapping = aes(x=class, y=hwy))

mpg %>%
  mutate_if(is.character, as.factor) %>%
  summary()

p1 <- ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) + 
  geom_point(mapping = aes(x=cty, y=hwy, color=class))
p2 <- p1 + geom_smooth(method = 'lm')
p3 <- p2 + scale_color_brewer(type = 'qual') + facet_grid(fl ~ cyl)

ggplot(data = mpg) + geom_bar(mapping = aes(x=class))

mpg %>%
  group_by(class) %>%
  summarise(avg_hwy=mean(hwy)) %>%
  ggplot() + geom_col(aes(x=class, y=avg_hwy))
