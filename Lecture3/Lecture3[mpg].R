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

ggplot(data = mpg) +
  geom_dotplot(mapping = aes(x=hwy, fill=class), dotsize = 0.5)

ggplot(mpg) +
  geom_violin(aes(x = class, y = hwy, fill = class, color = class))

ggplot(data = mpg) +
  geom_bin2d(mapping = aes(x=displ, y=cty), bins=10) +
  scale_fill_gradient(low="black", high="#00FF00")

ggplot(mpg) + geom_bar(aes(x=class, fill=factor(year)), position = 'dodge')
# dodge: compare, stack:, fill: 100% plot
