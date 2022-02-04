superstore <- read.csv("superstore.csv")

s1 <- superstore %>%
  group_by(State) %>%
  summarise(Profit = sum(Profit)) %>%
  filter(Profit <= 0) 

region <- superstore %>% distinct(State, Region)

s1 %>%
  left_join(region, s1, by="State") %>%
  ggplot() + geom_col(aes(x = State,
                          y = Profit,
                          fill = Region,
                          color = Region)
                      )

superstore %>%
  #filter(State%in%s1$State) %>%
  filter(State=="Texas") %>%
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  group_by(Q) %>%
  summarise(Profit=sum(Profit)) %>%
  ggplot(aes(x = Q, y = Profit)) +
  geom_line() +
  coord_cartesian(xlim = c(2013, 2014), ylim = c(-50, 50))

s2 <- superstore %>%
  filter(State=="Texas") %>%
  group_by(Sub.Category) %>%
  summarise(Profit = sum(Profit)) %>%
  filter(Profit <= 0)
 
superstore %>% 
  filter(State=="Texas" & Sub.Category%in%s2$Sub.Category) %>%
  ggplot(aes(Sub.Category, Profit)) +
  geom_jitter()

superstore %>% 
  filter(State=="Texas") %>%
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  group_by(Q) %>%
  summarise(Profit=sum(Profit)) %>%
  ggplot(aes(Q, Profit)) +
  geom_line()

superstore %>% 
  filter(State%in%s1$State & Sub.Category=="Binders") %>%
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  ggplot(aes(x = Q, y = Profit, fill = State)) +
  geom_col(position = "dodge")

superstore %>% 
  filter(State=="Texas" & Sub.Category%in%s2$Sub.Category) %>%
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  ggplot(aes(x = Q, y = Profit, fill = Sub.Category)) +
  geom_col(position = "dodge")


