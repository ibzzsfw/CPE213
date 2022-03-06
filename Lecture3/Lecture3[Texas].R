library(tidyverse)
library(zoo)
superstore <- read.csv("superstore.csv")
texas <- superstore %>% filter(State=="Texas")
texas1 <- texas %>% filter(Sub.Category!="Binders")

texas %>% 
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  group_by(Q) %>%
  summarise(Profit=sum(Profit)) %>%
  mutate(Type = "Before") -> t

texas1 %>% 
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  group_by(Q) %>%
  summarise(Profit=sum(Profit)) %>%
  mutate(Type = "After") -> t1

t %>% 
  bind_rows(t, t1) %>%
  ggplot(aes(x = Q, y = Profit, colour = Type)) +
  geom_line()

texas %>% 
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  ggplot(aes(Sub.Category, Profit, color=Sub.Category,fill=Sub.Category)) +
  geom_jitter() +
  annotate("rect", 
           xmin=c(0,1,11.5), xmax=c(18,5,16.2),
           ymin=c(-500,-4000,-1500) , ymax=c(0,-600,-720),
           alpha=0.1, color="blue", fill="blue")
  
superstore %>%
  filter(!(State=="Texas"&Profit<=0)) %>%
  group_by(State) %>%
  summarise(Profit = sum(Profit)) %>%
  ggplot() + geom_col(aes(x = State,
                          y = Profit,
                         )
  )


binders_customer <- texas %>%
  filter(Sub.Category=="Binders") %>%
  select(Customer.ID)

texas2 <- texas %>%
  filter(!(Customer.ID%in%binders_customer$Customer.ID)) %>%
  mutate(Q = as.yearqtr(as.Date(substr(Order.Date,1,10)), format = "%Y-%m-%d")) %>%
  group_by(Q) %>%
  summarise(Profit=sum(Profit)) %>%
  mutate(Type = "Without Binders customer") -> t2

t %>% 
  bind_rows(t, t1, t2) %>%
  ggplot(aes(x = Q, y = Profit, colour = Type)) +
  geom_line()
