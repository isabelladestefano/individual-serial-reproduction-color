all_data_exp1%>%
  ggplot(aes(x=colorOffset))+
  geom_histogram()


all_data_exp1%>%
  group_by(subject, direction)%>%
  summarise(N=n())%>%
  spread(direction,N)%>%
  mutate(total = cw+ccw, prop_cw = cw/total) %>%
  ggplot(aes(x = prop_cw))+
  geom_histogram()+
  xlim(0.4,0.6)+
  xlab("prop clockwise color wheel by subject")

