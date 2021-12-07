library(tidyverse)

all_data_exp4 <- read_csv("data/category-naming/all_data_exp4.csv")
all_data_exp5 <- read_csv("data/category-naming/all_data_exp5.csv")


#exp4
plot_exp4 = all_data_exp4 %>%
  ggplot(aes(x=item, fill = colorVal, group = colorname))+
  geom_histogram(binwidth = 1, position = "identity")+
  scale_fill_identity()+  theme_minimal()+
  theme(panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = seq(0,360,60))+
  scale_y_continuous(breaks = seq(0,50,25), labels = c())+
  theme(axis.text.x = element_text(angle = 30),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  #facet_grid(~colorname)+
  labs(y="Count", x = "Color (degrees)")


plot_exp4


props = all_data_exp4 %>%
  group_by(colorname)%>%
  summarise(N=n()) %>%
  mutate(prop=N/sum(N))%>%
  dplyr::select(colorname,prop)

all_data_exp4$colorVal

tmp = all_data_exp4 %>%
  #mutate(item = .bincode(item, breaks = seq(0,360,1), include.lowest = T))%>%
  group_by(colorname, item)%>%
  summarise(N=n())

tmp$colorVal = sapply(tmp$item, convert360ToColorVal)
tmp%>%
  ggplot(aes(x=item, y= N, group = colorname, fill = colorVal, col = colorVal))+
  geom_histogram(binwidth = 5, position = "identity", stat = "identity", alpha = .8)+
  theme_minimal()+
  scale_fill_identity()+
  scale_color_identity()+
  #scale_x_continuous(breaks = seq(0,72,2), labels = seq(0,360,10))+
  labs(y="count", x="colors reproduced")+
  #scale_y_continuous(breaks = seq(0,50,25), labels = c())+
  theme(axis.text.x = element_text(angle = 30))+
  #facet_grid(~colorname)+
  labs(y="Count", x = "Color (degrees)")#+
#ggtitle("Category given color")

all_data_exp4 %>% pull(subject)%>% unique() %>% length()



#exp5



weighted_data_exp5 = all_data_exp5 %>%
  mutate(color = .bincode(color, breaks = seq(0,360,5), include.lowest = T))%>%
  group_by(color, colorVal, colorname) %>%
  summarise(N=n())%>%
  ungroup()%>%
  complete(color, colorname,
           fill = list(N = 0)) %>%
  inner_join(props, by = "colorname")%>%
  mutate(N=N*prop)


weighted_data_exp5 %>%
  ggplot(aes(x=color, y= N, group = colorname, fill = colorVal, color = colorVal))+
  geom_histogram(position = "identity", stat = "identity")+
  scale_fill_identity()+  
  scale_color_identity()+ 
  theme_minimal()

plot_exp5 =all_data_exp5%>%
  ggplot(aes(x=color, fill = colorVal))+
  geom_histogram(aes(group = colorname),binwidth = 5, alpha = 0.5, position = "identity")+
  scale_fill_identity()+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank())+
  scale_x_continuous(breaks = seq(0,360,60))+
  scale_y_continuous(breaks = seq(0,50,25), labels = c())+
  #facet_grid(~colorname)+
  theme(axis.text.x = element_text(angle = 30),
        panel.border = element_rect(colour = "black", fill=NA, size=1))+
  labs(y="Count", x = "Color (degrees)")

all_data_exp5 %>% pull(subject)%>% unique() %>% length()




