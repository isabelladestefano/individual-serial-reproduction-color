library(tidyverse)
MINX = 0
MAXX = 30
#categories = 30, 60, 90 

intervals = data.frame(set = c())


for(i in MINX:(MAXX-1)){
  for(j in MINX:(2*MAXX-1)){
    if(i<j){
      intervals = bind_rows(intervals,data.frame(set =i:j, setID = paste("[",i,", ",j, "]"), size = length(i:j)))
    }  
  }
}


categories = c(15,45) 

circular_intervals = intervals %>% 
  filter(size <MAXX) %>%
  mutate(set = set%%MAXX)

col1 = 20
col2 =25



circular_intervals %>%
  group_by(setID) %>%
  mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
  filter( (include_col1 & !include_col2))%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p))%>%
  
  ggplot(aes(x= set, y = p))+
  geom_histogram(binwidth = 1, alpha = .5, stat="identity")+
  geom_point(data = data.frame(col = c(col1,col2)), aes(x=col, y = 0))+
  geom_point(data = data.frame(col = c(col1,col2)), aes(x=col, y = 0))+
  #geom_vline(xintercept = col1)+
  xlim(0,50)+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())




  circular_intervals %>%
    group_by(setID) %>%
    mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
    filter( (include_col1 & !include_col2))%>%
    mutate(size = 1/size) %>%
    group_by(set) %>%
    summarise(p = sum(size)) %>%
    ungroup() %>%
    mutate(p = p/sum(p))%>%
    
  ggplot(aes(x= set, y = p))+
  geom_histogram(binwidth = 1, alpha = .5, stat="identity")+
  geom_point(data = data.frame(col = c(col1,col2)), aes(x=col, y = 0))+
  geom_point(data = data.frame(col = c(col1,col2)), aes(x=col, y = 0))+
    #geom_vline(xintercept = col1)+
  xlim(0,50)+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

  
  
  
  
  circular_intervals %>%
    group_by(setID) %>%
    mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
    filter( (include_col1 & include_col2))%>%
    mutate(size = 1/size) %>%
    group_by(set) %>%
    summarise(p = sum(size)) %>%
    ungroup() %>%
    mutate(p = p/sum(p))%>%
    
    ggplot(aes(x= set, y = p))+
    geom_histogram(binwidth = 1, alpha = .5, stat="identity")+
    geom_point(data = data.frame(col = c(col1,col2)), aes(x=col, y = 0))+
    geom_point(data = data.frame(col = c(col1,col2)), aes(x=col, y = 0))+
    #geom_vline(xintercept = col1)+
    xlim(0,50)+
    theme_minimal()+
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank())

 
  
### size principle animation
   
segment_intervals = circular_intervals %>%
  group_by(setID) %>%
  mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
  #filter((include_col1))%>%
  group_by(setID,size)%>%
  do(start = head(.$set,1), end = tail(.$set,1)) %>%
  mutate(start = unlist(start), end = unlist(end))%>%
  ungroup()%>%
  arrange(size) %>%
  ungroup()

include_interval = segment_intervals%>%
  pull(setID) %>% unique()

weighted_intervals =   circular_intervals %>%
  group_by(setID) %>%
  filter(setID %in% include_interval)%>%
  mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
  filter(include_col1)%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p)*100) 

segment_intervals %>%
  #mutate(start = start*360/15, end = end*360/15)%>%
  arrange(start,end)%>%
  mutate(y = -1:-nrow(segment_intervals))%>%
  mutate(start = start - 0.5, end = end+.5)%>%
  filter(start<end)%>%
  ggplot()+
  geom_segment(aes(x=start, xend = end, y = y, yend =y))+
  #geom_histogram(data = weighted_intervals, aes(x=set, y = p), binwidth = 1, alpha = .5, stat="identity", fill = NA)+
  theme_bw()+
  #xlim(c(10,22))+
  scale_y_continuous(limits = c(-nrow(segment_intervals),NA))
  #coord_polar("x")
  


#