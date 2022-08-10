library(tidyverse)

MINX = 0
MAXX = 60

#categories = 30, 60, 90 

intervals = data.frame(set = c())


for(i in MINX:(MAXX-1)){
  for(j in MINX:(2*MAXX-1)){
    if(i<=j){
      intervals = bind_rows(intervals,data.frame(set =i:j, setID = paste("[",i,", ",j, "]"), size = length(i:j)))
    }  
  }
}

  
categories = c(15,45) 

circular_intervals = intervals %>% 
  filter(size <60) %>%
  mutate(set = set%%MAXX)


tmp1 = function(set){
  c1 = categories[1] %in% set
  c2 = categories[2] %in% set


  (!c1 & c2) 
}


tmp2 = function(set){
  c1 = categories[1] %in% set
  c2 = categories[2] %in% set
  
  
  (c1 & !c2) 
}

cat_sets = circular_intervals%>% group_by(setID) %>%
  summarise(cat2 = tmp1(set), cat1 = tmp2(set)) 



circular_intervals %>%
  inner_join(cat_sets) %>%
  filter((cat1 | cat2)) %>%
  ggplot(aes(x= set))+
  geom_histogram()+
  coord_polar()

circular_intervals %>%
  inner_join(cat_sets) %>%
  #filter(cat2)%>%
  ggplot(aes(x= set))+
  geom_histogram()

circular_intervals %>%
  inner_join(cat_sets) %>%
  filter(cat1)%>%
  ggplot(aes(x= set))+
  geom_histogram()



item = 30

circular_intervals %>%
  inner_join(cat_sets) %>%
  filter(cat1)%>%
  group_by(setID)%>%
  filter(item %in% set)%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p)) %>%
  ggplot(aes(x=set, y = p))+
  geom_histogram(stat = "identity")
  

circular_intervals %>%
  inner_join(cat_sets) %>%
  filter(cat2)%>%
  group_by(setID)%>%
  filter(item %in% set)%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p)) %>%
  ggplot(aes(x=set, y = p))+
  geom_histogram(stat = "identity")

circular_intervals %>%
  inner_join(cat_sets) %>%
  filter((cat1|cat2))%>%
  group_by(setID)%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p)) %>%
  ggplot(aes(x=set, y = p))+
  geom_histogram(stat = "identity")+
  theme_minimal()#+
  #coord_polar()







col1 =30
col2 = 31



circular_intervals %>%
  inner_join(cat_sets) %>%
  #filter((cat1|cat2))%>%
  group_by(setID) %>%
  mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
  #filter( (include_col1 & !include_col2))%>%
  filter(include_col1 )%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p))%>%
  
  ggplot(aes(x= set, y = p))+
  geom_histogram(binwidth = 1, alpha = .5, stat="identity")+
  geom_vline(data = data.frame(col = c(col1)), aes(xintercept=col), col = "red")+
  #geom_vline(data = data.frame(categories = categories), aes(xintercept = categories))+
  #geom_vline(xintercept = col1)+
  theme_minimal()#+
   coord_polar()
  #theme(panel.grid = element_blank(),
  #      axis.text = element_blank(),
  #      axis.title = element_blank())+





circular_intervals %>%
  inner_join(cat_sets) %>%
  filter((cat1|cat2))%>%
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
  geom_vline(data = data.frame(col = c(col1,col2)), aes(xintercept=col), col = "red")+
  geom_vline(data = data.frame(categories = categories), aes(xintercept = categories))+
  #geom_vline(xintercept = col1)+
  theme_minimal()+

  coord_polar()





circular_intervals %>%
  inner_join(cat_sets) %>%
  filter(cat2)%>%
  ungroup()%>%
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







postpred_category = function(cols){
  col1 = cols[1]
  col2 = cols[2]
  circular_intervals %>%
    inner_join(cat_sets) %>%
    filter((cat1|cat2))%>%
    group_by(setID) %>%
    mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
    filter(include_col1 )%>%
    mutate(size = 1/size) %>%
    group_by(set) %>%
    summarise(p = sum(size)) %>%
    ungroup() %>%
    mutate(p = p/sum(p))
}

postpred_circmean_category = function(cols){
  postpred_category(cols)%>%
    mutate(p = round(p*100000))%>%
    group_by(set)%>%
    do(tmp = rep(.$set, .$p)) %>% pull(tmp) %>% unlist %>%
    circular_mean()
}


effects_cat = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  apply(.,MARGIN = c(1), FUN = postpred_circmean_category)


effects_cat = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  mutate(effect = effects_cat)


effects_cat %>%
  mutate(
         bias = circular_distance(col1, effect),
         nearest_cat = sapply(col1, nearest_cat_distance)) %>%
  filter(col1!=col2) %>%
  ggplot(aes(x = nearest_cat, y = bias))+
  geom_point()+
  geom_line()

  