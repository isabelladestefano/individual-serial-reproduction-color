circular_distance = function(x,y){
  a = x - y
  (a+ 30) %% 60 - 30
}



circular_mean = function(x){
  x= x*6/180*pi
  s_bar = 1/length(x) * sum(sin(x))
  c_bar = 1/length(x) * sum(cos(x))
  theta = case_when(s_bar>0 & c_bar > 0 ~ atan(s_bar/c_bar),
            c_bar<0 ~ atan(s_bar/c_bar) + pi,
            s_bar<0 & c_bar>0 ~ atan(s_bar/c_bar) + 2*pi)
  
  theta/pi*180/6
}


nearest_cat_distance = function(x){
  #min(abs(circular_distance(x, categories[1])),  abs(circular_distance(x, categories[2])))
  whichcat = which(c(abs(circular_distance(x, categories[1])),  
                     abs(circular_distance(x, categories[2]))) == 
                     min(abs(circular_distance(x, categories[1])),  
                         abs(circular_distance(x, categories[2]))))[1]
  circular_distance(x,categories[whichcat])
  
}

nearest_cat_direction = function(x){
  whichcat = which(c(circular_distance(x, categories[1]),  circular_distance(x, categories[2])) == max(circular_distance(x, categories[1]),  circular_distance(x, categories[2])))
  if(whichcat == 2){
    x-categories[whichcat] 
  }else{
    x- categories[whichcat]
    
  }
}

postpred_repel = function(cols){
  col1 = cols[1]
  col2 = cols[2]
  circular_intervals %>%
  inner_join(cat_sets) %>%
  filter((cat1|cat2))%>%
  group_by(setID) %>%
  mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
  filter( (include_col1 & !include_col2))%>%
  filter(include_col1 )%>%
  mutate(size = 1/size) %>%
  group_by(set) %>%
  summarise(p = sum(size)) %>%
  ungroup() %>%
  mutate(p = p/sum(p))
}

postpred_circmean_repel = function(cols){
  postpred_repel(cols)%>%
    mutate(p = round(p*100000))%>%
    group_by(set)%>%
    do(tmp = rep(.$set, .$p)) %>% pull(tmp) %>% unlist %>%
    circular_mean()
}


postpred_attract = function(cols){
  col1 = cols[1]
  col2 = cols[2]
  circular_intervals %>%
    inner_join(cat_sets) %>%
    filter((cat1|cat2))%>%
    group_by(setID) %>%
    mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
    filter( (include_col1 & include_col2))%>%
    filter(include_col1 )%>%
    mutate(size = 1/size) %>%
    group_by(set) %>%
    summarise(p = sum(size)) %>%
    ungroup() %>%
    mutate(p = p/sum(p))
}


postpred_circmean_attract = function(cols){
  postpred_attract(cols)%>%
    mutate(p = round(p*100000))%>%
    group_by(set)%>%
    do(tmp = rep(.$set, .$p)) %>% pull(tmp) %>% unlist %>%
    circular_mean()
}

effects_repel = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  apply(.,MARGIN = c(1), FUN = postpred_circmean_repel)


effects_repel = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  mutate(effect = effects_repel)

  
effects_repel %>%
  mutate(offset = circular_distance(col1, col2), 
         bias = circular_distance(col1, effect),
         nearest_cat = sapply(col1, nearest_cat_distance)) %>%
  filter(col1!=col2) %>%
  #filter(offset>0)%>%
  filter(col1 %in% c(0,5,10,15,20,25,30,35,40,45,50,55))%>%
  ggplot(aes(x = offset , y = bias, col = nearest_cat, group = nearest_cat))+
  geom_point()+
  geom_line()+
  ylim(-20,20)


effects_repel %>%
  mutate(offset = circular_distance(col1, col2), 
         bias = circular_distance(col1, effect),
         nearest_cat = sapply(col1, nearest_cat_distance)) %>%
  filter(col1!=col2) %>%
  filter(abs(offset)<30)%>%
  filter(offset %in% c(5,-5,10,-10,15, -15, 20,-20, 25,-25,30,-30))%>%
  #filter(col1 %in% categories)%>%
  ggplot(aes(x=nearest_cat, y = bias, col = abs(offset), group = (offset)))+
  geom_point()+
  geom_line()


effects_attract = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  apply(.,MARGIN = c(1), FUN = postpred_circmean_attract)


effects_attract = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  mutate(effect = effects_attract)


effects_attract %>%
  mutate(offset = circular_distance(col1, col2), 
         bias = circular_distance(col1, effect),
         nearest_cat = sapply(col1, nearest_cat_distance)) %>%
  filter(col1!=col2) %>%
  filter(abs(offset)<30)%>%
  filter(col1 %in% c(0, 5,10,15,20,25,30,35,40,45,50,55))%>%
  ggplot(aes(x = offset , y = bias, col = nearest_cat, group = nearest_cat))+
  geom_point()+
  geom_line()+
  ylim(-20,20)


### without categories



postpred_nocat_repel = function(cols){
  col1 = cols[1]
  col2 = cols[2]
  circular_intervals %>%
    inner_join(cat_sets) %>%
    group_by(setID) %>%
    mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
    filter( (include_col1 & !include_col2))%>%
    filter(include_col1 )%>%
    mutate(size = 1/size) %>%
    group_by(set) %>%
    summarise(p = sum(size)) %>%
    ungroup() %>%
    mutate(p = p/sum(p))
}

postpred_circmean_nocat_repel = function(cols){
  postpred_nocat_repel(cols)%>%
    mutate(p = round(p*100000))%>%
    group_by(set)%>%
    do(tmp = rep(.$set, .$p)) %>% pull(tmp) %>% unlist %>%
    circular_mean()
}

effects_nocat_repel= expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  apply(.,MARGIN = c(1), FUN = postpred_circmean_nocat_repel)


effects_nocat_repel = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  mutate(effect = effects_nocat_repel)



sliding_window<- function(data){ 
  window_size = 6
  window_step = 1
  window_max = 30 
  window_min = -30
  
  serdep_sliding_window<- data.frame( bias = c(), N= c(), dist= c())
  
  for(i in seq(window_min-window_size,window_max+window_size,by = window_step)){
    cur_min = i-window_size/2
    cur_max = i+window_size/2
    
    if(abs(cur_min) < -30){
      
      serdep_sliding_window <- data%>%
        filter(offset >= cur_min) %>%
        bind_rows(data %>% filter(offset< cur_max))%>%
        summarise(bias = circular_mean(bias), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
    if(abs(cur_max) >= 30){
      cur_max = cur_max-60
      serdep_sliding_window <- data %>%
        filter(offset >= cur_min) %>%
        bind_rows(data %>% filter(offset< cur_max))%>%
        summarise(bias = mean(bias), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
    else{
      serdep_sliding_window <- data %>%
        filter(offset < cur_max, offset >= cur_min) %>%
        summarise(bias = mean(bias), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
  }
  return(serdep_sliding_window)
}

smooth_effects_noncat_repel = effects_nocat_repel%>%
  mutate(offset = circular_distance(col1, col2), 
         bias = circular_distance(col1, effect)) %>%
  filter(abs(offset)<30)%>%
  sliding_window()

smooth_effects_noncat_repel%>%
  ggplot(aes(x=dist, y = bias))+
  geom_smooth(data = smooth_effects_noncat_repel, aes(x=dist, y = bias), se=F)


effects_nocat_repel_3 %>%
  mutate(offset = circular_distance(col1, col2), 
         bias = circular_distance(col1, effect)) %>%
  filter(abs(offset)<30)%>%
  filter(col1!=col2) %>%
  filter(col1 %in% c(0,5,10,15,20,25,30,35,40,45,50,55))%>%
  ggplot(aes(x = offset , y = bias))+
  geom_point( col = "red", size = 2)+
  geom_line(col = "red", size = 2)+
  ylim(-10,10)+
  geom_smooth(data = smooth_effects_noncat_repel_3 %>%filter(abs(dist)<30), aes(x=dist, y = bias), se=F)+
  theme_minimal()







postpred_nocat_attract = function(cols){
  col1 = cols[1]
  col2 = cols[2]
  circular_intervals %>%
    inner_join(cat_sets) %>%
    group_by(setID) %>%
    mutate( include_col1 = any(col1 %in% set), include_col2 = any(col2 %in% set)) %>%
    filter( (include_col1 & include_col2))%>%
    filter(include_col1 )%>%
    mutate(size = 1/size) %>%
    group_by(set) %>%
    summarise(p = sum(size)) %>%
    ungroup() %>%
    mutate(p = p/sum(p))
}

postpred_circmean_nocat_attract = function(cols){
  postpred_nocat_attract(cols)%>%
    mutate(p = round(p*100000))%>%
    group_by(set)%>%
    do(tmp = rep(.$set, .$p)) %>% pull(tmp) %>% unlist %>%
    circular_mean()
}

effects_nocat_attract = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  apply(.,MARGIN = c(1), FUN = postpred_circmean_nocat_attract)


effects_nocat_attract = expand.grid(col1 = c(0:59), col2 = c(0:59)) %>%
  mutate(effect = effects_nocat_attract)



effects_nocat_attract %>%
  mutate(offset = circular_distance(col1, col2), 
         bias = circular_distance(col1, effect)) %>%
  filter(offset>-30)%>%
  filter(col1!=col2) %>%
  filter(col1 %in% c(0,5,10,15,20,25,30,35,40,45,50,55))%>%
  ggplot(aes(x = offset , y = bias))+
  geom_point( col = "red", size = 2)+
  geom_line(col = "red", size = 2)+
  geom_smooth(data = smooth_effects_noncat_attract, aes(x=dist, y = bias), se=F)
 
  


#smoothing

