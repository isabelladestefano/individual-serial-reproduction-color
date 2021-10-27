serdep_data_exp1 <-test_data_exp1 %>%
  #filter(type == "fixed")%>%
  group_by(subject, currentBlock)%>% 
  mutate(dist_prev_trial = circDist(lag(items), items)) %>%
  filter(!is.na(dist_prev_trial)) 

serdep_data_exp1 %>%
  ggplot(aes(x= dist_prev_trial))+
  geom_histogram()

serial_dependence_sliding_window <- function(data){ 
  window_size = 90
  window_step = 6
  window_max = 180 
  window_min = -180
  
  serdep_sliding_window<- data.frame(subject = c(),  error = c(), N= c(), dist= c())
  
  for(i in seq(window_min,window_max,6)){
    cur_min = i-window_size/2
    cur_max = i+window_size/2
    
    if(abs(cur_min) < -180){
      cur_min = cur_min+360
      serdep_sliding_window <- data%>%
        filter(dist_prev_trial >= cur_min) %>%
        bind_rows(data %>% filter(dist_prev_trial< cur_max))%>%
        group_by(subject)%>%
        summarise(error = mean(error), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
    if(abs(cur_max) >= 180){
      cur_max = cur_max-360
      serdep_sliding_window <- data %>%
        filter(dist_prev_trial >= cur_min) %>%
        bind_rows(data %>% filter(dist_prev_trial< cur_max))%>%
        group_by(subject)%>%
        summarise(error = mean(error), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
    else{
    serdep_sliding_window <- data %>%
      filter(dist_prev_trial < cur_max, dist_prev_trial >= cur_min) %>%
      group_by(subject)%>%
      summarise(error = mean(error), N=n())%>% 
      mutate(dist = i)%>%
      bind_rows(serdep_sliding_window)
    }
  }
  return(serdep_sliding_window)
}

serdep_sliding_window_exp1 <- serial_dependence_sliding_window(serdep_data_exp1) 

serdep_sliding_window_exp1_summary = serdep_sliding_window_exp1%>%
  group_by(dist)%>%
  summarise(error = mean(error))

serdep_sliding_window_exp1 %>% arrange(subject) %>%
  ggplot(aes(x=dist, y = error))+
  geom_point(data = serdep_sliding_window_exp1_summary, aes(x=dist, y = error))+
  theme_minimal()+
  geom_smooth()+
  geom_point(aes(x=dist, y =error), alpha = 0.01)+
  geom_hline(yintercept = mean(serdep_sliding_window_exp1_summary$error), size = 1, linetype = 2, col = "red")+
  geom_hline(yintercept = 0, size = .5, linetype = 1)+
  geom_vline(xintercept = 0, size = .5, linetype = 1)+
  ggtitle("all trials exp 1")+
  ylim(-10,10)

## chain data only???
serdep_chain_data_exp1 <-test_data_exp1 %>%
  group_by(subject, currentBlock)%>% 
  mutate(dist_prev_trial = circDist(lag(items), items)) %>%
  filter(type == "fixed")%>%
  filter(abs(error)<22.5) %>%
  group_by(subject, id )%>%
  arrange(iteration)%>%
  mutate(iteration = 1:length(unique(iteration)))%>%
  ungroup() %>%
  filter(!is.na(dist_prev_trial))

serdep_sliding_window_chain_exp1 <- serial_dependence_sliding_window(serdep_chain_data_exp1)

serdep_sliding_window_chain_exp1_summary = serdep_sliding_window_chain_exp1%>%
  group_by(dist)%>%
  summarise(error = mean(error))

serdep_sliding_window_chain_exp1 %>% arrange(subject) %>%
  ggplot(aes(x=dist, y = error))+
  geom_point(data = serdep_sliding_window_chain_exp1_summary, aes(x=dist, y = error))+
  theme_minimal()+
  geom_smooth()+
  geom_point(aes(x=dist, y =error), alpha = 0.01)+
  geom_hline(yintercept = mean(serdep_sliding_window_chain_exp1_summary$error), size = 1, linetype = 2, col = "red")+
  geom_hline(yintercept = 0, size = .5, linetype = 1)+
  geom_vline(xintercept = 0, size = .5, linetype = 1)+
  ggtitle("chain data exp1")+
  ylim(-10,10)


### exp 2

serdep_data_exp2 <-test_data_exp2 %>% #filter(type %in% c( "serial"))%>%
  group_by(subject, currentBlock)%>% 
  mutate(items = items %% 360)%>%
  mutate(dist_prev_trial = circDist(lag(items), items)) %>%
  filter(!is.na(dist_prev_trial)) 

serdep_data_exp2 %>%
  #filter(type == "fixed")%>%
  ggplot(aes(x=dist_prev_trial))+
  geom_histogram(bins = 360)


serdep_sliding_window_exp2 <- serial_dependence_sliding_window(serdep_data_exp2)

serdep_sliding_window_exp2_summary = serdep_sliding_window_exp2%>%
  group_by(dist)%>%
  summarise(error = mean(error))

serdep_sliding_window_exp2 %>% arrange(subject) %>%
  ggplot(aes(x=dist, y = error))+
  geom_point(data = serdep_sliding_window_exp2_summary, aes(x=dist, y = error))+
  theme_minimal()+
  geom_smooth()+
  geom_point(aes(x=dist, y =error), alpha = 0.01)+
  geom_hline(yintercept = mean(serdep_sliding_window_exp2_summary$error), size = 1, linetype = 2, col = "red")+
  geom_hline(yintercept = 0, size = .5, linetype = 1)+
  geom_vline(xintercept = 0, size = .5, linetype = 1)
#ylim(-10,10)


