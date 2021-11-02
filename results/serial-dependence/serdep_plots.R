require(tidyverse)
source("results/serial-dependence/serialDependenceFunctions.R")




serdep_data_exp1 <-test_data_exp1 %>%
  #filter(type == "fixed")%>%
  group_by(subject, currentBlock)%>% 
  mutate(dist_prev_trial = circDist(lag(items), items)) %>%
  filter(!is.na(dist_prev_trial)) 

serdep_data_exp1 %>%
  ggplot(aes(x= dist_prev_trial))+
  geom_histogram()



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


fits_exp1 <- DoG_subjectFits(serdep_sliding_window_exp1)

i=2
subject_pars = fits_exp1 %>% filter(subject == i)
\\fit = data.frame(x=-180:180) %>% mutate(y = DoG(x,subject_pars$b, subject_pars$a, subject_pars$w))

serdep_sliding_window_exp1 %>% 
  filter(subject == i)%>%
  ggplot(aes(x=dist, y = error))+
  geom_point( aes(x=dist, y = error))+
  geom_line(data = fit, aes(x=x,y=y))





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
  filter(!is.na(dist_prev_trial)) %>%
  filter(iteration>5)

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






