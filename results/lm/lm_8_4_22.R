library(tidyverse)
#exp1
#get summary for z-scoring slopes
sub_error_summary_exp1 = test_data_exp1 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

#zscore error within subject, calculate relative position
model_chain_data_exp1  = chain_data_exp1%>%
  filter(seed %in% c(18,90,162,234,306))%>%
  inner_join(sub_error_summary_exp1)%>%
  group_by(subject)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error))%>%
  mutate(folded_relative_pos = cumsum(error))%>%
  mutate(experiment = "exp1") %>%
  mutate(stim_direction = 0) %>%
  mutate(stim_distance = abs(stim_direction))%>%
  mutate(seedID = paste(seed))%>%
  ungroup()%>%
  select(iteration, relative_pos, folded_relative_pos, z_relative_pos, seed, experiment, stim_direction,seedID, stim_distance)


#exp 2
sub_error_summary_exp2 = test_data_exp2 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

model_chain_data_exp2 = chain_data_exp2%>%
  inner_join(sub_error_summary_exp2)%>%
  group_by(subject)%>%
  mutate(serialSign = if_else(stim_direction>0 , 1, -1))%>%
  mutate(error = error*serialSign)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error)) %>%
  mutate(folded_relative_pos = cumsum(error))%>%
  mutate(experiment = "exp2")%>%
  mutate(seedID = paste(seed, " ", stim_direction))%>%
  mutate(stim_distance = abs(stim_direction))%>%
  ungroup()%>%
  select(iteration, relative_pos, folded_relative_pos, z_relative_pos, seed, experiment, stim_direction,seedID, stim_distance)  

#exp3
sub_error_summary_exp3 = test_data_exp3 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

model_chain_data_exp3 = chain_data_exp3
model_chain_data_exp3$stim_direction = mapply(circDist,  model_chain_data_exp3$anchor,model_chain_data_exp3$items)
model_chain_data_exp3 = model_chain_data_exp3 %>%
  inner_join(sub_error_summary_exp3)%>%
  group_by(subject)%>%
  mutate(serialSign = if_else(stim_direction>0 , 1, -1))%>%
  mutate(error = error*serialSign)%>%
    mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error))%>%
  mutate(folded_relative_pos = cumsum(error))%>%
  mutate(seedID = paste(seed, " ", stim_direction))%>%
  mutate(experiment = "exp3") %>%
  mutate(stim_distance = abs(stim_direction))
  


model_chain_data_exp3 <- model_chain_data_exp3 %>%   
  ungroup()%>%
  select(iteration, relative_pos, folded_relative_pos, z_relative_pos, seed, experiment, stim_direction,seedID, stim_distance)
#treat the stim_direction as 10 seed, cw seed1 ccw seed1,
# all are different vs. null all the same


all_z_chain_data = bind_rows(z_chain_data_exp1, z_chain_data_exp2, z_chain_data_exp3)

# experiment 1 stimulus specific variability
model_null_exp1 <- lm(data = model_chain_data_exp1, formula = relative_pos~0+iteration)
model_exp1 <- lm(data = model_chain_data_exp1, formula = relative_pos ~ 0+ iteration + iteration:seed)
anova(model_exp1)


#experiment 2 and 3

exp2exp3_model_chain_data = bind_rows( model_chain_data_exp2, model_chain_data_exp3)
model_null_exp2exp3 <- lm(data = exp2exp3_model_chain_data, formula = folded_relative_pos ~ iteration)

model_all_exp2exp3 <- lm(data = exp2exp3_model_chain_data, formula = folded_relative_pos ~ iteration:experiment +  iteration:seedID )
summary(model_all_exp2exp3)
anova(model_all_exp2exp3)

exp2exp3_model_chain_data = bind_rows( model_chain_data_exp2, model_chain_data_exp3) %>%
  mutate(folded_relative_pos = if_else(experiment == "exp2", folded_relative_pos, -folded_relative_pos))
exp2exp3_model_chain_data%>%
  mutate(groupingVar = paste(seed, stim_direction,experiment))%>%
  mutate(stim_direction = factor(stim_direction))%>%
  ggplot()+
  #geom_smooth(aes(x=iteration, y = folded_relative_pos, group = groupingVar, col = experiment),formula = y~0+x, method = "lm", se = F, linetype = 2, size = 0.5)+
  stat_summary(data = exp2exp3_model_chain_data, aes(x= iteration, y= folded_relative_pos, col = experiment),geom = "errorbar", fun.data = mean_se)+
  geom_hline(yintercept = 0, color = "black")+
  theme_minimal()+
  scale_x_continuous(expand = c(0,0))

#experiment 1 2 3
all_model_chain_data = bind_rows(model_chain_data_exp1, model_chain_data_exp2 ,model_chain_data_exp3)

model_all_null <- lm(data = all_model_chain_data, formula = relative_pos~ iteration:seed + iteration:stim_direction:experiment)
model_all <- lm(data = all_model_chain_data, formula = relative_pos~ iteration:seed + iteration:stim_direction:experiment  + iteration:seedID)

anova(model_all)
#plots



#relative_pos exp 1
ggplot(aes(x = ))

#folded_relative_pos exp2&3
bind_rows(model_chain_data_exp2, model_chain_data_exp3) %>%
  mutate(groupingVar = paste(seed, stim_direction,experiment))%>%
  mutate(stim_direction = factor(stim_direction))%>%
ggplot()+
  geom_smooth(aes(x=iteration, y = folded_relative_pos, group = groupingVar, col = experiment),formula = y~0+x, method = "lm", se = F, linetype = 2, size = 0.5)+
  geom_smooth(data = exp2exp3_model_chain_data, aes(x= iteration, y= folded_relative_pos, col = experiment),formula = y~0+x, method = "lm", se=F)+
  geom_hline(yintercept = 0, color = "black")+
  theme_minimal()+
  scale_x_continuous(expand = c(0,0))
  #facet_grid(.~seed)

#relative_pos exp1&2&3
bind_rows(model_chain_data_exp1, model_chain_data_exp2, model_chain_data_exp3) %>%
  mutate(groupingVar = paste(seed, stim_direction,experiment))%>%
  mutate(stim_direction = factor(stim_direction))%>%
  ggplot(aes(x=iteration, y = relative_pos, col = stim_direction, group = groupingVar, linetype = experiment))+
  geom_smooth(formula = y~0+x, method = "lm", se = F)+
  facet_grid(.~seed)
