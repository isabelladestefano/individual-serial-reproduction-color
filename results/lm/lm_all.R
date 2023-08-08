model_chain_data_exp1  = chain_data_exp1%>%
  #filter(seed %in% c(18,90,162,234,306))%>%
  group_by(subject)%>%
  group_by(seedID,setID, subject)%>%
  mutate(folded_relative_pos = cumsum(error))%>%
  mutate(experiment = "exp1") %>%
  mutate(seedID = paste(seed))%>%
  ungroup()%>%
  select(iteration, relative_pos, folded_relative_pos, seed, experiment, seedID)


model_chain_data_exp2 = test_data_exp2%>%
  arrange(subject, currentBlock, currentTrial) %>%
  mutate(stim_direction = mapply(circDist,  lag(.$items),.$items)) %>%
  filter(type == "fixed")%>%
  filter(abs(error)<22.5) %>%
  group_by(subject, id , setID)%>%
  arrange(iteration)%>%
  mutate(iteration = 1:length(unique(iteration)))%>% 
  select(stim_direction, subject, id, iteration) %>%
  inner_join(chain_data_exp2,.)

model_chain_data_exp2 = model_chain_data_exp2%>%
  group_by(subject)%>%
  mutate(serialSign = if_else(stim_direction>0, 1,-1))%>%
  mutate(error = error*serialSign)%>%
  group_by(seedID,setID, subject)%>%
  mutate(folded_relative_pos = cumsum(error))%>%
  mutate(experiment = "exp2")%>%
  mutate(seedID = paste(seed, " ", stim_direction))%>%
  ungroup()%>%
  select(iteration, relative_pos, folded_relative_pos,  seed, experiment, seedID, stim_direction)  

model_chain_data_exp3 = chain_data_exp3
model_chain_data_exp3$stim_direction = mapply(circDist,  model_chain_data_exp3$anchor,model_chain_data_exp3$items)
model_chain_data_exp3 = model_chain_data_exp3 %>%
  group_by(subject)%>%
  mutate(serialSign = if_else(stim_direction>0 , 1, -1))%>%
  mutate(flipped_error = error*serialSign)%>%
  group_by(seed,stim_direction, subject)%>%
  mutate(folded_relative_pos = cumsum(flipped_error))%>%
  mutate(seedID = paste(seed, " ", stim_direction))%>%
  mutate(experiment = "exp3") %>%
  select(iteration, relative_pos, folded_relative_pos,  seed, experiment, seedID, stim_direction)  


model_chain_data_exp3 %>% group_by(seed, stim_direction) %>% summarise(N=n())
#exp 1
model_null_exp1 <- lm(data = model_chain_data_exp1, formula = relative_pos~0+iteration)
model_exp1 <- lm(data = model_chain_data_exp1, formula = relative_pos ~ 0+ iteration + iteration:seed)
anova(model_exp1)

model_exp1_seed <- lm(data = model_chain_data_exp1 %>% mutate(seed = factor(seed)), formula = relative_pos ~ 0+iteration:seed)
lm_exp1 = summary(model_exp1_seed)

t_dif_seed = data.frame(seed1 = c() , seed2=c(), t =c(), df = c(), p = c() )
for(i in 1:10){
  for(j in 1:10){
    if(i>j){
      df_dif =sqrt((lm_exp1$coefficients[i,2])^2 + (lm_exp1$coefficients[j,2])^2) 
      t_dif = (lm_exp1$coefficients[i,3] - lm_exp1$coefficients[j,3])/df_dif
      p_dif = dt(t_dif, df_dif)
      t_dif_seed = bind_rows(t_dif_seed, data.frame(seed1 = i, seed2=j, t= t_dif, df = df_dif, p = p_dif))
    }
  }
}
t_dif_seed %>%
  filter(p<0.05)



#experiment 2 and 3

exp2exp3_model_chain_data = bind_rows( model_chain_data_exp2, model_chain_data_exp3)
model_null_exp2exp3 <- lm(data = exp2exp3_model_chain_data, formula = folded_relative_pos ~ iteration)

model_all_exp2exp3 <- lm(data = exp2exp3_model_chain_data, formula = folded_relative_pos ~ iteration:experiment +  iteration:seedID:experiment)
summary(model_all_exp2exp3)
anova(model_all_exp2exp3)

