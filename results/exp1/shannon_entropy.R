library(tidyverse)
test_data_exp1 = read_csv("data/exp1/test_data_exp1.csv")
chain_data_exp1 = read_csv("data/exp1/chain_data_exp1.csv")


binsize = 10

test_data_exp1 = read_csv("data/exp1/test_data_exp1.csv")
resp_count_exp1 = test_data_exp1 %>% 
  group_by(subject)%>%
  summarise(num_resp = n())

prob_resp_exp1 = test_data_exp1 %>% 
  mutate(imgNum = imgNum%%360)%>%
  mutate(imgNum = .bincode(imgNum, breaks = seq(0,360,10), include.lowest = T))%>%
  group_by(subject, imgNum)%>%
  summarise(p = n())%>%
  inner_join(resp_count_exp1)%>%
  mutate(p=p/num_resp)



prob_targ_exp1 = test_data_exp1 %>% 
  mutate(items = items%% 360)%>%
  mutate(items = .bincode(items, breaks = seq(0,360,10), include.lowest = T))%>%
  group_by(subject, items)%>%
  summarise(p = n())%>%
  inner_join(resp_count_exp1)%>%
  mutate(p=p/num_resp)


prob_resp_exp1 %>% group_by(subject) %>% summarise(N=n()) %>% filter(N<36)
prob_targ_exp1 %>% group_by(subject) %>% summarise(N=n()) %>% filter(N<36)

entropy_resp_exp1 = prob_resp_exp1 %>% 
  group_by(subject)%>%
  summarise(entropy = sum(p*log2(1/p)))

entropy_targ_exp1 = prob_targ_exp1 %>% 
  group_by(subject)%>%
  summarise(entropy = sum(p*log2(1/p)))

t.test(entropy_resp_exp1$entropy,entropy_targ_exp1$entropy, paired =T)


#across iteration


resp_count_exp1 = chain_data_exp1 %>% 
  group_by(subject, iteration)%>%
  summarise(num_resp = n())

prob_resp_exp1 = chain_data_exp1 %>% 
  mutate(imgNum = imgNum%%360)%>%
  mutate(imgNum = .bincode(imgNum, breaks = seq(0,360,binsize), include.lowest = T))%>%
  group_by(subject, imgNum, iteration)%>%
  summarise(p = n())%>%
  inner_join(resp_count_exp1)%>%
  mutate(p=p/num_resp)

entropy_resp_exp1 = prob_resp_exp1 %>% 
  group_by(subject, iteration)%>%
  summarise(entropy = sum(p*log2(1/p))) %>%
  mutate(experiment = 1)


entropy_resp_exp1 %>%
  ggplot(aes(x=iteration, y= entropy))+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  theme_minimal()

entropyUniformFunc = function(df){
  unifSample = df %>%
    group_by(iteration,subject)%>%
    mutate(imgNum = runif(length(imgNum),0,359))
  
  unif_resp_count = unifSample %>% 
    group_by(subject, iteration)%>%
    summarise(num_resp = n())
  
  unif_prob_resp = unifSample %>% 
    mutate(imgNum = imgNum%%359)%>%
    mutate(imgNum = .bincode(imgNum, breaks = seq(0,360,binsize), include.lowest = T))%>%
    group_by(subject, imgNum, iteration)%>%
    summarise(p = n())%>%
    inner_join(unif_resp_count)%>%
    mutate(p=p/num_resp)
  
  unif_entropy_resp = unif_prob_resp %>% 
    group_by(subject, iteration)%>%
    summarise(entropy = sum(p*log2(1/p))) %>%
    mutate(experiment = 1)
}
UnifSamp_exp1 = replicate(100,entropyUniformFunc(chain_data_exp1))

for( i in 1:ncol(UnifSamp_exp1)){
  if(i%%100 == 0){
    print(i)
  }
  if(i == 1){
    entropyUnifSamp_exp1 = data.frame(UnifSamp_exp1[,i])
    entropyUnifSamp_exp1$samp = rep(i, nrow(entropyUnifSamp_exp1))
  }
  else{
    TempentropyUnifSamp = data.frame(UnifSamp_exp1[,i])
    TempentropyUnifSamp$samp = rep(i, nrow(TempentropyUnifSamp))
    entropyUnifSamp_exp1 = rbind(entropyUnifSamp_exp1, TempentropyUnifSamp)
  }
}

entropyUnifSamp_exp1 %>%
  filter(iteration == 15)%>%
  group_by(samp)%>%
  summarise(mentropy = mean(entropy))%>%
  ggplot(aes(x=mentropy))+
  geom_histogram()+
  geom_histogram(data = entropy_resp_exp1 %>% 
                   ungroup()%>%
                   filter(iteration == 15)%>%
                   summarise(mentropy = mean(entropy)),
                 fill = "red")




entropyUnifSamp_exp1 %>% 
  ggplot (aes(x = iteration, y = entropy))+
  stat_summary(fun.data = mean_se, geom = "errorbar")+
  stat_summary(data = entropy_resp_exp1, fun.data = mean_se, geom = "errorbar")+
  
  theme_minimal()
#analysis
model = lm(data = entropy_resp_exp1, formula = entropy~iteration)
null_model =lm(data = entropy_resp_exp1, formula = entropy~1)
summary(model) 
coef(model)

summary(model)
n = nrow(entropy_resp_exp1)
df = n-2

iteration_dif_unif = entropy_resp_exp1 %>%
  group_by(iteration)%>%
  do(tests = t.test(.$entropy, entropyUnifSamp_exp1$entropy,alternative = "less" ))

iteration_dif_unif$tests[15]
