require(tidyverse)
source("results/nearest-neighbor/nearestNeighborFunctions.R")


chain_data_exp1 <- read_csv("data/exp1/chain_data_exp1.csv")
chain_data_exp2 <- read_csv("data/exp2/chain_data_exp2.csv")

NNunifSamp_exp1 = read_csv(file ="results/nearest-neighbor/null-hypotheses/NNunifSamp_exp1.csv" )
NNshuffleSamp_exp1 = read_csv( file ="results/nearest-neighbor/null-hypotheses/NNshuffleSamp_exp1.csv" )

NNunifSamp_exp2 = read_csv(file ="results/nearest-neighbor/null-hypotheses/NNunifSamp_exp2.csv" )
NNshuffleSamp_exp2 = read_csv( file ="results/nearest-neighbor/null-hypotheses/NNshuffleSamp_exp2.csv" )

##exp1
nearestNeighbor_exp1 = chain_data_exp1 %>% 
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID))

nearestNeighbor_exp1 %>%
  ggplot(aes(x= iteration, y = NN))+
  stat_summary(fun.data = mean_se, geom="errorbar")


NNunifSamp_exp1 %>% group_by(samp) %>% summarise(n())

NNunif_exp1 = NNunifSamp_exp1 %>%
  group_by(subject, iteration,setID) %>% 
  summarise(NN_unif = mean(NN_unif))




NNshuffle_exp1 = NNshuffleSamp_exp1 %>% 
  group_by(subject, iteration,setID) %>% 
  summarise(NN_shuffle = mean(NN_shuffle))


###figure
all_nn_exp1 = inner_join(nearestNeighbor_exp1,NNunif_exp1) %>%
  inner_join(NNshuffle_exp1)%>%
  mutate(`original` = NN/NN_unif, `shuffled` = NN_shuffle/NN_unif, `uniform` = NN_unif/NN_unif)%>%
  select(-NN,-NN_unif, -NN_shuffle)%>%
  gather(key = "type", value = "NN", -subject, - iteration, -setID) #%>% 
  #mutate(iteration = .bincode(iteration, breaks = seq(1,16, 3), include.lowest = T))

all_nn_exp1_summary = all_nn_exp1 %>% filter(type %in% c( "original")) %>% group_by(subject, iteration, setID,type) %>% summarise(NN = mean(NN))
all_nn_exp1 %>%
  ggplot(aes(x=iteration, y =NN, col = type))+
  geom_hline(yintercept = 1, linetype = 2, color = "black", size =1)+
  stat_summary(data = all_nn_exp1 %>% filter(type %in% c( "original")),fun = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp1 %>% filter(type %in% c(  "shuffled")),fun = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp1 %>% filter(type %in% c( "original", "shuffled")), fun.data = mean_se, geom = "errorbar",width=0.5, size = 1.25)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),legend.text	
=element_text(size = 16))+
  #ylim(.5,1.5)+
  labs(y = "", x = "", col = "")+
  scale_color_manual(values = c( "dimgrey","darkgrey", "#37474F"))



nn_lm = lm(all_nn_exp1 %>% filter(type == "original"), formula =  NN ~ iteration )
nn_shuffle_lm =lm(all_nn_exp1 %>% filter(type == "shuffled"), formula =  NN_shuffle ~ iteration )

all_nn_exp1 %>% 
  filter(type == "original", iteration == 15) %>%
  pull(NN) %>%
  t.test(.,mu=1)

all_nn_exp1 %>% 
  filter(type == "shuffled", iteration == 15) %>%
  pull(NN) %>%
  t.test(.,mu=1)

anova(nn_lm)
summary(nn_lm)

coef(nn_lm)
temp = anova(nn_lm)
temp$`Sum Sq`



##exp2


nearestNeighbor_exp2 = chain_data_exp2 %>% 
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID))

nearestNeighbor_exp2 %>%
  ggplot(aes(x= iteration, y = NN))+
  stat_summary(fun.data = mean_se, geom="errorbar")


#write.csv(NNunifSamp_exp2, file ="NNunifSamp_exp2.csv" )
NNunifSamp_exp2 %>% group_by(samp) %>% summarise(n()) %>% nrow

NNunif_exp2 = NNunifSamp_exp2 %>%
  group_by(subject, iteration,setID) %>% 
  summarise(NN_unif = mean(NN_unif))

#get shuffled sample
#shuffleSamp_exp2 = replicate(B,nearestNeighborShuffleFunc(chain_data_exp2))    

NNshuffleSamp_exp2 %>% group_by(samp) %>% summarise(n()) %>% nrow


NNshuffle_exp2 = NNshuffleSamp_exp2 %>% 
  group_by(subject, iteration,setID) %>% 
  summarise(NN_shuffle = mean(NN_shuffle))

#write.csv(NNshuffleSamp_exp2, file ="NNshuffleSamp_exp2.csv" )



###figure
all_nn_exp2 = inner_join(nearestNeighbor_exp2,NNunif_exp2) %>%
  inner_join(NNshuffle_exp2)%>%
  #inner_join(nearestNeighbor_exp1_compare)%>%
  mutate(`experiment 2` = NN/NN_unif,  `uniform` = NN_unif/NN_unif, `shuffled` = NN_shuffle/NN_unif)%>%
  select(-NN,-NN_unif)%>%
  gather(key = "type", value = "NN", -subject, - iteration, -setID)

all_nn_exp2 %>%
  ggplot(aes(x=iteration, y =NN, col = type))+
  geom_hline(yintercept = 1, linetype = 2, color = "black", size =1)+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "experiment 2", "experiment 1")),fun.y = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c(  "shuffled")),fun.y = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "experiment 2", "shuffled", "experiment 1")), fun.data = mean_se, geom = "errorbar",width=0.5, size = 1.25)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),legend.text	
        =element_text(size = 16))+
  #ylim(.5,1.5)+
  labs(y = "", x = "", col = "")+
  scale_color_manual(values = c( "dimgrey","darkgrey", "#37474F"))


all_nn_exp2 = inner_join(nearestNeighbor_exp2,NNunif_exp2) %>%
  #inner_join(NNshuffle_exp2)%>%
  inner_join(nearestNeighbor_exp1_compare)%>%
  mutate(`original` = NN/NN_unif, `exp1` = NN_exp1/NN_unif, `uniform` = NN_unif/NN_unif)%>%
  select(-NN,-NN_unif, -NN_exp1)%>%
  gather(key = "type", value = "NN", -subject, - iteration, -setID) %>%
  mutate(`influence trial` = as.character(if_else(setID == 1, -20,20)))
  

all_nn_exp2 %>%  ggplot(aes(x=iteration, y =NN))+
  geom_jitter(data = all_nn_exp2 %>% filter(type %in% c( "original")), alpha = .1, size =.5, aes(col = `influence trial` ))+
  #geom_line(aes(y =m_NN, col = type))+
  #geom_errorbar(aes(ymin = m_NN-2*se_NN, ymax = m_NN + 2*se_NN))+
  geom_smooth(data = all_nn_exp2 %>% filter(type %in% c( "original")), method = "glm", se=F, size = .6,aes(col = `influence trial` ))+
  geom_smooth(data = all_nn_exp2 %>% filter(type %in% c( "exp1")), method = "glm", se=F, size = .6, col = "black", linetype = 2)+  
  geom_smooth(data = all_nn_exp2 %>% filter(type %in% c( "uniform")), method = "glm", se=F, size = .6, col ="#37474F" )+  
    stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "original")),fun = mean,geom = "point", size = 1 ,aes(col = `influence trial` ))+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "original")), fun.data = mean_se, geom = "errorbar",width=0.5,size=.5, aes(col = `influence trial` ))+
  
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "exp1")),fun = mean,geom = "point", size = 1 ,col = "black")+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "exp1")), fun.data = mean_se, geom = "errorbar",width=0.5,size=.5, col = "black", linetype = 1)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank())+
  ylim(.5,1.75)+
  labs(y = "NN Ratio", x = "Iteration", col = "")+
  scale_color_manual(values = c("#AD1457", "#00838F", "#37474F"))


#exp3
NNunifSamp_exp3 = read_csv(file ="results/nearest-neighbor/null-hypotheses/NNunifSamp_exp3.csv" )
NNshuffleSamp_exp3 = read_csv( file ="results/nearest-neighbor/null-hypotheses/NNshuffleSamp_exp3.csv" )

nearestNeighbor_exp3 = chain_data_exp3 %>% 
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID))



#write.csv(NNunifSamp_exp3, file ="NNunifSamp_exp3.csv" )
NNunifSamp_exp3 %>% group_by(samp) %>% summarise(n()) %>% nrow

NNunif_exp3 = NNunifSamp_exp3 %>%
  group_by(subject, iteration,setID) %>% 
  summarise(NN_unif = mean(NN_unif))

#get shuffled sample
#shuffleSamp_exp3 = replicate(B,nearestNeighborShuffleFunc(chain_data_exp3))    

NNshuffleSamp_exp3 %>% group_by(samp) %>% summarise(n()) %>% nrow


NNshuffle_exp3 = NNshuffleSamp_exp3 %>% 
  group_by(subject, iteration,setID) %>% 
  summarise(NN_shuffle = mean(NN_shuffle))

#write.csv(NNshuffleSamp_exp3, file ="NNshuffleSamp_exp3.csv" )



###figure
all_nn_exp3 = inner_join(nearestNeighbor_exp3,NNunif_exp3) %>%
  inner_join(NNshuffle_exp3)%>%
  #inner_join(nearestNeighbor_exp1_compare)%>%
  mutate(`experiment 3` = NN/NN_unif,  `uniform` = NN_unif/NN_unif, `shuffled` = NN_shuffle/NN_unif)%>%
  select(-NN,-NN_unif)%>%
  gather(key = "type", value = "NN", -subject, - iteration, -setID) 
  

all_nn_exp3 %>%
  ggplot(aes(x=iteration, y =NN, col = type))+
  geom_hline(yintercept = 1, linetype = 2, color = "black", size =1)+
  stat_summary(data = all_nn_exp3 %>% filter(type %in% c( "experiment 3", "experiment 1")),fun.y = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp3 %>% filter(type %in% c(  "shuffled")),fun.y = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp3 %>% filter(type %in% c( "experiment 3", "shuffled", "experiment 1")), fun.data = mean_se, geom = "errorbar",width=0.5, size = 1.25)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),legend.text	
        =element_text(size = 16))+
  #ylim(.5,1.5)+
  labs(y = "", x = "", col = "")+
  scale_color_manual(values = c( "dimgrey","darkgrey", "#37474F"))

#exp1_compare

NNunifSamp_exp3 = read_csv(file ="results/nearest-neighbor/null-hypotheses/NNunifSamp_exp1_compare.csv" )
NNshuffleSamp_exp3 = read_csv( file ="results/nearest-neighbor/null-hypotheses/NNshuffleSamp_exp1_compare.csv" )

nearestNeighbor_exp1_compare = chain_data_exp1_compare %>% 
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID))

nearestNeighbor_exp1_compare %>%
  ggplot(aes(x= iteration, y = NN))+
  stat_summary(fun.data = mean_se, geom="errorbar")


NNunifSamp_exp1_compare %>% group_by(samp) %>% summarise(n())

NNunif_exp1_compare = NNunifSamp_exp1_compare %>%
  group_by(subject, iteration,setID) %>% 
  summarise(NN_unif = mean(NN_unif))




NNshuffle_exp1_compare = NNshuffleSamp_exp1_compare %>% 
  group_by(subject, iteration,setID) %>% 
  summarise(NN_shuffle = mean(NN_shuffle))


###figure
all_nn_exp1_compare = inner_join(nearestNeighbor_exp1_compare,NNunif_exp1_compare) %>%
  inner_join(NNshuffle_exp1_compare)%>%
  mutate(`original` = NN/NN_unif, `shuffled` = NN_shuffle/NN_unif, `uniform` = NN_unif/NN_unif)%>%
  select(-NN,-NN_unif, -NN_shuffle)%>%
  gather(key = "type", value = "NN", -subject, - iteration, -setID) #%>% 
#mutate(iteration = .bincode(iteration, breaks = seq(1,16, 3), include.lowest = T))

all_nn_exp1_compare_summary = all_nn_exp1_compare %>% filter(type %in% c( "original")) %>% group_by(subject, iteration, setID,type) %>% summarise(NN = mean(NN))
all_nn_exp1_compare %>%
  ggplot(aes(x=iteration, y =NN, col = type))+
  geom_hline(yintercept = 1, linetype = 2, color = "black", size =1)+
  stat_summary(data = all_nn_exp1_compare %>% filter(type %in% c( "original")),fun = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp1_compare %>% filter(type %in% c(  "shuffled")),fun = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp1_compare %>% filter(type %in% c( "original", "shuffled")), fun.data = mean_se, geom = "errorbar",width=0.5, size = 1.25)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),legend.text	
        =element_text(size = 16))+
  #ylim(.5,1.5)+
  labs(y = "", x = "", col = "")+
  scale_color_manual(values = c( "dimgrey","darkgrey", "#37474F"))














all_nn_exp2 = inner_join(nearestNeighbor_exp2,NNunif_exp2) %>%
  #inner_join(NNshuffle_exp2)%>%
  inner_join(nearestNeighbor_exp1_compare)%>%
  mutate(`original` = NN/NN_unif, `exp1` = NN_exp1/NN_unif, `uniform` = NN_unif/NN_unif)%>%
  select(-NN,-NN_unif, -NN_exp1)%>%
  gather(key = "type", value = "NN", -subject, - iteration, -setID) %>%
  mutate(`influence trial` = as.character(if_else(setID == 1, -20,20)))
  

all_nn_exp2 %>%  ggplot(aes(x=iteration, y =NN))+
  geom_jitter(data = all_nn_exp2 %>% filter(type %in% c( "original")), alpha = .1, size =.5, aes(col = `influence trial` ))+
  #geom_line(aes(y =m_NN, col = type))+
  #geom_errorbar(aes(ymin = m_NN-2*se_NN, ymax = m_NN + 2*se_NN))+
  geom_smooth(data = all_nn_exp2 %>% filter(type %in% c( "original")), method = "glm", se=F, size = .6,aes(col = `influence trial` ))+
  geom_smooth(data = all_nn_exp2 %>% filter(type %in% c( "exp1")), method = "glm", se=F, size = .6, col = "black", linetype = 2)+  
  geom_smooth(data = all_nn_exp2 %>% filter(type %in% c( "uniform")), method = "glm", se=F, size = .6, col ="#37474F" )+  
    stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "original")),fun.y = mean,geom = "point", size = 1 ,aes(col = `influence trial` ))+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "original")), fun.data = mean_se, geom = "errorbar",width=0.5,size=.5, aes(col = `influence trial` ))+
  
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "exp1")),fun.y = mean,geom = "point", size = 1 ,col = "black")+
  stat_summary(data = all_nn_exp2 %>% filter(type %in% c( "exp1")), fun.data = mean_se, geom = "errorbar",width=0.5,size=.5, col = "black", linetype = 1)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank())+
  ylim(.5,1.75)+
  labs(y = "NN Ratio", x = "Iteration", col = "")+
  scale_color_manual(values = c("#AD1457", "#00838F", "#37474F"))

#all


nearestNeighbor_exp1 = chain_data_exp1 %>% 
  filter(seed %in% seed_colors$imgNum)%>%
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID))%>%
  mutate(experiment = "1")

nearestNeighbor_exp2 = chain_data_exp2 %>% 
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID))%>%
  mutate(experiment = "2")

nearestNeighbor_exp3 = chain_data_exp3 %>% 
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN=mean(NN))%>%
  mutate(setID = as.numeric(setID)) %>%
  mutate(experiment = "3")

bind_rows(nearestNeighbor_exp1, nearestNeighbor_exp2,nearestNeighbor_exp3) %>%
  ggplot(aes(x= iteration, y = NN, group = experiment, col = experiment))+
  stat_summary(fun.data = function(x){mean_se(x,mult = 1.98)}, geom="errorbar", width = .3)+
  stat_summary(fun = mean, geom= "point")+
  theme_minimal()



