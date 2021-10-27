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
  stat_summary(data = all_nn_exp1 %>% filter(type %in% c( "original")),fun.y = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp1 %>% filter(type %in% c(  "shuffled")),fun.y = mean,geom = "point", size = 3 )+
  stat_summary(data = all_nn_exp1 %>% filter(type %in% c( "original", "shuffled")), fun.data = mean_se, geom = "errorbar",width=0.5, size = 1.25)+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),legend.text	
=element_text(size = 16))+
  #ylim(.5,1.5)+
  labs(y = "", x = "", col = "")+
  scale_color_manual(values = c( "dimgrey","darkgrey", "#37474F"))



nn_lm = lm(nearestNeighbor_exp1, formula =  NN ~ iteration )
nn_unif_lm = lm(NNunif_exp1, formula =  NN_unif ~ iteration )
nn_shuffle_lm =lm(NNshuffle_exp1, formula =  NN_shuffle ~ iteration )

anova(nn_lm)

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

nearestNeighbor_exp1_compare = chain_data_exp1 %>%
  filter(seed %in% seeds_exp2$seed)%>%
  ungroup()%>%
  group_by(iteration,subject, setID)%>%
  do(NN_exp1 = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
  mutate(NN_exp1=mean(NN_exp1))%>%
  mutate(setID = as.numeric(setID))


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

