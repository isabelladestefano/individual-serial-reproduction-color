require(tidyverse)
require(lme4)
chain_data_exp1 <- read_csv("data/exp1/chain_data_exp1.csv")
chain_data_exp2 <- read_csv("data/exp2/chain_data_exp2.csv")
chain_data_exp3 <- read_csv("data/exp3/chain_data_exp3.csv")

test_data_exp1 <- read_csv("data/exp1/test_data_exp1.csv")
test_data_exp2 <- read_csv("data/exp2/test_data_exp2.csv")
test_data_exp3 <- read_csv("data/exp3/test_data_exp3.csv")


sub_error_summary_exp1 = test_data_exp1 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

z_chain_data_exp1  = chain_data_exp1%>%
  inner_join(sub_error_summary_exp1)%>%
  group_by(subject)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error))%>%
  mutate(subject = factor(subject), seedID = as.character(seedID))
  #filter(seed %in% seeds_exp3$seed)
 
slopes_exp1 = z_chain_data_exp1%>%
  select(subject,seedID, z_error, z_relative_pos, iteration, seed) %>% 
  group_by(subject, seedID, setID,seed) %>%
  do(slope = coef(lm(z_relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) %>%
  mutate(experiment = "exp1") 


sub_error_summary_exp2 = test_data_exp2 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

z_chain_data_exp2 = chain_data_exp2%>%
  inner_join(sub_error_summary_exp2)%>%
  group_by(subject)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error)) %>%
  mutate(subject = factor(subject), seedID = as.character(seedID))
  #filter(seed %in% seeds_exp3$seed)

slopes_exp2= z_chain_data_exp2%>%
  select(subject,seedID, z_error, z_relative_pos, iteration, seed) %>% 
  group_by(subject, seedID, setID,seed) %>%
  do(slope = coef(lm(z_relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) %>%
mutate(experiment = "exp2") 


sub_error_summary_exp3 = test_data_exp3 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

z_chain_data_exp3 = chain_data_exp3%>%
  inner_join(sub_error_summary_exp3)%>%
  group_by(subject)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error))%>%
  mutate(subject = factor(subject), seedID = as.character(seedID))
  #filter(seed %in% seeds_exp3$seed)


slopes_exp3 = z_chain_data_exp3%>% 
  select(subject,seedID, z_error, z_relative_pos, iteration, seed) %>% 
  group_by(subject, seedID, setID, seed) %>%
  do(slope = coef(lm(z_relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) %>%
  mutate(experiment = "exp3")

bind_rows(slopes_exp1, slopes_exp2, slopes_exp3)%>%
  filter(seed %in% c(18,90,162,234,306))%>%
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(seed = factor(seed))%>%
  ggplot(aes(x = slope, fill= setID))+
  geom_histogram(position = "identity", alpha = .3, binwidth = 0.05)+
  geom_vline(xintercept = 0)+
  facet_grid(experiment~seed)



slopes_exp2


ggplot(z_chain_data_exp1)+
  stst_summary()


bind_rows(z_chain_data_exp1, z_chain_data_exp2, z_chain_data_exp3) %>%
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(group = paste(setID, experiment))%>%
  ggplot(aes(x=iteration, y = z_relative_pos, group = group, col =setID ))+
  geom_hline(yintercept = 0, col = "darkgrey")+
  geom_smooth( method = "glm", formula = y~0+x, se=F, size = .6, aes(linetype =experiment))+
  stat_summary(linetype = 1,  fun.data = mean_se, geom = "errorbar",size=.5, width = .2 )+
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  scale_color_manual(values = c("#AD1457", "#00838F", "black"))+
  theme(panel.grid.minor.x = element_blank())+
  labs(y="Relative position", x="Iteration") +
  facet_grid(seed~.)



  

