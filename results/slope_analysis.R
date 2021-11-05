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
  mutate(subject = factor(subject), seedID = as.character(seedID))%>%
  mutate(experiment = "exp1") 

slopes_exp1 = z_chain_data_exp1%>%
  select(subject,seedID, error, relative_pos, iteration, seed,experiment) %>% 
  group_by(subject, seedID, setID,seed,experiment) %>%
  do(slope = coef(lm(relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) 

sub_error_summary_exp2 = test_data_exp2 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

z_chain_data_exp2 = chain_data_exp2%>%
  inner_join(sub_error_summary_exp2)%>%
  group_by(subject)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error)) %>%
  mutate(subject = factor(subject), seedID = as.character(seedID))%>%
  mutate(experiment = "exp2") 

slopes_exp2= z_chain_data_exp2%>%
  select(subject,seedID, error, relative_pos, iteration, seed,experiment) %>% 
  group_by(subject, seedID, setID,seed,experiment) %>%
  do(slope = coef(lm(relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) 


sub_error_summary_exp3 = test_data_exp3 %>% 
  group_by(subject) %>%
  summarise(m = mean(error), s = sd(error))

z_chain_data_exp3 = chain_data_exp3%>%
  inner_join(sub_error_summary_exp3)%>%
  group_by(subject)%>%
  mutate(z_error = (m-error)/s ) %>%
  group_by(seedID,setID, subject)%>%
  mutate(z_relative_pos = cumsum(z_error))%>%
  mutate(subject = factor(subject), seedID = as.character(seedID))%>%
  mutate(experiment = "exp3")


slopes_exp3 = z_chain_data_exp3%>% 
  select(subject,seedID, error, relative_pos, iteration, seed, experiment) %>% 
  group_by(subject, seedID, setID, seed, experiment) %>%
  do(slope = coef(lm(relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) 

bind_rows(slopes_exp1, slopes_exp2, slopes_exp3)%>%
  filter(seed %in% c(18,90,162,234,306))%>%
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(seed = factor(seed))%>%
  ggplot(aes(x = slope, fill= seed))+
  geom_histogram(position = "identity", alpha = .3, binwidth = 0.5)+
  geom_vline(xintercept = 0)+
  facet_grid(experiment~seed)

bind_rows(slopes_exp1, slopes_exp2)%>%
  filter(seed %in% c(18,90,162,234,306))%>%
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(seed = factor(seed))%>%
  group_by(seed) %>%
  do(Fval = var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp2"])$statistic,
     df = var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp2"])$parameter,
     pval = var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp2"])$p.value,
     conf95 =var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp2"])$conf.int,
     estimate =var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp2"])$estimate ) %>%
  mutate(df1 = df[1], df2 = df[2], lower = conf95[1], upper = conf95[2])%>%
  select(-df, -conf95)%>%
  unnest()


bind_rows(slopes_exp1, slopes_exp3)%>%
  filter(seed %in% c(18,90,162,234,306))%>%
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(seed = factor(seed))%>%
  group_by(seed) %>%
  do(Fval = var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp3"])$statistic,
     df = var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp3"])$parameter,
     pval = var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp3"])$p.value,
     conf95 =var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp3"])$conf.int,
     estimate =var.test(.$slope[.$experiment == "exp1"], .$slope[.$experiment == "exp3"])$estimate ) %>%
  mutate(df1 = df[1], df2 = df[2], lower = conf95[1], upper = conf95[2])%>%
  select(-df, -conf95)%>%
  unnest()



bind_rows(z_chain_data_exp1, z_chain_data_exp2, z_chain_data_exp3) %>%
  filter(seed %in% c(18,90,162,234,306))%>%
  
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(group = paste(setID, experiment))%>%
  ggplot(aes(x=iteration, y = relative_pos, group = group, col =setID ))+
  geom_hline(yintercept = 0, col = "darkgrey")+
  geom_smooth( method = "glm", formula = y~0+x, se=F, size = 1.5, aes(linetype =experiment))+
  stat_summary(linetype = 1,  fun.data = mean_se, geom = "errorbar",size=1, width = .2 )+
  stat_summary(  fun = mean, geom = "point",size=3)+
  
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  scale_color_manual(values = c("#AD1457", "#00838F", "black"))+
  theme(panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20))+
  labs(y="Z-score Relative position", x="Iteration") #+
facet_grid(seed~.)


bind_rows(z_chain_data_exp1, z_chain_data_exp2, z_chain_data_exp3) %>%
  filter(seed %in% c(18,90,162,234,306))%>%
  
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(group = paste(setID, experiment))%>%
  ggplot(aes(x=iteration, y = z_relative_pos, group = group, col =setID ))+
  geom_hline(yintercept = 0, col = "darkgrey")+
  geom_smooth( method = "glm", formula = y~0+x, se=F, size = 1.5, aes(linetype =experiment))+
  stat_summary(linetype = 1,  fun.data = mean_se, geom = "errorbar",size=1, width = .2 )+
  stat_summary(  fun = mean, geom = "point",size=3)+
  
  scale_x_continuous(breaks = 1:15, labels = 1:15)+
  theme_minimal()+
  scale_color_manual(values = c("#AD1457", "#00838F", "black"))+
  theme(panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 18),
        axis.title.x = element_text(size = 20),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 20))+
  labs(y="Z-score Relative position", x="Iteration")# +
  facet_grid(seed~.)


lm_data_exp1_exp2_exp3 =   bind_rows(z_chain_data_exp1, z_chain_data_exp2, z_chain_data_exp3) %>%
  filter(seed %in% c(18,90,162,234,306))%>%
  
  mutate(setID = case_when(experiment == "exp1" ~ "exp1",
                           setID == 1 ~"+20",
                           setID == 2 ~ "-20"))%>%
  mutate(seed = factor(seed)) 
summary(lm(data = lm_data_exp1_exp2_exp3, formula = z_relative_pos~0+iteration+seed*experiment))
  
lm_data_exp2_exp3 =   bind_rows(z_chain_data_exp2, z_chain_data_exp3)%>% 
   mutate(seed = factor(seed)) 
summary(lm(data = lm_data_exp2_exp3, formula = z_relative_pos~0+iteration*setID+setID*experiment))
  