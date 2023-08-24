library(tidyverse)
source("helperFunctions.R")
test_data_exp1 = read_csv("data/exp1/test_data_exp1.csv")

filler_chain_data = test_data_exp1 %>% 
  filter(type == "filler")%>%
  group_by(subject, id )%>%
  arrange(currentBlock,currentTrial, iteration)%>%
  mutate(rejected = curError>22.5)%>%
  filter(!rejected) %>%
  mutate(iteration_true = iteration, iteration = 1:length(iteration))

filler_chain_data%>%
  filter(type == "filler") %>%
  group_by(id,subject)%>%
  summarise(maxiter = max(iteration)) %>%
  group_by(maxiter)%>%summarise(N=n()) %>%
  ggplot(aes(x = maxiter, y = N))+
  geom_histogram(stat = "identity")

filler_chains = filler_chain_data%>%
  filter(type == "filler") %>%
  group_by(id,subject)%>%
  summarise(maxiter = max(iteration)) %>%
  filter(maxiter>2) %>% select(id,subject)


filler_slopes = filler_chain_data %>%
  inner_join(filler_chains)%>%
  group_by(id, subject)%>%
  mutate(relative_pos = cumsum(error)) %>%
  ungroup()%>%
  select(subject,id, error, relative_pos, iteration) %>%
  group_by(subject,id) %>%
  do(slope = coef(lm(relative_pos~0+iteration, data=.))) %>%
  unnest(cols = slope) %>%
  rename(slopes = slope)


filler_slopes = filler_chain_data %>%
  group_by(subject,id) %>%
  distinct()%>%
  filter(iteration == 1)%>%
  summarise(col = imgNum) %>%
  inner_join(filler_slopes) %>%
  ungroup()


sliding_window <- function(data){ 
  window_size = 22.5
  window_step = 1
  window_max = 360 
  window_min = 0
  
  sliding_window<- data.frame(subject = c(),  error = c(), N= c(), dist= c())
  
  for(i in seq(window_min,window_max,window_step)){
    cur_min = i-window_size/2
    cur_max = i+window_size/2
    
    if(abs(cur_min) < window_min){
      cur_min = cur_min+360
      sliding_window <- data%>%
        filter(col >= cur_min) %>%
        bind_rows(data %>% filter(col < cur_max))%>%
        summarise(slope = mean(slopes), N=n())%>% 
        mutate(color = i)%>%
        bind_rows(sliding_window)
    }
    if(abs(cur_max) >= window_max){
      cur_max = cur_max-360
      sliding_window <- data %>%
        filter(col >= cur_min) %>%
        bind_rows(data %>% filter(col< cur_max))%>%
        summarise(slope = mean(slopes), N=n())%>% 
        mutate(color = i)%>%
        bind_rows(sliding_window)
    }
    else{
      sliding_window <- data %>%
        filter(col < cur_max, col >= cur_min) %>%
        summarise(slope = mean(slopes), N=n())%>% 
        mutate(color = i)%>%
        bind_rows(sliding_window)
    }
  }
  return(sliding_window)
}

tmp = sliding_window(filler_slopes) 

almostZero = function(x, limit){
  abs(x)<limit
}

zero_slopes = tmp[tmp$slope%>%almostZero(.,.1),]

  
tmp%>%
  mutate(near_zero = if_else(color %in% zero_slopes$color, "near zero", ""))%>%
  ggplot(aes(x=color, y = slope, col = near_zero))+
  geom_point()


zero_slopes

attractors = c(314,181,66)
boundaries = c(239,105,43)

filler_attractors = data.frame(crit_points = c(attractors, boundaries), type = c(rep("attractor",3), rep("boundary", 3)))


filler_slopes$colorVal = sapply(filler_slopes$col, convert360ToColorVal)

p1 = filler_slopes %>%
  ggplot()+
  geom_hline(yintercept = 0)+
  
  geom_point(aes(x=col, y = slopes,col =colorVal),size = .5, alpha = 0.1)+
  stat_summary(aes(x=col, y = slopes,col =colorVal), fun = mean, geom = "point", size = 1.5)+
  #geom_smooth(se = F, col = "black")+
  geom_line(data = tmp, aes(x=color, y = slope), size = 1)+
  geom_point(data = filler_attractors,
             aes(x=crit_points, fill = type), col = "black", pch = 21, y = 0, size = 6)+
  #geom_smooth(col = "black")+
  #stat_summary(data = slopes_exp1, aes(x=seed, y = slope, group = seed), fun = mean, geom = "point", shape = 4)+
  scale_color_identity()+
  scale_fill_manual(values = c("black", "white"))+
  theme_minimal()+
  #coord_cartesian(ylim = c(-25,25))+
  scale_x_continuous(expand = c(0,0),breaks = seq(0,360,30), labels = seq(0,360,30))+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 14),
        legend.position = "none")+
  labs(x = "color", y = "slope")

ggsave(plot = p1, 
       filename = "C:/Users/17742/OneDrive/Documents/GitHub/individual-serial-reproduction-color/Figures/filler_slopes.png", 
       width = 1400, height = 500, unit = "px",
       bg = "white")
