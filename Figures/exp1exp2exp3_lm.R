
lm_exp1 = lm(model_chain_data_exp1 %>% mutate(seed = factor(seed)) %>% filter(seed %in% seeds_exp2$seed), formula = relative_pos~0+iteration:seed)
model_exp1 = summary(lm_exp1)

coefs_exp1 = data.frame( seed = coef(model_exp1) %>% rownames %>% str_split("seed") %>% data.frame() %>% .[2,] %>% unlist(),
                         slope = coef(model_exp1)[,1],
                         experiment = "Experiment 1")%>%
  mutate(
    intercept = as.numeric(seed))
rownames(coefs_exp1) = NULL


lm_exp2 = lm(model_chain_data_exp2 %>% mutate(seed = factor(seed), stim_direction = factor(stim_direction)), formula = relative_pos~0+iteration:seed:stim_direction)
model_exp2 = summary(lm_exp2)

coefs_exp2 = data.frame( seed = coef(model_exp2) %>% rownames %>% str_split("seed") %>% data.frame() %>% .[2,] %>% unlist() %>% str_split(":") %>% data.frame() %>%.[1,] %>% unlist(),
                         direction = coef(model_exp2) %>% rownames %>% str_split("seed") %>% data.frame() %>% .[2,] %>% unlist() %>% str_split("stim_direction") %>% data.frame() %>% .[2,] %>% unlist(),
                         slope = coef(model_exp2)[,1],
                         experiment = "Experiment 2")%>%
  mutate(
    intercept = as.numeric(seed))
rownames(coefs_exp2) = NULL

lm_exp3 = lm(model_chain_data_exp3 %>% mutate(seed = factor(seed), stim_direction = factor(stim_direction)), formula = relative_pos~0+iteration:seed:stim_direction)
model_exp3 = summary(lm_exp3)

coefs_exp3 = data.frame( seed = coef(model_exp3) %>% rownames %>% str_split("seed") %>% data.frame() %>% .[2,] %>% unlist() %>% str_split(":") %>% data.frame() %>%.[1,] %>% unlist(),
                         direction = coef(model_exp3) %>% rownames %>% str_split("seed") %>% data.frame() %>% .[2,] %>% unlist() %>% str_split("stim_direction") %>% data.frame() %>% .[2,] %>% unlist(),
                         slope = coef(model_exp3)[,1],
                         experiment = "Experiment 3")%>%
  mutate(
    intercept = as.numeric(seed))
rownames(coefs_exp3) = NULL

segment_exp1 = coefs_exp1 %>%
  mutate(y1 = as.numeric(seed), x1 = 0, x2 = 15, y2 = slope*15+y1)

slope = segment_exp1 %>% filter(seed == "18") %>% pull(slope)
end_seg = -18/slope

seg_1_18 = segment_exp1 %>% filter(seed == "18") %>%
  mutate(x2 = end_seg, y2 = end_seg*slope+18)

seg_2_18 = segment_exp1 %>% filter(seed == "18") %>%
  mutate(x1 = end_seg, y1 = 360, x2 = 15, y2 = 15*slope+18 + 360)
segment_exp1 = segment_exp1[-1,] %>% bind_rows(.,seg_1_18, seg_2_18)


segment_exp2 = coefs_exp2 %>%
  mutate(y1 = as.numeric(seed), x1 = 0, x2 = 15, y2 = slope*15+y1)
slope = segment_exp2 %>% filter(seed == "18", direction == -20) %>% pull(slope)
end_seg = -18/slope
slope*end_seg+18

seg_1_18_20 = segment_exp2 %>% filter(seed == "18", direction == -20) %>%
  mutate(x2 = end_seg, y2 = end_seg*slope+18)

seg_2_18_20 = segment_exp2 %>% filter(seed == "18", direction == -20) %>%
  mutate(x1 = end_seg, y1 = 360, x2 = 15, y2 = 15*slope+18 + 360)

segment_exp2$seedID = paste0(segment_exp2$seed, segment_exp2$direction)
segment_exp2 = segment_exp2 %>% filter(seedID !="18-20") %>% bind_rows(.,seg_1_18_20, seg_2_18_20)


#segment_exp2_1 = coefs_exp2 %>%
#  mutate(y1 = as.numeric(seed), x1 = 0, x2 = 15, y2 = slope*15+y1)
#slope = segment_exp2_1 %>% filter(seed == "18", direction == 20) %>% pull(slope)
#end_seg = -18/slope #y=mx+b

#seg_1_18_20 = segment_exp2_1 %>% filter(seed == "18", direction == 20) %>%
# mutate(x2 = end_seg, y2 = end_seg*slope+18) 

#seg_2_18_20 = segment_exp2_1 %>% filter(seed == "18", direction == 20) %>%
#  mutate(x1 = end_seg, y1 = 360, x2 = 15, y2 = 15*slope+18 + 360)

#segment_exp2_1 = segment_exp2_1[-6,] %>% bind_rows(.,seg_1_18_20, seg_2_18_20)


segment_exp3 = coefs_exp3 %>%
  mutate(y1 = as.numeric(seed), x1 = 0, x2 = 15, y2 = slope*15+y1)
slope = segment_exp3 %>% filter(seed == "18", direction == 20) %>% pull(slope)
end_seg = -18/slope

seg_1_18_20 = segment_exp3 %>% filter(seed == "18", direction == 20) %>%
  mutate(x2 = end_seg, y2 = end_seg*slope+18)

seg_2_18_20 = segment_exp3 %>% filter(seed == "18", direction == 20) %>%
  mutate(x1 = end_seg, y1 = 360, x2 = 15, y2 = 15*slope+18 + 360)

segment_exp3$seedID = paste0(segment_exp3$seed, segment_exp3$direction)
segment_exp3 = segment_exp3 %>% filter(seedID !="1820") %>% bind_rows(.,seg_1_18_20, seg_2_18_20)


segment_exp1 = segment_exp1 %>% mutate(direction = " ")

colorbar = test_data_exp1 %>% group_by(items) %>% select(colorValTarget, items) %>% distinct
#anchor_points = data.frame(x = c(seeds_exp2$seed,(seeds_exp2$seed - 20 + 360)%%360, 
#           (seeds_exp2$seed + 20 + 360)%%360),
#           color = c(rep("black", 5),rep("#40b9c7",5),rep("#d93909",5)))
anchor_points = data.frame(x = c(seeds_exp2$seed), color = rep("black", 5))

segment_anchor_points = data.frame(start = c((seeds_exp2$seed - 20 + 360)%%360, seeds_exp2$seed),
                                   end = c(seeds_exp2$seed, (seeds_exp2$seed + 20 + 360)%%360),
                                   col = c(rep("#40b9c7", 5), rep("#d93909", 5)))
segment_18 = data.frame(start = c(0, 358), end = c(18,360), col = "#40b9c7")
segment_18

segment_anchor_points = segment_anchor_points[-1,] %>% bind_rows(segment_18)

bind_rows(segment_exp1, segment_exp2, segment_exp3)%>%
  mutate(influence_dir = direction)%>%
  mutate(direction = case_when(direction ==-20 ~ "#40b9c7", 
                               direction == 20 ~ "#d93909",
                               direction == " " ~ "black"))%>%
  ggplot()+
  geom_segment( aes(x=x1, y= y1, xend = x2, yend = y2,size = experiment,col = direction, linetype = experiment))+
  geom_segment(data = segment_anchor_points, aes(y=start, yend =end,col = col), x = .15, xend =.15, size = 2)+
  geom_hline(data = seeds_exp2, aes(yintercept = seed), col = "gray")+
  geom_point(data = colorbar, aes(y=items, color = colorValTarget), x = 15, size = 6)+
  geom_point(data = anchor_points, aes(y=x, col = color), x = .15, size = 3)+
  scale_y_continuous(expand = c(0,0),c(0,0), breaks = seq(0,360,30),limits = c(0,360))+
  scale_size_manual(values = c(1, 1.25, 1.25))+
  scale_linetype_manual(values = c(1,2,3))+
  scale_x_continuous(expand= c(0,0), breaks = seq(0,15,5))+
  scale_color_identity()+
  coord_flip()+
  theme_minimal()+
  labs(x = "iteration", y = "color (degrees)", linetype = "", size = "")+
  theme(panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = "transparent"))





