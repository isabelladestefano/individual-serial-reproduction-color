#as specified by Barbos & Compte 2020 -- ref to Fischer & Whitney 
# still need to make sure this is appropriate

serial_dependence_sliding_window <- function(data){ 
  window_size = 90
  window_step = 6
  window_max = 180 
  window_min = -180
  
  serdep_sliding_window<- data.frame(subject = c(),  error = c(), N= c(), dist= c())
  
  for(i in seq(window_min,window_max,6)){
    cur_min = i-window_size/2
    cur_max = i+window_size/2
    
    if(abs(cur_min) < -180){
      cur_min = cur_min+360
      serdep_sliding_window <- data%>%
        filter(dist_prev_trial >= cur_min) %>%
        bind_rows(data %>% filter(dist_prev_trial< cur_max))%>%
        group_by(subject)%>%
        summarise(error = mean(error), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
    if(abs(cur_max) >= 180){
      cur_max = cur_max-360
      serdep_sliding_window <- data %>%
        filter(dist_prev_trial >= cur_min) %>%
        bind_rows(data %>% filter(dist_prev_trial< cur_max))%>%
        group_by(subject)%>%
        summarise(error = mean(error), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
    else{
      serdep_sliding_window <- data %>%
        filter(dist_prev_trial < cur_max, dist_prev_trial >= cur_min) %>%
        group_by(subject)%>%
        summarise(error = mean(error), N=n())%>% 
        mutate(dist = i)%>%
        bind_rows(serdep_sliding_window)
    }
  }
  return(serdep_sliding_window)
}


#DoG

c = sqrt(2)/exp(-0.5)
DoG = function(x,b,a,w){b + x*a*w*c*exp(-(w*x)^2)}

data.frame(x=-180:180) %>% mutate(y = DoG(x,3,4,1/60))%>%
  ggplot(aes(x=x,y=y))+geom_point()



DoG_subjectFits = function(data){
  fits = data.frame(subject= c(), b= c(), a = c(), w = c())
  for(i in unique(data$subject)){
    rmse = function(par){
      data %>%
        filter(subject == i)%>%
        mutate(pred_error = DoG(dist, par[1],par[2],par[3]),
               sqer = (error - pred_error)^2) %>%
        summarise(N=n(), RMSE = sqrt(sum(sqer)/N)) %>% pull(RMSE)
    }
    
    result = optim(par = c(3, 4, 1/60), rmse)
    fits = bind_rows(fits,
                     data.frame(subject= i, b=result$par[1], a = result$par[2], w =result$par[3]))
    print(fits)
  }
  return(fits)
}

