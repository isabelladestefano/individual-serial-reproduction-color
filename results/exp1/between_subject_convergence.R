library(tidyverse)
test_data_exp1 = read_csv("data/exp1/test_data_exp1.csv")
chain_data_exp1 = read_csv("data/exp1/chain_data_exp1.csv")


log_probs = data.frame(subject = c(), lp = c())

for(s in unique(chain_data_exp1$subject)){
  one_out = chain_data_exp1 %>% mutate(imgNum = imgNum%%360)%>%
    filter(iteration == 15, subject ==s) %>% 
    select(imgNum)
  
  all_but = test_data_exp1 %>%mutate(imgNum = imgNum%%360)%>%
    filter( subject !=s) %>% 
    select(imgNum) %>%
    group_by(imgNum) %>% 
    summarise(N=n()) 

  if(length((0:359)[!(0:359 %in% all_but$imgNum)])!=0){  
  all_but = all_but %>% bind_rows(data.frame( imgNum = (0:359)[!(0:359 %in% all_but$imgNum)], N=0)) %>%
    mutate(N=N+1) %>%
    mutate(p = N/sum(N))
  }
   else{
     all_but = all_but %>%
       mutate(N=N+1) %>%
       mutate(p = N/sum(N))
   }

  
    
  logProb = 0
  for(i in one_out$imgNum){
    logProb = logProb + all_but %>% filter(imgNum == i) %>%
      pull(p) %>%
      log2()
  }
  
  print(logProb)
  log_probs = bind_rows(log_probs, data.frame(subject = s, lp = logProb))
}


null =20*log2(1/360)

t.test(log_probs$lp, mu = null, alternative = "greater")




#exp2

log_probs = data.frame(subject = c(), lp = c())

for(s in unique(chain_data_exp2$subject)){
  one_out = chain_data_exp2 %>% mutate(imgNum = imgNum%%360)%>%
    filter(iteration == 15, subject ==s) %>% 
    select(imgNum)
  
  all_but = test_data_exp2 %>%mutate(imgNum = imgNum%%360)%>%
    filter( subject !=s) %>% 
    select(imgNum) %>%
    group_by(imgNum) %>% 
    summarise(N=n()) 
  
  if(length((0:359)[!(0:359 %in% all_but$imgNum)])!=0){  
    all_but = all_but %>% bind_rows(data.frame( imgNum = (0:359)[!(0:359 %in% all_but$imgNum)], N=0)) %>%
      mutate(N=N+1) %>%
      mutate(p = N/sum(N))
  }
  else{
    all_but = all_but %>%
      mutate(N=N+1) %>%
      mutate(p = N/sum(N))
  }
  
  
  
  logProb = 0
  for(i in one_out$imgNum){
    logProb = logProb + all_but %>% filter(imgNum == i) %>%
      pull(p) %>%
      log2()
  }
  
  print(logProb)
  log_probs = bind_rows(log_probs, data.frame(subject = s, lp = logProb))
}


null =10*log2(1/360)

t.test(log_probs$lp, mu = null, alternative = "greater")
