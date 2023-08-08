library(jsonlite)
library(tidyverse)
source("helperFunctions.R")


rawDat =read_file("https://bradylab.ucsd.edu/turk/data/Isabella/iteratedColorsSS2_AnchorColor_fourLocations_15iter_30errorTol_V3")
ls = strsplit(stringr::str_extract_all(rawDat, '<a href="(.*?).txt">')[[1]], '[\"]')

filenames = unlist(lapply(ls, function(x){return(x[2])}))

raw_data = list()


for(f in 1:length(filenames)){
  raw_data[[f]] = fromJSON(url(paste0("https://bradylab.ucsd.edu/turk/data/Isabella/iteratedColorsSS2_AnchorColor_fourLocations_15iter_30errorTol_V3/",filenames[f])))
}

fups = c()
data = c()
for(d in 1:length(raw_data)){
  dat = raw_data[[d]]
  curID = dat$curID
  print(curID)
  if(!(dat$curID %in% fups)){
    for(i in 1:length(dat$totalTrials)){
      if(i == 2){
        totalTrials = dat$totalTrials[[i]]
      } 
      else if(i>2){
        totalTrials = rbind(totalTrials, dat$totalTrials[[i]])
      }
    }
    totalTrials$subject = rep(d,nrow(totalTrials))
    totalTrials$curID = rep(curID,nrow(totalTrials))
    print(curID)
    print(dat$comments)
    
    demographics = data.frame(subject = d, english = dat$english, firstLanguage = dat$firstLanguage, otherLanguage = dat$otherLanguage, gender = dat$gender, age = as.numeric(dat$age))
    
    
    if(d == 1){
      data = totalTrials
      demographics_data =demographics
      #codes = c(code = code, curID = curID)
    }
    else{
      data = rbind(data,totalTrials)
      demographics_data = rbind(demographics_data, demographics)
      #codes = rbind(codes,c(code = code, curID = curID))
    }
  }
  #}
}

all_data_exp3 = data.frame(data) %>% distinct()
all_demographics_data_exp3 = data.frame(demographics_data )


test_data_exp3 = all_data_exp3 %>%
  filter(!isPractice)

test_data_exp3$anchor= lapply(test_data_exp3$items, function(x){return(x[2])}) %>% unlist()
test_data_exp3$items = lapply(test_data_exp3$items, function(x){return(x[1])}) %>% unlist()
test_data_exp3$order = test_data_exp3$order %>% paste() %>% gsub("c","", .) %>% gsub("3:0","(3, 2, 1, 0)",.)%>% gsub("0:3","(0, 1, 2, 3)",.)

test_data_exp3$error = mapply(circDist, test_data_exp3$imgNum, test_data_exp3$items)


chain_data_exp3 = test_data_exp3 %>% 
  filter(type == "fixed")%>%
  filter(abs(error)<=22.5) %>%
  group_by(subject, id )%>%
  arrange(iteration)%>%
  mutate(iteration = 1:length(unique(iteration)))%>%
  ungroup() 

# exclude subjects who did not complete all chains 
incomplete_chains_exclude_exp3 = chain_data_exp3 %>% group_by(subject, id) %>% summarise(sumit = sum(iteration)) %>% ungroup() %>% filter(sumit < 120) %>% select(subject, id) %>% group_by(subject) %>% summarise(numSeed = n()) %>% filter(numSeed<20) %>% select(subject)

no_demographics_exclude_exp3 = all_demographics_data_exp3 %>% filter(english == "---") %>% select(subject)

chain_data_exp3 = chain_data_exp3 %>% anti_join(incomplete_chains_exclude_exp3) %>% anti_join(no_demographics_exclude_exp3)
test_data_exp3 = test_data_exp3 %>% anti_join(incomplete_chains_exclude_exp3) %>% anti_join(no_demographics_exclude_exp3)
demographics_data_exp3 = all_demographics_data_exp3 %>% anti_join(incomplete_chains_exclude_exp3) %>% anti_join(no_demographics_exclude_exp3)

test_data_exp3$colorVal = sapply(as.integer(test_data_exp3$imgNum),convert360ToColorVal)
test_data_exp3$colorValTarget = sapply(as.integer(test_data_exp3$items),convert360ToColorVal)

seeds_exp3 = data.frame(seedID = 0:4, seed = c(18,90,162,234,306))

chain_data_exp3 =   chain_data_exp3 %>%
  mutate(seedID = if_else(setID == "1", as.numeric(id), as.numeric(id)-5))%>%
  inner_join(seeds_exp3)%>%
  group_by(seedID,setID, subject)%>%
  mutate(relative_pos = cumsum(error))%>%
  ungroup()

print(paste0("test_data_exp3 subjects: ", test_data_exp3 %>% pull(subject) %>% unique() %>% length()))
print(paste0("chain_data_exp3 subjects: ", chain_data_exp3 %>% pull(subject) %>% unique() %>% length()))
print(paste0("demographics_data_exp3 subjects: ", demographics_data_exp3 %>% pull(subject) %>% unique() %>% length()))

#write.csv(all_data_exp3, 'data/exp3/inlab/all_data_exp3_inlab.csv')
write_csv(test_data_exp3, "data/exp3/inlab/test_data_exp3_inlab.csv")
write_csv(chain_data_exp3, "data/exp3/inlab/chain_data_exp3_inlab.csv")
write_csv(demographics_data_exp3, "data/exp3/inlab/demographics_data_exp3_inlab.csv")

