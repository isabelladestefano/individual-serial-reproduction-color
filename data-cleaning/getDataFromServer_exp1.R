library(jsonlite)
library(tidyverse)
source("helperFunctions.R")

rawDat =read_file("https://bradylab.ucsd.edu/turk/data/Isabella/iteratedColorsSS1UniformTargetDistribution15iterations_colorwheel2_V3")
ls = strsplit(stringr::str_extract_all(rawDat, '<a href="(.*?).txt">')[[1]], '[\"]')

filenames = unlist(lapply(ls, function(x){return(x[2])}))

raw_data = list()


for(f in 1:length(filenames)){
  raw_data[[f]] = fromJSON(url(paste0("https://bradylab.ucsd.edu/turk/data/Isabella/iteratedColorsSS1UniformTargetDistribution15iterations_colorwheel2_V3/",filenames[f])))
}

fups=c() ## subject 64 and 65 ids are replicated but are different subjects and are treated as such in code
data = c()
for(d in 1:length(raw_data)){
  dat = raw_data[[d]]
  curID = dat$curID
  print(curID)
  if(!(dat$curID %in% fups)){
    for(i in 1:length(dat$totalTrials)){
      if(i == 2){ ## removes practice block
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
    
    focalCols = data.frame(subject =c(), focalColors = c())
    for(j in 1:length(dat$totalFocalColors)){
      for(k in 1:length(dat$totalFocalColors[[j]]))
      if(length(dat$totalFocalColors[[j]][k])>0){
        if(!is.na(dat$totalFocalColors[[j]][k])){
          focalCols = rbind(focalCols, data.frame(subject =c(d), focalColors = c(dat$totalFocalColors[[j]][k])))
        }
      }
    }
    
    if(d == 1){
      demographics_data =demographics
      data = totalTrials
      focalReport = focalCols
    }
    else{
      demographics_data = rbind(demographics_data, demographics)
      data = rbind(data,totalTrials)
      focalReport = rbind(focalReport, focalCols)
    }
  }
}

all_data_exp1 = data.frame(data)%>% distinct() %>%   
  mutate(items = unlist(items))%>%
  mutate(order = paste(order))

#write.csv(all_data, 'data/all_data.csv')
all_demographics_data_exp1 = data.frame(demographics_data )

test_data_exp1 = all_data_exp1 %>%
  arrange(subject, currentBlock,currentTrial)%>%
  filter(!isPractice)%>%
  mutate(items = unlist(items)%%360) %>%
  group_by(subject) %>%
  mutate(curTrial = currentTrial + 20*(currentBlock - 1))

test_data_exp1$error = mapply(circDist, test_data_exp1$imgNum, test_data_exp1$items)

test_data_exp1 %>% ggplot(aes(x=error))+ 
  geom_histogram(binwidth = 1)+
  facet_grid(type~.) 

chain_data_exp1 = test_data_exp1 %>% 
  filter(type == "fixed")%>%
  group_by(subject, id )%>%
  arrange(currentBlock,currentTrial, iteration)%>%
  mutate(rejected = curError>22.5) %>%
  filter(!rejected) %>%
  mutate(iteration_true = 1:length(iteration))
  


chain_data_exp1%>%
  select(currentBlock, currentTrial, subject, id, iteration)%>%
  group_by(iteration, subject,id) %>% summarise(N=n()) %>% filter(N>1)

chain_data_exp1 %>%
  ggplot(aes(x=error))+
  geom_histogram(binwidth = 1)

incomplete_chains_exclude_exp1 = 
  chain_data_exp1 %>% 
  group_by(subject, id) %>% 
  summarise(sumit = sum(iteration)) %>% 
  ungroup() %>% 
  filter(sumit < 120) %>% 
  select(subject, id) %>% 
  group_by(subject) %>% 
  summarise(numSeed = n()) %>% 
  filter(numSeed<20) %>%
  select(subject)

no_demographics_exclude_exp1 = all_demographics_data_exp1 %>% filter(english == "---") %>% select(subject)

chain_data_exp1 = chain_data_exp1 %>% anti_join(incomplete_chains_exclude_exp1) %>% anti_join(no_demographics_exclude_exp1)
test_data_exp1 =test_data_exp1 %>% anti_join(incomplete_chains_exclude_exp1) %>% anti_join(no_demographics_exclude_exp1)
demographics_data_exp1 = all_demographics_data_exp1 %>% anti_join(incomplete_chains_exclude_exp1) %>% anti_join(no_demographics_exclude_exp1)
focalReport_exp1 = focalReport%>% anti_join(incomplete_chains_exclude_exp1) %>% anti_join(no_demographics_exclude_exp1)

test_data_exp1$colorVal = sapply(as.integer(test_data_exp1$imgNum),convert360ToColorVal)
test_data_exp1$colorValTarget = sapply(as.integer(test_data_exp1$items),convert360ToColorVal)


seeds_exp1 = data.frame(seedID = 0:9, seed = c(18,54,90,126,162,198,234,270,306,342))

chain_data_exp1$colorVal = sapply(as.integer(chain_data_exp1$imgNum),convert360ToColorVal)


chain_data_exp1 = chain_data_exp1%>%
  ungroup()%>%
  mutate(seedID = if_else(setID == "1", as.numeric(id), as.numeric(id)-10))%>%
  inner_join(seeds_exp1)%>%
  group_by(seedID,setID, subject)%>%
  mutate(relative_pos = cumsum(error)) %>%
  ungroup()

print(paste0("test_data_exp1 subjects: ", test_data_exp1 %>% pull(subject) %>% unique() %>% length()))
print(paste0("chain_data_exp1 subjects: ", chain_data_exp1 %>% pull(subject) %>% unique() %>% length()))
print(paste0("demographics_data_exp1 subjects: ", demographics_data_exp1 %>% pull(subject) %>% unique() %>% length()))
print(paste0("focalReport_exp1 subjects: ", focalReport_exp1 %>% pull(subject) %>% unique() %>% length()))

# write_csv(all_data_exp1, "data/exp1/all_data_exp1.csv")
# write_csv(test_data_exp1, "data/exp1/test_data_exp1.csv")
# write_csv(chain_data_exp1, "data/exp1/chain_data_exp1.csv")
# write_csv(demographics_data_exp1, "data/exp1/demographics_data_exp1.csv")
# write_csv(focalReport_exp1, "data/exp1/focalReport_exp1.csv")

