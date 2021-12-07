library(jsonlite)
library(tidyverse)
source("helperFunctions.R")

rawDat =read_file("https://bradylab.ucsd.edu/turk/data/Isabella/colorCategoryReport_color_given_name_V1")
ls = strsplit(stringr::str_extract_all(rawDat, '<a href="(.*?).txt">')[[1]], '[\"]')

filenames = unlist(lapply(ls, function(x){return(x[2])}))

raw_data = list()


for(f in 1:length(filenames)){
  raw_data[[f]] = fromJSON(url(paste0("https://bradylab.ucsd.edu/turk/data/Isabella/colorCategoryReport_color_given_name_V1/",filenames[f])))
}

fups = c("19221")
data = c()
for(d in 1:length(raw_data)){
  dat = raw_data[[d]]
  curID = dat$curID
  print(curID)
  print(dat$comments)
  if(!(dat$curID %in% fups)){
    for(i in 1:length(dat$totalTrials)){
      if(i == 1){
        totalTrials = dat$totalTrials[[i]]
        totalTrials$currentTrial = 0:7
        totalTrials$currentBlock = rep(i-1, length(dat$totalTrials[[i]]))
      } else{
        temp = dat$totalTrials[[i]]
        temp$currentTrial = 0:7
        temp$currentBlock = rep(i-1, length(dat$totalTrials[[i]]))
        totalTrials = rbind(totalTrials, temp)
      }
    }

    
    totalTrials$subject = rep(d,nrow(totalTrials))
    totalTrials$curID = rep(curID,nrow(totalTrials))


    
    
    # demographics = data.frame(subject = d,
    #                           english = dat$egnlish,
    #                           firstLanguage = dat$firstLanguage,
    #                           #otherLanguage = dat$otherLanguage,
    #                           gender = dat$gender,
    #                           age = dat$age)
    
    colorBlindness = data.frame(responses = dat$colorBlindness$responses, place = dat$colorBlindness$plates, subject = d)
    
    if(d == 1){
      data = totalTrials
      #demographics_data =demographics
      colorBlindness_data = colorBlindness  
    }         
    else{
      data = rbind(data,totalTrials)
      #demographics_data = rbind(demographics_data, demographics)
      colorBlindness_data = rbind(colorBlindness_data, colorBlindness)  
    }
  }
  #}
}

all_data_exp5 = data.frame(data) %>%
  mutate(category = if_else(category == "Pruple", "Purple", category))%>% #lol I spelled purple wrong in js code (the word subjects saw was spelled right) what an idiot amirite
  mutate(colorname = tolower(category))
all_colorBlindness_data = data.frame(colorBlindness_data)
all_data_exp5$colorVal = sapply(as.integer(all_data_exp5$color),convert360ToColorVal)


#write_csv(all_data_exp5, "data/category-naming/all_data_exp5.csv")
