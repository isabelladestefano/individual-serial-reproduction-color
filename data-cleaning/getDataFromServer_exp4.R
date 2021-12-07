library(jsonlite)
library(tidyverse)

rawDat =read_file("https://bradylab.ucsd.edu/turk/data/Isabella/colorCategoryReport_name_given_color_V1")
ls = strsplit(stringr::str_extract_all(rawDat, '<a href="(.*?).txt">')[[1]], '[\"]')

filenames = unlist(lapply(ls, function(x){return(x[2])}))

raw_data = list()


for(f in 1:length(filenames)){
  raw_data[[f]] = fromJSON(url(paste0("https://bradylab.ucsd.edu/turk/data/Isabella/colorCategoryReport_name_given_color_V1/",filenames[f])))
}

fups = c("19221")
data = c()
for(d in 1:length(raw_data)){
  dat = raw_data[[d]]
  curID = dat$curID
  print(curID)
  print(dat$comments)
  
  if(!(dat$curID %in% fups)){
    totalTrials = dat$totalTrials
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

all_data_exp4 = data.frame(data)
all_colorBlindness_data = data.frame(colorBlindness_data)
all_data_exp4$colorVal = sapply(as.integer(all_data_exp4$item),convert360ToColorVal)

#write_csv(all_data_exp4, "data/category-naming/all_data_exp4.csv")

