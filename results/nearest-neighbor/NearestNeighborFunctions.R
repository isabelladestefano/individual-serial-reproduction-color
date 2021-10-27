nearestNeighborDistance = function(itemPosition, set){
  distances = c()
  for(i in 1:length(set)){
    if(i != itemPosition){
      dist =circDist(set[i], set[itemPosition])
      if(dist!=0){
        distances = c(distances, dist)
      }
    }
  }
  min(abs(distances))
}


nearestNeighborUniformFunc = function(df){df %>%
    group_by(iteration,subject,setID)%>%
    mutate(imgNum = runif(length(imgNum),0,359))%>%
    do(NN_unif = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>% 
    mutate(NN_unif=mean(NN_unif))
}

nearestNeighborShuffleFunc = function(df){
  df %>%
    group_by(id,iteration,setID)%>%
    mutate(subject = sample(subject,length(subject)))%>%
    ungroup()%>%
    group_by(iteration,subject, setID)%>%
    do(NN_shuffle = mean(mapply(nearestNeighborDistance, 1:length(.$imgNum), MoreArgs = list(set = .$imgNum))))%>%
    mutate(NN_shuffle=mean(NN_shuffle))
}
