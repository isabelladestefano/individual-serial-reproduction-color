clustering_final_iter = function(final_iter, attractors){
  distances = c()
  dist = c()
  for(j in 1:length(final_iter)){
    for(i in 1:length(attractors)){
        distance =circDist(attractors[i], final_iter[j])
        if(distance!=0){
          distances = c(distances, distance)
        }
      }
  dist = c(dist,min(abs(distances)))
  }
  mean(dist)
}

cluster_attractors =function(attractors){
  attractor_clustering = c()
  for( i in unique(chain_data_exp1$subject)){
  
  attractor_clustering_sub = chain_data_exp1 %>%
    filter(iteration == 15) %>%
    filter(subject == i)%>% 
    pull(imgNum)%>%
    clustering_final_iter(., attractors  )
  attractor_clustering = c(attractor_clustering,attractor_clustering_sub)
  }
  
  attractor_clustering
}


unif_attractors = replicate(1000,mean(cluster_attractors(sample(0:359, 3))))
unif_attractors = data.frame(x= unif_attractors)

filler_attract = cluster_attractors(filler_attractors %>% filter(type == "attractor") %>% pull(crit_points)) %>% data.frame(x=.) %>% summarise(x=mean(x))


ggplot(unif_attractors)+
  geom_histogram(aes(x=x), binwidth = .1)+
  #geom_vline(data = data.frame(x=mean(unif_attractors$x)), aes(xintercept = x))+
  geom_vline(data = filler_attract, aes(xintercept=x), col = "black")



filler_attract$x>=unif_attractors$x %>% as.numeric() %>% sum
