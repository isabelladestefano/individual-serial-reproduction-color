source("results/nearest-neighbor/nearestNeighborFunctions.R")

B=10000

#### exp2 ####
#get uniform sample
#unifSamp_exp2 = replicate(B,nearestNeighborUniformFunc(chain_data_exp2))    

for( i in 1:ncol(unifSamp_exp2)){
  if(i%%100 == 0){
    print(i)
  }
  if(i == 1){
    NNunifSamp_exp2 = data.frame(unifSamp_exp2[,i])
    NNunifSamp_exp2$samp = rep(i, nrow(NNunifSamp_exp2))
  }
  else{
    TempunifSamp_exp2 = data.frame(unifSamp_exp2[,i])
    TempunifSamp_exp2$samp = rep(i, nrow(TempunifSamp_exp2))
    NNunifSamp_exp2 = rbind(NNunifSamp_exp2, TempunifSamp_exp2)
  }
}

#write.csv(NNunifSamp_exp2,file ="results/nearestneighbor/null-hypotheses/NNunifSamp_exp2.csv" )

#get shuffled sample
#shuffleSamp_exp2 = replicate(B,nearestNeighborShuffleFunc(chain_data_exp2))    


for( i in 1:ncol(shuffleSamp_exp2)){
  if(i%%100 == 0){
    print(i)
  }
  if(i == 1){
    NNshuffleSamp_exp2 = data.frame(shuffleSamp_exp2[,i])
    NNshuffleSamp_exp2$samp = rep(i, nrow(NNshuffleSamp_exp2))
  }
  else{
    TempshuffleSamp_exp2 = data.frame(shuffleSamp_exp2[,i])
    TempshuffleSamp_exp2$samp = rep(i, nrow(TempshuffleSamp_exp2))
    NNshuffleSamp_exp2 = rbind(NNshuffleSamp_exp2, TempshuffleSamp_exp2)
  }
}
#write.csv(NNshuffleSamp_exp2,file ="results/nearestneighbor/null-hypotheses/NNshuffleSamp_exp2.csv" )



