source("results/nearest-neighbor/nearestNeighborFunctions.R")

B=10000

#### exp1 ####
#get uniform sample
#unifSamp_exp1 = replicate(B,nearestNeighborUniformFunc(chain_data_exp1))    

for( i in 1:ncol(unifSamp_exp1)){
  if(i%%100 == 0){
    print(i)
  }
  if(i == 1){
    NNunifSamp_exp1 = data.frame(unifSamp_exp1[,i])
    NNunifSamp_exp1$samp = rep(i, nrow(NNunifSamp_exp1))
  }
  else{
    TempunifSamp_exp1 = data.frame(unifSamp_exp1[,i])
    TempunifSamp_exp1$samp = rep(i, nrow(TempunifSamp_exp1))
    NNunifSamp_exp1 = rbind(NNunifSamp_exp1, TempunifSamp_exp1)
  }
}

#write.csv(NNunifSamp_exp1,file ="results/nearestneighbor/null-hypotheses/NNunifSamp_exp1.csv" )

#get shuffled sample
#shuffleSamp_exp1 = replicate(B,nearestNeighborShuffleFunc(chain_data_exp1))    


for( i in 1:ncol(shuffleSamp_exp1)){
  if(i%%100 == 0){
    print(i)
  }
  if(i == 1){
    NNshuffleSamp_exp1 = data.frame(shuffleSamp_exp1[,i])
    NNshuffleSamp_exp1$samp = rep(i, nrow(NNshuffleSamp_exp1))
  }
  else{
    TempshuffleSamp_exp1 = data.frame(shuffleSamp_exp1[,i])
    TempshuffleSamp_exp1$samp = rep(i, nrow(TempshuffleSamp_exp1))
    NNshuffleSamp_exp1 = rbind(NNshuffleSamp_exp1, TempshuffleSamp_exp1)
  }
}
#write.csv(NNshuffleSamp_exp1,file ="results/nearestneighbor/null-hypotheses/NNshuffleSamp_exp1.csv" )



