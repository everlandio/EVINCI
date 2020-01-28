
torneioMC<- function(lambda,ranks,pfc,tournamentSize){
  # lambda = tamanho da prole
  # ranks = vector com o fitness dos ind da pop pai
  # tournamentSize = quantidade de ind que entra no torneio
  # em caso de empate seleciona o individuo com maior diversidade (pfc)
  
  res<-vector()
  popSize<-length(ranks)
  for(n in 1:(2*lambda)){
    poolIdxs <- sample(popSize, tournamentSize)
    maxFit<-max(ranks[poolIdxs])
    best<-poolIdxs[which(ranks[poolIdxs]==maxFit)]
    if(length(best)==1){
      res[n]<-best
    }else{
      aux<-vector()
      for(x in best){
        aux<-c(aux,pfc[x])
      }
      if(anyNA(aux)){
        aux[is.na(aux)]<-0
      }
      res[n]<-best[which.max(aux)]
    }
  }
  return(res)
}