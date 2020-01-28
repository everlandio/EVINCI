#Sao criadas amostras relativamente balanceadas
#Operador de crossover gera um ponto de crossover 
#Os filhos sao gerados com partes dos vetores dos pais (ponto de crossover)
#O operador de mutacao gera uma pertubacao nos filhos.
#Versao com alteracao do algoritmo genetico

EVINCI <- function(database,QtGer,popsize){
  
  pop<-list()
  points<-vector()
  sizeTwins<-vector()
  mcount<-0
  
  #which is majority class
  n1max<-vector()
  n1min<-vector()
  cl<-levels(database$class)
  lim<-mean(table(database$class)) - sd(table(database$class))/2
  for(i in 1:length(cl)){
    if(length(which(database$class==cl[i]))<lim)
      n1min<-c(n1min,i)
    else
      n1max<-c(n1max,i)
  }
  for(j in 1:popsize){
    pop[[j]]<-list()
    amostra<-amostMC3(database,popsize,j)
    pop[[j]]<-FitMeas(amostra,database)
    mcount<-mcount+1
  }
  # print(sapply(pop,function(x) table(database[which(x$ams==1),"class"])))
  print("Initial Population Iteration 1")
  e<-EnsembleVotProb(lapply(pop,function(x) x$pred))
  print(ensg<-gmeanMC(database$class,e))
  ensgmean<-ensg[[1]]
  EnsAnaliseGmean<-ensg[[1]]
  savedpop<-pop
  gerSP<-1

  for(z in 2:QtGer){
    for(i in 1:length(pop)){
      pop[[i]]$filho<-F
    }
    # Extract fitness values
    fitness<-sapply(pop,function(x){
      a<-vector()
      b<-vector()
      for(i in n1max){
        a<-c(a,x$meas[i,n1min])
      }
      a<--sum(a)
      b<-x$gmean
      return(c(a,b))
    })
    # print(fitness)
    fitness<-nds_rank(fitness)
    pfc<-PFC(pop,database)
    
    parentIndices<-torneioMC(popsize,fitness,pfc,3)
    allParentIndices <- 1:(2*popsize)
    motherIndices <- parentIndices[allParentIndices %% 2 == 1] # mothers are odd parent indicies
    fatherIndices <- parentIndices[allParentIndices %% 2 == 0] # fathers are even parent indices
    filhos<-offspringMC(pop,motherIndices,fatherIndices,database)
    for(i in 1:length(filhos)){
      filhos[[i]]$filho<-T
    }
    pop2<-pop
    pop<-c(pop,filhos)
    # Verifica se existe solucoes gemeas e as retira
    twins<-VerifyTwinsMC2(pop)
    cat("size of twins: ",length(twins),"\n")
    if(length(twins)>0){
      pop<-pop[-twins]
    }
    if(length(pop)<popsize){
      for(i in 1:length(filhos)){
        filhos[[i]]$ams<-mutationMC(filhos[[i]]$ams)
      }
      pop<-c(pop,filhos)
    }
    aux<-lapply(pop, function(x) {
      if(x$filho){
        mcount<-mcount+1
        return(FitMeas(x$ams,database))
      }
      else{
        return(x)
      }})
    mcount<-mcount+(length(pop)-popsize)
    pop<-aux
    fitness<-sapply(pop,function(x){
      a<-vector()
      b<-vector()
      for(i in n1max){
        a<-c(a,x$meas[i,n1min])
      }
      a<--sum(a)
      b<-x$gmean
      return(c(a,b))
    })
    fitness<-nds_rank(fitness)
    pfc<-PFC(pop,database)
    indicesNewPop<-order(fitness,pfc,decreasing = T)[1:popsize]
    pop<-pop[indicesNewPop]
    
    # print(sapply(pop,function(x) table(database[which(x$ams==1),"class"])))
    e<-EnsembleVotProb(lapply(pop,function(x) x$pred))
    ensg<-gmeanMC(database$class,e)
    cat("Gmean Ensemble ",ensg[[1]], "\n")
    EnsAnaliseGmean<-c(EnsAnaliseGmean,ensg[[1]])
    # cat("Ens Gmean",EnsAnaliseGmean,"\n")
    
    if(ensgmean<tail(EnsAnaliseGmean,n=1)){
      ensgmean<-tail(EnsAnaliseGmean,n=1)
      savedpop<-pop
      gerSP<-z
      print("savedpop")
    }
    cat("Iteration ",z,"\n")
    if(((z-gerSP)>=10)||(ensgmean==1)){
      break
    }
  }
  cat("Finished with ",z,"iterations\n")
  cat("Best Ensemble saved in iteration ",gerSP,", Gmean: ",ensgmean,"\n")
  return(list(savedpop,ensgmean))
}