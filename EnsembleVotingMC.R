EnsembleVotingMC<- function(tab,lev){
  
  if(is.vector(tab)){
    tab<-matrix(tab,ncol=1)
  }
  #tab; tabela com as predicoes
  if(ncol(tab)>1){
    ens<-rep(0,nrow(tab))
    for(n in 1:nrow(tab)){
      ens[n]<-names(which.max(table(tab[n,])))
    }
    return(factor(ens,levels = lev))
  }else{
    return(factor(tab,levels=lev))
  }
}

EnsembleVotProb<-function(outputs){
  if(!is.list(outputs)){
    stop("O parametro precisa ser uma lista")
  }
  tamEns<-length(outputs)
  numExem<-nrow(outputs[[1]])
  ret<-vector()
  for(i in 1:numExem){
    ret<-rbind(ret,rowMeans(sapply(outputs,function(x){x[i,]})))
  }
#   ret<-ret/tamEns
  return(ret)
}