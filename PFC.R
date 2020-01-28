PFC<-function(pop,database){  
  if(length(pop)==1){
    return(0)
  }
  tam<-length(pop)
  failpat<-vector()
  numFail<-vector()
  numObs<-length(database$class)
  for(n in 1:tam){
    p<-pop[[n]]$pred
    failpat<-rbind(failpat,as.numeric(p==database$class))
    numFail[n]<-numObs-sum(failpat[n,])
  }
  ham<-hamming.distance(failpat)
  accfailcred<-matrix(0,ncol=tam,nrow=tam)
  for(x in 1:(tam-1)){
    for(y in (x+1):tam){
      accfailcred[x,y]<-ham[x,y]/(numFail[x]+numFail[y])
      accfailcred[y,x]<-accfailcred[x,y]
    }
  }
  res<-vector()
  for(n in 1:tam){
    res[n]<-sum(accfailcred[n,])/tam
  }
  return(res)
}