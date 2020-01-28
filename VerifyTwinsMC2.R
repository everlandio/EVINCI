VerifyTwinsMC2<-function(pop,similar=0.85){
  twins<-vector()
  tam<-length(pop)
  for(x in 1:(tam-1)){
    for(y in (x+1):tam){
      a<-length(which(pop[[x]]$ams==1 & pop[[y]]$ams==1))
      if(a/length(which(pop[[x]]$ams==1))>similar & a/length(which(pop[[y]]$ams==1))>similar){
        if(is.null(pop[[y]]$classifier))
          twins<-c(twins,y)
        else{
          if(pop[[x]]$classifier==pop[[y]]$classifier)
            twins<-c(twins,y)
        }
      }
    }
  }
  return(unique(twins))
}