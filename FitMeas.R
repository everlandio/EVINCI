FitMeas <- function(ind,base){
  
  ret<-list()
  indx<-which(ind==1)
  classes<-levels(base$class)
  baseTrain<-base[indx,]
  
  ret$ams<-ind
  dst = distCat(baseTrain[,-ncol(baseTrain)])
  meas<-n1byClass(dst,baseTrain)
  ret$meas<-meas[[2]]
  # ret$meas2<-meas[[3]]
  # ret$imb<--sd(summary(baseTrain$class)/nrow(baseTrain))
  m<-J48(class~.,data=baseTrain)
  rJava::.jcache(m$classifier)
  ret$model<-m
  p<-predict(m,newdata=base,type="prob")
  ret$pred<-p
  g<-gmeanMC(base$class,p)
  ret$gmean<-g[[1]]
  # ret$fit<-g[[2]]
  return(ret)
}
