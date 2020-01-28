# install.packages("/home/EVER/Codigos/mlbench_2.1-1.tar.gz",lib="/home/EVER/Packages/",repos=NULL)
# install.packages("/home/EVER/Codigos/doParallel_1.0.10.tar.gz",lib="/home/EVER/Packages/",repos=NULL)
# library(doParallel,lib.loc="/home/EVER/Packages")
# install.packages("/home/EVER/Codigos/RANN_2.5.tar.gz",lib="/home/EVER/Packages/",repos=NULL)
# library(RANN,lib.loc="/home/EVER/Packages")
# install.packages("/home/EVER/Codigos/emoa_0.5-0.tar.gz",lib="/home/EVER/Packages/",repos=NULL)
# install.packages("/home/EVER/Codigos/adabag_4.1.tar.gz",lib="/home/EVER/Packages/",repos=NULL)
# install.packages("/home/EVER/Codigos/unsatellited_2.0.tar.gz",lib="/home/EVER/Packages/",repos=NULL)
# install.packages("/home/EVER/Packages/RSNNS_0.4-7.tar.gz",lib="/home/EVER/Packages/",repos=NULL)


library(cluster)
library(igraph)
library(emoa,lib.loc="/home/EVER/Packages")
library(e1071)
library(RWeka,lib.loc="/home/EVER/Packages")
library(RSNNS,lib.loc="/home/EVER/Packages")
library(mlbench)
library(caret)

source('/home/EVER/EVINCI/EVINCI.R')
source('/home/EVER/EVINCI/FitMeas.R')
source('/home/EVER/EVINCI/amostMC3.R')
source('/home/EVER/EVINCI/offspringMC.R')
source('/home/EVER/EVINCI/torneioMC.R')
source('/home/EVER/EVINCI/VerifyTwinsMC2.R')
source('/home/EVER/EVINCI/PFC.R')
source('/home/EVER/EVINCI/EnsembleVotingMC.R')
source('/home/EVER/EVINCI/measures.R')
source('/home/EVER/EVINCI/n1byClass.R')

MLP = RWeka::make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")

gerbase<-function(tam){
  tam<-round((3*tam)/2.1)
  base<-mlbench.2dnormals(tam,cl=3)
  base<-as.data.frame(cbind(base$x,base$classes))
  names(base)<-c(paste("X",1:(ncol(base)-1),sep=""),"class")
  base$class<-factor(base$class)
  min<-which(base$class==3)
  base<-base[-sample(min,(0.9*length(min))),]
  base<-data.frame(base)
  
  b<-list()
  a<-createDataPartition(base$class,p=0.8)
  b$train<-base[a$Resample1,]
  b$test<-base[-a$Resample1,]
  
  return(b)
}

aux<-commandArgs(TRUE)

temp<-vector()
gm<-vector()
mcount<-vector()
for(i in 1:5){
  base<-gerbase(as.numeric(aux[1]))
  train<-base$train
  test<-base$test
  temp<-c(temp,system.time(z<-EVINCI(train,20,10))[3])
  
  predProb<-list()
  for(i in 1:length(z[[1]])){
    predProb[[length(predProb)+1]]<-predict(z[[1]][[i]]$model,newdata=test,type="prob")
  }
  ensProb<-EnsembleVotProb(predProb)
  gm<-c(gm,gmeanMC(test$class,ensProb)[[1]])
  mcount<-c(mcount,z[[2]])
}

res<-list()
res$gm<-gm
res$temp<-temp
save(res,file=paste("/home/EVER/EVINCI/testsize/size",aux[1],".RData",sep=""))
