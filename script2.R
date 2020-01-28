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
library(emoa)
library(e1071)
library(RWeka)
library(RSNNS)



source('EVINCI.R')
source('FitMeas.R')
source('amostMC3.R')
source('offspringMC.R')
source('torneioMC.R')
source('VerifyTwinsMC2.R')
source('PFC.R')
source('EnsembleVotingMC.R')
source('measures.R')
source('n1byClass.R')

MLP = RWeka::make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")


aux<-commandArgs(TRUE)
teste<-read.arff(paste("/home/EVER/Base/",aux[1],"/",aux[1],"-5-",aux[2],"tst.arff",sep=""))
train<-read.arff(paste("/home/EVER/Base/",aux[1],"/",aux[1],"-5-",aux[2],"tra.arff",sep=""))


g<-0

while(g==0){
  zeta<-EVINCI(train,30,30)
  
  predProb<-list()
  for(i in 1:length(zeta)){
    predProb[[length(predProb)+1]]<-predict(zeta[[i]]$model,newdata=teste,type="prob")
  }
  ensProb<-EnsembleVotProb(predProb)
  g<-gmeanMC(teste$class,ensProb)[[1]]
  a<-mAuc(teste$class,ensProb)
}

resp<-c(g,a)

save(resp,file=paste("/home/EVER/EVINCI/results/",aux[1],aux[2],"-",aux[3],".RData",sep=""))
