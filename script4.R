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


aux<-commandArgs(TRUE)
teste<-read.arff(paste("/home/EVER/Base/",aux[1],"/",aux[1],"-5-",aux[2],"tst.arff",sep=""))
train<-read.arff(paste("/home/EVER/Base/",aux[1],"/",aux[1],"-5-",aux[2],"tra.arff",sep=""))


temp<-vector()
for(i in 1:5){
  temp<-c(temp,system.time(zeta<-EVINCI(train,20,as.numeric(aux[4])))[3])
}

save(temp,file=paste("/home/EVER/EVINCI/temp",aux[4],".RData",sep=""))
