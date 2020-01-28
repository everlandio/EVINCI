

source('EVINCI/EVINCI.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/FitMeas.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/amostMC3.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/offspringMC.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/torneioMC.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/VerifyTwinsMC2.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/PFC.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/EnsembleVotingMC.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/measures.R')
source('Google Drive File Stream/Meu Drive/Codigos/EVINCI/n1byClass.R')



library(cluster)
library(igraph)
library(emoa)
library(e1071)
library(RWeka)
library(RSNNS)

MLP = RWeka::make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
