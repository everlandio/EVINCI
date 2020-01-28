
require(caret)

cfold = function(data,  fold=FOLDS) {
  lines = createFolds(data$target, k=fold, list=TRUE);
  test = lapply(1:fold, function (i){
    data[lines[[i]], ]
  });
  train = lapply(1:fold, function(i){
    data[-lines[[i]], ]
  });
  tmp = list();
  tmp$train = train;
  tmp$test = test;
  return(tmp);
}

library(dismo);
FOLDS = 5;
stratified <- function(data) {
  id = kfold(data, k=FOLDS);  
  tran = lapply(1:FOLDS, function(i) {
    subset(data, id %in% setdiff(1:FOLDS, i));
  });
  test = lapply(1:FOLDS, function(i) {
    subset(data, id %in% i);
  });
  tmp = list();
  tmp$tran = tran;
  tmp$test = test;
  return(tmp);
}


aux<-"bank2"
sum(is.na(base))

names(base)<-c(paste("X",1:(ncol(base)-1),sep=""),"class")
base<-as.data.frame(base)
base$class<-factor(base$class)
table(base$class)
base<-base[-which(base$class%in%names(which(table(base$class)<15))),]
table(base$class)
base$class<-factor(as.numeric(base$class))
table(base$class)

sapply(base,class)
for(n in 1:(ncol(base)-1)){
  if(class(base[,n])=="factor"){
    base[,n]<-as.factor(as.numeric(base[,n]))
  }
}

base<-normalizeFeatures(base,target = "class")

b2<-cfold(base,5)
for(n in 1:5){
  write.arff(b2$train[[n]],paste("~/Documents/Base/",aux,"/",aux,"-5-",n,"tra.arff",sep=""))
  write.arff(b2$test[[n]],paste("~/Documents/Base/",aux,"/",aux,"-5-",n,"tst.arff",sep=""))
}


vec<-vector()
for(n in 1:(ncol(base)-1)){
  base[,n]<-as.numeric(as.character(base[,n]))
  if(sum(base[,n])==0){
    vec<-c(vec,n)
  }
}
print(vec)

out<-decodeClassLabels(base$class)
base<-cbind(base[,1:(ncol(base)-1)],out)
write.table(base,paste("~/Documents/Codigos/DyS_source/src/",aux,".data",sep=""),row.names = F,col.names = F,sep=",",na="?")

teste<-list()
train<-list()
aux<-"dnormal"
for(n in 1:5){
  teste[[n]]<-read.arff(paste("Google Drive File Stream/Meu Drive/Base/",aux,"/",aux,"-5-",n,"tst.arff",sep=""))
  train[[n]]<-read.arff(paste("Google Drive File Stream/Meu Drive/Base/",aux,"/",aux,"-5-",n,"tra.arff",sep=""))
}


base<-rbind(train[[1]],teste[[1]])
write.arff(base,paste("~/Documents/Codigos/DyS_source/src/",aux,".arff",sep=""))

med2<-function(aux,e){
  gm<-vector()
  gmin<-vector()
  gmaj<-vector()
  for(x in 1:5){
    for(y in 1:10){
      load(paste("/home/EVER/Execmoga22/",aux,"/Exec",aux,x,"-",y,".RData",sep=""))
      gm<-c(gm,zeta$gmean[[1]])
      gmin<-c(gmin,zeta$gmean[[2]][2])
      gmaj<-c(gmaj,zeta$gmean[[2]][1])
    }
  }
  cat("gmean: ", mean(gm),"\n")
  cat("gmin: ", mean(gmin),"\n")
  cat("gmaj: ", mean(gmaj),"\n")
  return(gm)
}

med2<-function(aux,v){
  qg<-vector()
  for(x in 1:5){
    for(y in 1:10){
      load(paste("/home/EVER/Execmoga22/",aux,"/Exec",aux,x,"-",y,".RData",sep=""))
      qg<-c(qg,zeta[[1]]$QG)
    }
  }
  print(qg)
  print(mean(qg))
  print(sd(qg))
}

med3<-function(aux,v){
  ensize<-vector()
  for(x in 1:5){
    for(y in 1:10){
      load(paste("/home/EVER/Execmoga",v,"/",aux,"/Exec",aux,x,"-",y,".RData",sep=""))
      ensize<-c(ensize,length(zeta))
    }
  }
  print(ensize)
  print(mean(ensize))
  print(sd(ensize))
}

med<-function(aux){
  gmean<-vector()
  gmin<-vector()
  gmaj<-vector()
  for(x in 1:5){
    for(y in 1:10){
      load(paste("/home/EVER/HP/bank/Exec",aux,x,"-",y,".RData",sep=""))
      gmean<-c(gmean,zeta[[3]][[1]])
      gmin<-c(gmin,zeta[[3]][[2]][2])
      gmaj<-c(gmaj,zeta[[3]][[2]][1])
    }
  }
  cat("gmean: ", mean(gmean),"\n")
  cat("gmin: ", mean(gmin),"\n")
  cat("gmaj: ", mean(gmaj),"\n")
  return(gmean)
}

med6<-function(aux,v){
  gmMLP<-vector()
  for(x in 1:5){
    for(y in 1:10){
      load(paste("/home/EVER/Execmoga",v,"/",aux,"/Exec",aux,x,"-",y,".RData",sep=""))
      gmMLP<-rbind(gmMLP,zeta$gmeanMLP[[2]])
    }
  }
  print(gmMLP)
  cat("Mean MLP: ",colMeans(gmMLP),"\n")
}

med5<-function(aux,v){
  gmMLP<-vector()
  gmJ48<-vector()
  gmsvm<-vector()
  for(x in 1:5){
    train<-read.arff(paste("/home/EVER/Base/",aux,"/",aux,"-5-",x,"tra.arff",sep=""))
    teste<-read.arff(paste("/home/EVER/Base/",aux,"/",aux,"-5-",x,"tst.arff",sep=""))
    for(y in 1:10){
      load(paste("/home/EVER/Execmoga",v,"/",aux,"/Exec",aux,x,"-",y,".RData",sep=""))
      prevs<-sapply(zeta$pop,function(x) predict(x$modelMLP,newdata=train,type="class"))
      gm<-vector()
      for(i in 1:ncol(prevs)){
        gm<-c(gm,gmeanMC(train$class,prevs[,i])[[1]])
      }
      pruned<-ensprune(prevs,train,gm)
      prevs<-sapply(zeta$pop[pruned[[1]]],function(x) predict(x$modelMLP,newdata=teste,type="class"))
      gmMLP<-c(gmMLP,gmeanMC(teste$class,EnsembleVotingMC(prevs))[[1]])
      
    }
  }
  cat("Mean MLP: ",mean(gmMLP),"\n")
  cat("Sd   MLP: ",sd(gmMLP),"\n")
  # print(gmJ48)
  # cat("Mean J48: ",mean(gmJ48),"\n")
  # cat("Sd   J48: ",sd(gmJ48),"\n")
  # print(gmsvm)
  # cat("Mean svm: ",mean(gmsvm),"\n")
  # cat("Sd   svm: ",sd(gmsvm),"\n")
}

for(i in pp){
  mydata<-train[[1]][which(i$ams==1),]
  d<-dist(mydata)
  fit2 <- cmdscale(d,eig=TRUE, k=2)
  x <- fit2$points[,1]
  y <- fit2$points[,2]
  plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",main="Metric	MDS",col=mydata$class)
}



matx<-vector()
for(x in 4:5){
  for(y in 1:10){
    load(paste("Documents/Resultados/chess/Execchess",x,"-",y,".RData",sep=""))
    met<-lapply(zeta,function(i){
      c<-complexity(train[[x]][which(i$ams==1),])
      return(c(c,i$gmean))})
    for(n in 1:length(zeta)) matx<-rbind(matx,met[[n]])
    print(c("y: ",y))
  }
  print(c("x: ",x))
}


aux<-vector()
matx<-vector()
f<-list.files(path = "Documents/Resultados/metricas/",full.names = T)
x<-1
for(i in f){
  load(i)
  aux<-c(aux,gsub(gsub(x=i,pattern = "Documents/Resultados/metricas//",replacement = ""),pattern = "complex.RData",replacement = ""))
  teste<-read.arff(paste("Documents/Base/",aux[x],"/",aux[x],"-5-1tst.arff",sep=""))
  train<-read.arff(paste("Documents/Base/",aux[x],"/",aux[x],"-5-1tra.arff",sep=""))
  base<-rbind(train,teste)
  c<-c(c,irm(base))
  matx<-rbind(matx,c)
  x<-x+1
}
nam<-c("f1", "f2", "f3", "f4","l1", "l2", "l3","n1","n2", "n3", "n4", "t1", "t2","imb")
matx<-as.data.frame(matx)
rownames(matx)<-aux
colnames(matx)<-nam




gm<-vector()
for(i in rownames(matx)){
  aux<-i
  gmb<-vector()
  for(n in 1:5){
    teste<-read.arff(paste("Documents/Base/",aux,"/",aux,"-5-",n,"tst.arff",sep=""))
    train<-read.arff(paste("Documents/Base/",aux,"/",aux,"-5-",n,"tra.arff",sep=""))
    m<-MLP(class~.,data=train)
    p<-predict(m,newdata=teste,type="class")
    gmb<-c(gmb,gmeanMC(teste$class,p))
  }
  gm<-c(gm,c(i,mean(gmb)))
  print(gm)
}


matcar<-vector()
for(n in pp){
  c<-c(complexity(train[[1]][which(n$ams==1),]),irm(train[[1]][which(n$ams==1),]))
  p<-predict(n$model,newdata = teste[[1]])
  matcar<-rbind(matcar,c(c,gmeanMC(teste[[1]]$class,p)))
}

matchess<-vector()
foreach(n=1:10)%dopar%{
  source('/home/ever/emosaicwc/complexity.r')
  source('/home/ever/emosaicwc/config.r')
  source('/home/ever/emosaicwc/overlapping.r')
  source('/home/ever/emosaicwc/neighborhood.r')
  source('/home/ever/emosaicwc/imbalance.r')
  source('/home/ever/emosaicwc/linearity.r')
  gmeanMC<-function(truth,response){
    cl<-levels(truth)
    nclass<-length(cl)
    MatConfTest<-table(response,truth)
    TPtest<-vector()
    for(n in 1:nclass){
      TPtest[n]<-(MatConfTest[n,n]/sum(MatConfTest[,n]))
    }
    print(TPtest)
    gm<-prod(TPtest)^(1/nclass)
    print(gm)
  }
  teste<-read.arff(file="/home/ever/Base/chess/chess-5-1tst.arff")
  train<-read.arff(file="/home/ever/Base/chess/chess-5-1tra.arff")
  c<-c(complexity(train[which(zeta[[n]]$ams==1),]),irm(train[which(zeta[[n]]$ams==1),]))
  p<-predict(zeta[[n]]$model,newdata=teste)
  c<-c(c,gmeanMC(teste$class,p))
  save(c,file=paste("complexChess",n,".RData",sep=""))
  }

for(x in 1:10){
  for(y in 1:5){
    m<-bagging(class~.,data=train[[y]])
    p<-predict(m,newdata = teste[[y]])
    gm<-c(gm,gmeanMC(teste[[y]]$class,p$class))
  }
  print(gm)
}

tab<-vector()
for(i in 1:20){
  m<-MLP(class~.,data = train[[1]][which(pp[[i]]$ams==1),])
  rJava::.jcache(m$classifier)
  tab<-cbind(tab,predict(m,newdata=teste[[1]]))
}
e<-EnsembleVotingMC(tab)
print(gmeanMC(teste[[1]]$class,e))


0.6034834 0.7733744
