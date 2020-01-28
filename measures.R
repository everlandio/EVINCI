measures<-function(truth,predicted){
  cm<-as.matrix(table(truth,predicted))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  accuracy = sum(diag) / n 
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  return(data.frame(mean(precision),mean(recall),mean(f1)))
}

mAuc<-function(targets,outputs){
  if(is.null(ncol(targets))){
    targets<-decodeClassLabels(targets)
  }
  Auc<-0
  s<-seq(1,nrow(targets))
  tar<-sapply(s,function(x){as.vector(which.max(targets[x,]))})
  out<-sapply(s,function(x){as.vector(which.max(outputs[x,]))})
  
  classes<-unique(tar)
  nClass<-length(classes)
  
  comb<-combn(classes,2,simplify=F)
  for(i in comb){
    index<-which(tar==i[1]|tar==i[2])
    score_i<-outputs[index,i[1]]
    score_k<-outputs[index,i[2]]
    targetsLabel_i<-targets[index,i[1]]
    targetsLabel_k<-targets[index,i[2]]
    Auc<-Auc+calculateAuc(score_i,targetsLabel_i)+calculateAuc(score_k,targetsLabel_k)
  }
  Auc<-Auc/(nClass*(nClass - 1))
  return(Auc)
}

calculateAuc<-function(score,targetsLabel){
  probsSort = sort(score, decreasing = TRUE, index.return = TRUE)
  val = unlist(probsSort$x)
  idx = unlist(probsSort$ix)  
  
  roc_y = targetsLabel[idx];
  stack_x = cumsum(roc_y == 0)/sum(roc_y == 0)
  stack_y = cumsum(roc_y == 1)/sum(roc_y == 1)
  auc = sum((stack_x[2:length(roc_y)]-stack_x[1:length(roc_y)-1])*stack_y[2:length(roc_y)])
  return(auc)
}

gmeanMC<-function(truth,response){
  truth<-factor(max.col(decodeClassLabels(truth)))
  response<-factor(max.col(response),levels = levels(truth))
  cl<-levels(truth)
  nclass<-length(cl)
  MatConfTest<-table(response,truth)
  TPtest<-vector()
  for(n in 1:nclass){
    TPtest[n]<-(MatConfTest[n,n]/sum(MatConfTest[,n]))
  }
  TPtest[is.na(TPtest)]<-0
  # print(TPtest)
  gm<-prod(TPtest)^(1/nclass)
  # print(gm)
  return(list(gm,TPtest))
}