
distCat <- function(data) {
  as.matrix(daisy(data, metric = "gower", stand = TRUE,warnType = F))
}


n1byClass <- function(dst, data) {
  
  classes<-levels(data$class)
  g = graph.adjacency(dst, weighted = TRUE)
  tree = as.matrix(as_adj(mst(as.undirected(g))))
  
  tmp = which(tree != 0, arr.ind = TRUE)
  cons = data[tmp[,1],]$class != data[tmp[,2],]$class
  aux = length(unique(tmp[cons,1]))
  
  n1class<-vector()
  nam<-vector()
  for(i in classes){
    tam<-sum(data[,"class"] == i)
    n1c<-vector()
    for(j in classes){
      aux2<-sum(data[tmp[,1],]$class==i & data[tmp[,2],]$class==j)
      n1c<-c(n1c,aux2/tam)
    }
    n1class<-rbind(n1class,n1c)
  }
  rownames(n1class)<-classes
  colnames(n1class)<-classes
  
  # n1class2 = sapply(classes, FUN=function(c){
  #   aux2 = sum(data[unique(tmp[cons,1]),"class"] == c)
  #   return(aux2/sum(data[,"class"] == c))
  # })
  
  return(list(aux/nrow(data),n1class))
}
