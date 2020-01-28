
#cria amostras balanceadas ou proximas do balanceamento

amostMC3 <- function(database,popsize,alfa){
  
  #alfa is used to calculate the amount of example in each class
  
  #gera o vetor primario
  ret<-sample(0,nrow(database),replace=TRUE)
  
  cl<-names(table(as.vector(database$class)))
  from<-min(table(database$class))*0.5
  
  #faz com que pelo menos uma quantidade minima de elementos
  #de cada classe seja selecionado (baseado no tamanho da classe minoritaria)
  for(i in cl){
    vec<-which(database$class==i)
    to<-length(vec)-1
    if(to>=(10*from))
      to<-10*from
    tam<-round(seq(from,to,length.out = popsize))[alfa]
    v<-sample(vec,tam)
    ret[v]<-1
  }
  return(ret)
}
  