
offspringMC<- function(pop,motherIndices,fatherIndices,database,
                       CrossoverProbability=1,mutationProbability=0.1){
  tam<-length(pop[[1]]$ams)
  filhosPop <- list()
  z<-1
  for(n in 1:length(motherIndices)){
    f1<-pop[[motherIndices[n]]]
    f2<-pop[[fatherIndices[n]]]
    if(runif(1)<=CrossoverProbability){
      mae<-pop[[motherIndices[n]]]$ams
      pai<-pop[[fatherIndices[n]]]$ams
      cl<-names(table(as.vector(database$class)))
      for(i in cl){
        id.class<-which(database$class==i)
        tamClass<-length(id.class)
        vec1<-mae[id.class]
        vec2<-pai[id.class]
        cross<-sample(tamClass-1,1)
        vec3<-c(vec1[1:cross],vec2[(cross+1):tamClass])
        if(sum(vec3)==0){
          id<-sample(tamClass,1)
          vec3[id]<-1
        }
        f1$ams[id.class]<-vec3
        vec4<-c(vec2[1:cross],vec1[(cross+1):tamClass])
        if(sum(vec4)==0){
          id<-sample(tamClass,1)
          vec4[id]<-1
        }
        f2$ams[id.class]<-vec4
      }
    }
    if(runif(1)<mutationProbability){
      f1$ams<-mutationMC(f1$ams)
    }
    filhosPop[[z]]<-f1
    z<-z+1
    if(runif(1)<mutationProbability){
      f2$ams<-mutationMC(f2$ams)
    }
    filhosPop[[z]]<-f2
    z<-z+1
  }
  return(filhosPop)
}

mutationMC<-function(ind){
  tam<-length(ind)
  porc<-runif(1,min=0.01,max=0.2)
  muTam<-ceiling(tam*porc)
  # muTam<-sample(ceiling(tam/10),1)
  muVec<-sample(1:tam,muTam)
  for(m in muVec){
    if(ind[m]==0) {
      ind[m]<-1}
    else {ind[m]<-0}
  }
  return(ind)
}
