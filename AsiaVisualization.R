library(rgdal)
Asia<-readOGR(dsn="data5",layer="ASIAASIA")
plot(Asia,col="lightgrey")

#points(orstationc$lon, orstationc$lat)
text(coordinates(Asia)[,1], coordinates(Asia)[,2], labels=Asia$ID, col="red",
     cex=.05)

trueCentroids = gCentroid(Asia,byid=TRUE)
plot(Asia)
points(coordinates(Asia),pch=1)
points(trueCentroids,pch=2)



for(i in 359:359){
  x<-Asia$ID==i
  plot(Asia[x,],col="turquoise",add =TRUE)
}
for(i in 320:320){
  x<-Asia$ID==i
  plot(Asia[x,],col="red",add =TRUE)
}
library(rgdal)
library(ggplot2)

funVisualization<-function(SolutionList){
  n=length(SolutionList)
  myColors<-rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
  #myColors<-heat.colors(n, alpha = 1)
  #myColors<-terrain.colors(n, alpha = 1)
  #myColors<-topo.colors(n, alpha = 1)
  #myColors<-cm.colors(n, alpha = 1)
  #myColors<-c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7", "#CC79A7")
  ASIA<-readOGR(dsn = "data5",layer = "ASIAASIA")
  ASIA$ID<-as.numeric(as.factor(ASIA$ID))
  plot(ASIA,col="lightgrey")
  
  for(i in 1:length(SolutionList)){
    for(j in 1:length(SolutionList[[i]])){
      a<-ASIA$ID==SolutionList[[i]][j]
      plot(ASIA[a,],col=myColors[i],add =TRUE) 
    }
  }
  #text(coordinates(Asia)[,1], coordinates(Asia)[,2], labels=Asia$ID, col="black",
    #   cex=.30)
}




funINITALL<-function(adjmat,electmat,teta,n,beta){
  resultList<-list();
  key=TRUE;
  while(key){
    rr<-InitializationFunc1(adjmat,electmat,teta,n)
    rrr<-funSecondStepforInitialization(rr,n)
    e<-funFeasibilityControl(rrr,adjmat,electmat,beta)
    if(e[[3]]>20){
      rrr<-funThirdStepforInitialization(rrr,adjmat,electmat,beta)
    }
    if(e[[1]]==TRUE && e[[3]]>26){
      key=FALSE
      resultList<-rrr;
    }
  }
  return(resultList)
}



library(rgdal)
Asia<-readOGR(dsn="data5",layer="ASIAASIA")
plot(Asia,col="lightgrey")

for(i in rrrr[[20]]){
  x<-Asia$ID==i
  plot(Asia[x,],col="yellow",add =TRUE)
}
for(i in rrrr[[29]]){
  x<-Asia$ID==i
  plot(Asia[x,],col="red",add =TRUE)
}


text(coordinates(Asia)[,1], coordinates(Asia)[,2], labels=Asia$ID, col="black",
     cex=.30)





#to color the districts

ColorFunction<-function(){
  disList<-list()
  a1<-c(1:62)
  a2<-c(63:(63+44))
  a3<-c(108:(108+20))
  a4<-c(129:(129+18))
  a5<-c(148:(148+14))
  a6<-c(163:(163+35))
  a7<-c(199:(199+16))
  a8<-c(216:(216+32))
  a9<-c(249:(249+34))
  a10<-c(284:(284+16))
  a11<-c(301:(301+20))
  a12<-c(322:(322+17))
  a13<-c(340:(340+19))
  disList<-append(disList,list(a1))
  disList<-append(disList,list(a2))
  disList<-append(disList,list(a3))
  disList<-append(disList,list(a4))
  disList<-append(disList,list(a5))
  disList<-append(disList,list(a6))
  disList<-append(disList,list(a7))
  disList<-append(disList,list(a8))
  disList<-append(disList,list(a9))
  disList<-append(disList,list(a10))
  disList<-append(disList,list(a11))
  disList<-append(disList,list(a12))
  disList<-append(disList,list(a13))
  
  ASIA<-readOGR(dsn = "data5",layer = "ASIAASIA")
  plot(ASIA,col="lightgrey")
  #points(orstationc$lon, orstationc$lat)

  n<-length(disList)
  myColors<-rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
  for(i in 1:length(disList)){
    for(j in 1:length(disList[[i]])){
      a<-ASIA$ID==disList[[i]][j]
      plot(ASIA[a,],col=myColors[i],add =TRUE) 
    }
  }
  text(coordinates(Asia)[,1], coordinates(Asia)[,2], labels=Asia$ID, col="black",
       cex=.25)
}
  


