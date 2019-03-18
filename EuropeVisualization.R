library(rgdal)
Europe<-readOGR(dsn="data6",layer="europe1")
Europe$ID<-1:nrow(Europe)
plot(Europe,col="lightgrey")


#points(orstationc$lon, orstationc$lat)
text(coordinates(Europe)[,1], coordinates(Europe)[,2], labels=Europe$ID, col="red",
     cex=.05)

trueCentroids = gCentroid(Europe,byid=TRUE)
plot(Europe)
points(coordinates(Europe),pch=1)
points(trueCentroids,pch=2)



for(i in 359:359){
  x<-Europe$ID==i
  plot(Europe[x,],col="turquoise",add =TRUE)
}
for(i in 320:320){
  x<-Europe$ID==i
  plot(Europe[x,],col="red",add =TRUE)
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
  Europe<-readOGR(dsn = "data5",layer = "EuropeEurope")
  Europe$ID<-as.numeric(as.factor(Europe$ID))
  Europe$ID<-1:nrow(Europe)
  plot(Europe,col="lightgrey")
  
  for(i in 1:length(SolutionList)){
    for(j in 1:length(SolutionList[[i]])){
      a<-Europe$ID==SolutionList[[i]][j]
      plot(Europe[a,],col=myColors[i],add =TRUE) 
    }
  }
  text(coordinates(Europe)[,1], coordinates(Europe)[,2], labels=Europe$ID, col="black",
       cex=.20)
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
Europe<-readOGR(dsn="data6",layer="europe1")
plot(Europe,col="lightgrey")

for(i in rrrr[[20]]){
  x<-Europe$ID==i
  plot(Europe[x,],col="yellow",add =TRUE)
}
for(i in rrrr[[29]]){
  x<-Europe$ID==i
  plot(Europe[x,],col="red",add =TRUE)
}


text(coordinates(Europe)[,1], coordinates(Europe)[,2], labels=Europe$ID, col="black",
     cex=.30)





#to color the districts

ColorFunction<-function(){
  disList<-list()
  a1<-c(1:35)
  a2<-c(36:74)
  a3<-c(75:112)
  a4<-c(113:136)
  a5<-c(137:146)
  a6<-c(147:166)
  a7<-c(167:176)
  a8<-c(177:186)
  a9<-c(187:214)
  a10<-c(215:252)
  a11<-c(253:271)
  a12<-c(272:296)
  a13<-c(297:319)
  a14<-c(320:364)
  a15<-c(365:421)
  a16<-c(422:436)
  a17<-c(437:452)
  a18<-c(453:463)
  a19<-c(464:476)
  a20<-c(477:492)
  a21<-c(493:503)
  a22<-c(504:525)
  a23<-c(526:536)
  a24<-c(537:557)
  a25<-c(558:572)
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
  disList<-append(disList,list(a14))
  disList<-append(disList,list(a15))
  disList<-append(disList,list(a16))
  disList<-append(disList,list(a17))
  disList<-append(disList,list(a18))
  disList<-append(disList,list(a19))
  disList<-append(disList,list(a20))
  disList<-append(disList,list(a21))
  disList<-append(disList,list(a22))
  disList<-append(disList,list(a23))
  disList<-append(disList,list(a24))
  disList<-append(disList,list(a25))
  
  Europe<-readOGR(dsn = "data6",layer = "europe1")
  Europe$ID<-1:nrow(Europe)
  plot(Europe,col="lightgrey")
  #points(orstationc$lon, orstationc$lat)
  
  n<-length(disList)
  myColors<-rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
  for(i in 1:length(disList)){
    for(j in 1:length(disList[[i]])){
      a<-Europe$ID==disList[[i]][j]
      plot(Europe[a,],col=myColors[i],add =TRUE) 
    }
  }
}
  text(coordinates(Europe)[,1], coordinates(Europe)[,2], labels=Europe$ID, col="black",
       cex=.15)
}



