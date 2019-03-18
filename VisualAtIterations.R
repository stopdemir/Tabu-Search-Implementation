library(rgdal)
library(ggplot2)

funVisualization<-function(SolutionList){
  n=6
  #myColors<-rainbow(n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, alpha = 1)
  #myColors<-heat.colors(n, alpha = 1)
  #myColors<-terrain.colors(n, alpha = 1)
  #myColors<-topo.colors(n, alpha = 1)
  myColors<-cm.colors(n, alpha = 1)
  #myColors<-c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7", "#CC79A7")
  turkey<-readOGR(dsn = "/home/stopdemir/data2",layer = "TUR_adm1")
  turkey$ID_1<-as.numeric(as.factor(turkey$ID_1))
  plot(turkey,col="lightgrey")
  
  for(i in 1:length(SolutionList)){
    for(j in 1:length(SolutionList[[i]])){
      a<-turkey$ID_1==SolutionList[[i]][j]
      plot(turkey[a,],col=myColors[i],add =TRUE) 
    }
  }
}

turkey<-readOGR(dsn="data2",layer="TUR_adm1")
turkey$ID_1<-as.numeric(as.factor(turkey$ID_1))
plot(turkey,col="lightgrey")

  x1<-c(21,47,71,35,1,7,17,77,49,63,18,15,11)
  x2<-c(36,52,38,26,2,75,6,55,19,16,56,33)
  x3<-c(80,39,8,42,12,23,46,48,79,68,81,64)
  x4<-c(50,3,78,58,59,54,40,34,31,61,43,37,28,44,45,65)
  x5<-c(25,4,5,62,10,73,27,24,29,60,70,72,51,22,74)
  x6<-c(30,69,76,66,53,9,14,57,67,32,41,20,13)


for(i in 1:length(x1)){
  a<-turkey$ID_1==x1[i]
  plot(turkey[a,],col="seagreen1",add =TRUE) 
}
  
  for(i in 1:length(x2)){
    a<-turkey$ID_1==x2[i]
    plot(turkey[a,],col="violetred1",add =TRUE) 
  }
  
  for(i in 1:length(x3)){
    a<-turkey$ID_1==x3[i]
    plot(turkey[a,],col="turquoise1",add =TRUE) 
  }
  
  for(i in 1:length(x4)){
    a<-turkey$ID_1==x4[i]
    plot(turkey[a,],col="mediumorchid1",add =TRUE) 
  }
  
  for(i in 1:length(x5)){
    a<-turkey$ID_1==x5[i]
    plot(turkey[a,],col="greenyellow",add =TRUE) 
  }
  
  for(i in 1:length(x6)){
    a<-turkey$ID_1==x6[i]
    plot(turkey[a,],col="brown1",add =TRUE) 
  }
  