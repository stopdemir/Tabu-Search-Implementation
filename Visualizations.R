

funVisualizationofElectionResultsofEachSolution<-function(SolutionList,ElectionData,beta,n){
  populationVectorAKP<-c()
  populationVectorCHP<-c()
  for(i in 1:length(SolutionList)){
    populationVectorAKP<-append(populationVectorAKP,sum(ElectionData[1,SolutionList[[i]]]))
    populationVectorCHP<-append(populationVectorCHP,sum(ElectionData[2,SolutionList[[i]]]))
  }
  y = 1:length(SolutionList);
 
  plot( y, populationVectorAKP, pch=19, col="blue" )
  par(new=TRUE)
  plot( y, populationVectorCHP, pch=19, col="red" )
  averagePopulation<-sum(ElectionData[nrow(ElectionData),])/n
  #abline(h=averagePopulation,col="purple")
  #abline(h=averagePopulation*(1+beta),col="red")
  #abline(h=averagePopulation*(1-beta),col="red")
  
}

library(rgdal)
Asia<-readOGR(dsn="data5",layer="ASIAASIA")
plot(Asia,col="lightgrey")

for(i in 1:359){
  if(funElectionResults()[1,i]>funElectionResults()[2,i]){
    x<-Asia$ID==i
    plot(Asia[x,],col="yellow",add =TRUE)
  }else{
    x<-Asia$ID==i
    plot(Asia[x,],col="red",add =TRUE)
  }
}

library(choroplethr)
state_choropleth(Asia, title="Asia Demographics", legend="timestamp")











