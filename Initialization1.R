library(readxl)
library(xlsx)

InitializationFunc1<-function(AdjMatrix, ElectResultMatrix,teta,n){
  unitNumber<-nrow(AdjMatrix)
  AdjMatrixx<-funAdjacency()
  ElectResultMatrix<-funElectionResults()
  FinishedOnes<-c()
  averagePopulation<-sum(ElectResultMatrix[nrow(ElectResultMatrix),])/n
  ResultList<-list()
  a<-c(1:unitNumber)
  
  while(length(FinishedOnes)<nrow(AdjMatrix)){
    
    solutionVector<-c()
    potentialOnes<-c();
    unUsed<-setdiff(a, FinishedOnes)
    if(length(unUsed)==1){
      x<-unUsed[1]
    }else{
      x<-sample(unUsed,1)
    }
    FinishedOnes<-append(FinishedOnes,x)
    solutionVector<-append(solutionVector,x)
    potentialOnes<-FindAdjacentUnits(x,AdjMatrixx,FinishedOnes)

    
    while(PopulationNumFunc(solutionVector,ElectResultMatrix)<averagePopulation*teta && length(potentialOnes)>0){
      
      if(length(potentialOnes)==1){
        x<-potentialOnes[1]
      }else{
        x<-sample(potentialOnes,1)
      }
      FinishedOnes<-append(FinishedOnes,x)
      solutionVector<-append(solutionVector,x)
      potentialOnes<-FindAdjacentUnits(x,AdjMatrixx,FinishedOnes)
    }

    ResultList<-append(ResultList,list(solutionVector))
  }
  return(ResultList)
}  




FindAdjacentUnits<-function(x,adjMatrix,Finisheds){
  adjacentVector<-c()
  for(i in 1:nrow(adjMatrix)){
      if(adjMatrix[(x),(i)]==1){
        adjacentVector<-append(adjacentVector,i)
    }
  }
  adjacentVector<-setdiff(adjacentVector,x)
  dummyVector<-c()
  for(i in 1:length(adjacentVector)){
    for(j in 1:length(Finisheds)){
      if(adjacentVector[i]==Finisheds[j]){
        dummyVector<-append(dummyVector,adjacentVector[i])
      }
    }
  }
  for(k in 1:length(dummyVector)){
    adjacentVector<-setdiff(adjacentVector,dummyVector[k])
  }
  return(adjacentVector)
}



PopulationNumFunc<-function(solVector,electionMatrix){
  pop<-0;
    pop=sum(electionMatrix[6,solVector])

  return(pop)
  }




funAdjacency <- function() {
  #AdjMatrix <- as.matrix(read_excel("~/LondonAdjacency.xlsx"))
  #AdjMatrix <- as.matrix(read_excel("~/FatihAdjacency.xlsx")) 
  AdjMatrix <- as.matrix(read_excel("~/Asia Adjacency Data.xlsx")) 
  #AdjMatrix <- as.matrix(read_excel("~/TurkeyAdjacency.xlsx"))
  #AdjMatrix<-AdjMatrix[63:nrow(AdjMatrix),63:ncol(AdjMatrix)]

 return(AdjMatrix)
}

funElectionResults <- function() {
  #ElectionResults <- as.matrix(read_excel("~/LondonElectionResults.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/FatihElection.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/Asia November Election Data.xlsx")) 
  ElectionResults <- as.matrix(read_excel("~/Asia November Election Data Fake.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/TurkeyElection.xlsx")) 
  #ElectionResults<-ElectionResults[,63:ncol(ElectionResults)]

  return(ElectionResults)
}