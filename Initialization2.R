library(readxl)
library(xlsx)

InitializationFunc2<-function(AdjMatrix, ElectResultMatrix,teta,n){
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
    
    
    while(PopulationNumFunc(solutionVector,ElectResultMatrix)<teta*averagePopulation && length(potentialOnes)>0)
    {
      if(length(potentialOnes)==1){
        x<-potentialOnes[1]
      }else{
        x<-sample(potentialOnes,1)
      }
      FinishedOnes<-append(FinishedOnes,potentialOnes)
      solutionVector<-append(solutionVector,potentialOnes)
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
  if(!(x %in% adjacentVector)){
    adjacentVector<-append(adjacentVector,x)
  }
  
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
  adjacentVector<-setdiff(adjacentVector,x)
  
  return(adjacentVector)
}


PopulationNumFunc<-function(solVector,electionMatrix){
  pop<-0;
  pop=sum(electionMatrix[6,solVector])
  
  return(pop)
}

# to have the adjacency data from excel
funAdjacency <- function() {
  #AdjMatrix <- as.matrix(read_excel("~/LondonAdjacency.xlsx"))
  #AdjMatrix <- as.matrix(read_excel("~/FatihAdjacency.xlsx")) 
  AdjMatrix <- as.matrix(read_excel("~/Asia Adjacency Data.xlsx")) 
  #AdjMatrix <- as.matrix(read_excel("~/TurkeyAdjacency.xlsx")) 
  #AdjMatrix<-AdjMatrix[63:nrow(AdjMatrix),63:ncol(AdjMatrix)]
  
  return(AdjMatrix)
}

# to have the election data from excel
funElectionResults <- function() {
  #ElectionResults <- as.matrix(read_excel("~/LondonElectionResults.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/FatihElection.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/Asia November Election Data.xlsx")) 
  ElectionResults <- as.matrix(read_excel("~/Asia November Election Data Fake.xlsx"))
  #ElectionResults <- as.matrix(read_excel("~/TurkeyElection.xlsx")) 
  #ElectionResults<-ElectionResults[,63:ncol(ElectionResults)]
  
  return(ElectionResults)
}


funSecondStepforInitialization<-function(SolutionList,n){
  electionMatrix<-funElectionResults()
  while(length(SolutionList)>n){
    x<-funFindTheClusterThatHasLeastPopulation(electionMatrix,SolutionList)
    y<-funFindTheClusterThatHasLeastPopulationandNeighborToX(electionMatrix,SolutionList,x[[2]])
    SolutionList[[x[[1]]]]<-NULL
    if(y[[1]]>x[[1]]){
      SolutionList[[y[[1]]-1]]<-NULL
    }else{
      SolutionList[[y[[1]]]]<-NULL
    }
    newOne<-c(x[[2]],y[[2]])
    SolutionList<-append(SolutionList,list(newOne))
  }
  
  return(SolutionList)
  
}

funFindTheClusterThatHasLeastPopulation<-function(ElectResultMatrix,SolutionList){
  ResultList<-list()
  popVector<-c()
  for(i in 1:length(SolutionList)){
    pp<-PopulationNumFunc(SolutionList[[i]],ElectResultMatrix)
    popVector<-append(popVector,pp)
  }
  minOneIndex<-which.min(popVector)
  
  ResultList<-append(ResultList,list(minOneIndex))
  ResultList<-append(ResultList,list(SolutionList[[minOneIndex]]))

  return(ResultList)
}


funFindTheClusterThatHasLeastPopulationandNeighborToX<-function(electionMatrix,SolutionList,x){
  adjacentListToX<-list()
  adjacentIndexVector<-c()
  adjacentMat<-funAdjacency()
  Result_List<-list()
  for(i in 1:length(SolutionList)){
    if(funAdjacencyControl(x,SolutionList[[i]],adjacentMat)==TRUE){
      adjacentListToX<-append(adjacentListToX,list(SolutionList[[i]]))
      adjacentIndexVector<-append(adjacentIndexVector,i)
    }
  }
  for(i in 1:length(adjacentListToX)){
    if( x==adjacentListToX[[i]]){
      adjacentListToX[[i]]<-NULL
      adjacentIndexVector<-setdiff(adjacentIndexVector,adjacentIndexVector[i])
      break();
    }
  }
  Result_List<-list()
  popVector<-c()
  for(i in 1:length(adjacentListToX)){
    pp<-PopulationNumFunc(adjacentListToX[[i]],electionMatrix)
    popVector<-append(popVector,pp)
  }
  minOneIndex<-which.min(popVector)
  
  Result_List<-append(Result_List,list(adjacentIndexVector[minOneIndex]))
  Result_List<-append(Result_List,list(adjacentListToX[[minOneIndex]]))
  
  return(Result_List)
  
}


funAdjacencyControl<-function(x,y,AdjacencyMatrix){
  key=FALSE;
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      if(AdjacencyMatrix[x[i],y[j]]==1){
        key=TRUE;
        j=length(y);
        i=length(x);
        break();
      }
    }
  }
  return(key);
}
#######################################################################################
#Feasibility Check


funFeasibilityControl<-function(SolutionList,AdjacencyMatrix,ElectionMatrix,beta){
  
  #continuity##################################################################
  Contotal=0;
  ConKey=FALSE;
  for(i in 1:length(SolutionList)){
    if(funContiguityControl(SolutionList[[i]],AdjacencyMatrix)==TRUE){
      Contotal=Contotal+1
    }
  }
  if(Contotal==length(SolutionList)){
    ConKey=TRUE;
  }
  #cat("Number of Continuous Units",Contotal)
 
  #population constraint#################################################################3
  PopTotal=0;
  PopKey=TRUE;
  populationVector<-c()
  averagePopulation<-sum(ElectionMatrix[nrow(ElectionMatrix),])/(length(SolutionList))
  for(i in 1:length(SolutionList)){
    populationVector<-append(populationVector,sum(ElectionMatrix[nrow(ElectionMatrix),SolutionList[[i]]]))
  }
  for(i in 1:length(populationVector)){
    if(populationVector[i]>(averagePopulation*(1+beta)) || populationVector[i]<(averagePopulation*(1-beta))){
      PopKey=FALSE;
      PopTotal=PopTotal+1;
    }
  }
  print(length(SolutionList)-PopTotal)
  resultList<-list()
  resultList<-append(resultList,ConKey)
  resultList<-append(resultList,PopKey)
  resultList<-append(resultList,length(SolutionList)-PopTotal)
  return(resultList)
}


    funThirdStepforInitialization<-function(SolutionList,adjMatrix,elecMatrix,beta){
      
      n<-length(SolutionList);
      averagePopulation<-sum(elecMatrix[nrow(elecMatrix),])/n
      t<-funFeasibilityControl(SolutionList,adjMatrix,elecMatrix,beta);
      iteration<-0;
      while(t[[3]]<30 && iteration <60 ){
        mostPopOnes<-funFindTheClustersThatHasOverPopValue(elecMatrix,SolutionList,beta)
        if(length(mostPopOnes)==1){
          sayý<-1
        }
        else{
          sayý<-sample(1:length(mostPopOnes),1)
        }
        iteration<-iteration+1;
        mostPopOne<-mostPopOnes[[sayý]]
        potentialOnes<-funFindTheClustersThatAreUnderPopulationAndNeighborToX(elecMatrix,SolutionList,mostPopOne[[2]],beta)
        if(length(potentialOnes)==1){
          no<-1
        }
        else{
          no<-sample(1:length(potentialOnes),1)
        }
        iteration<-iteration+1;
        potentialOne<-potentialOnes[[no]]
        PopofPotential<-sum(elecMatrix[nrow(elecMatrix),potentialOne[[2]]])
        while(PopofPotential<averagePopulation*(1-beta) ){
          adjacentUnits<-funFindAdjacentUnits(mostPopOne[[2]],potentialOne[[2]],adjMatrix)
          if(length(adjacentUnits)==1){
            choo<-1;
          }else(
            choo<-sample(1:length(adjacentUnits),1)
          )
          x<-adjacentUnits[[choo]][1]
          mostPopOne[[2]]<-setdiff(mostPopOne[[2]],x)
          potentialOne[[2]]<-append(potentialOne[[2]],x)
          SolutionList[[mostPopOne[[1]]]]<-mostPopOne[[2]]
          SolutionList[[potentialOne[[1]]]]<-potentialOne[[2]]
          PopofPotential<-sum(elecMatrix[nrow(elecMatrix),potentialOne[[2]]])
        }
        t<-funFeasibilityControl(SolutionList,adjMatrix,elecMatrix,beta);
      }
      return(SolutionList)
    }

    
    funFindTheClusterThatHasMostPopulation<-function(ElectResultMatrix,SolutionList){
      ResultList<-list()
      popVector<-c()
      for(i in 1:length(SolutionList)){
        pp<-PopulationNumFunc(SolutionList[[i]],ElectResultMatrix)
        popVector<-append(popVector,pp)
      }
      maxOneIndex<-which.max(popVector)
      
      ResultList<-append(ResultList,list(maxOneIndex))
      ResultList<-append(ResultList,list(SolutionList[[maxOneIndex]]))
      
      return(ResultList)
    }
    
    funFindTheClustersThatHasOverPopValue<-function(ElectResultMatrix,SolutionList,beta){
      ResultList<-list()
      popVector<-c()
      overOnes<-c()
      n<-length(SolutionList)
      averagePopulation<-sum(ElectResultMatrix[nrow(ElectResultMatrix),])/n
      for(i in 1:length(SolutionList)){
        pp<-PopulationNumFunc(SolutionList[[i]],ElectResultMatrix)
        popVector<-append(popVector,pp)
      }
      for(i in 1:length(popVector)){
        if(popVector[i]>averagePopulation*(1+beta)){
          overOnes<-append(overOnes,i)
        }
      }
      for(i in 1:length(overOnes)){
        e<-list()
        e<-append(e,list(overOnes[i]))
        e<-append(e,list(SolutionList[[overOnes[i]]]))
        ResultList<-append(ResultList,list(e))
      }
      return(ResultList)
    }


    
    funFindTheClustersThatAreUnderPopulationAndNeighborToX<-function(electionMatrix,SolutionList,x,beta){
      adjacentListToX<-list()
      adjacentIndexVector<-c()
      adjacentMat<-funAdjacency()
      underOnes<-c()
      n<-length(SolutionList)
      averagePopulation<-sum(electionMatrix[nrow(electionMatrix),])/n
      
      for(i in 1:length(SolutionList)){
        if(funAdjacencyControl(x,SolutionList[[i]],adjacentMat)==TRUE){
          adjacentListToX<-append(adjacentListToX,list(SolutionList[[i]]))
          adjacentIndexVector<-append(adjacentIndexVector,i)
        }
      }
      for(i in 1:length(adjacentListToX)){
        if( x==adjacentListToX[[i]]){
          adjacentListToX[[i]]<-NULL
          adjacentIndexVector<-setdiff(adjacentIndexVector,adjacentIndexVector[i])
          break();
        }
      }
      Result_List<-list()
      popVector<-c()
      for(i in 1:length(adjacentListToX)){
        pp<-PopulationNumFunc(adjacentListToX[[i]],electionMatrix)
        popVector<-append(popVector,pp)
      }
      
      for(i in 1:length(popVector)){
        if(popVector[i]<averagePopulation*(1-beta)){
          underOnes<-append(underOnes,i)
        }
      }
      if(length(underOnes)>=1){
      for(i in 1:length(underOnes)){
        e<-list()
        e<-append(e,list(adjacentIndexVector[underOnes[i]]))
        e<-append(e,list(adjacentListToX[[underOnes[i]]]))
        Result_List<-append(Result_List,list(e))
      }
      }
      else{
        minOneIndex<-which.min(popVector)
        e<-list()
        e<-append(e,list(adjacentIndexVector[minOneIndex]))
        e<-append(e,list(adjacentListToX[[minOneIndex]]))
        Result_List<-append(Result_List,list(e))
      }
      return(Result_List)
      
    }




  funVisualizationofPopulationNumbers<-function(SolutionList,ElectionData,beta,n){
    populationVector<-c()
    for(i in 1:length(SolutionList)){
      populationVector<-append(populationVector,sum(ElectionData[nrow(ElectionData),SolutionList[[i]]]))
    }
    y = 1:length(SolutionList);
    plot(y,populationVector);
    averagePopulation<-sum(ElectionData[nrow(ElectionData),])/n
    abline(h=averagePopulation,col="purple")
    abline(h=averagePopulation*(1+beta),col="red")
    abline(h=averagePopulation*(1-beta),col="red")
    
  }
  
  

  funFindAdjacentUnits <- function(x,y,AdjacencyMatrix) {
    outputList<-list()
    for(i in 1:length(x)){
      for(j in 1:length(y)){
        if(AdjacencyMatrix[(x[i]),(y[j])]==1){
          #outputforAdjacents<-rbind(outputforAdjacents,c(x[i],y[j]))
          outputList<-append(outputList,list(c(x[i],y[j])))
        }
      }
    }
    #outputforAdjacents<- outputforAdjacents[-1:-1,]
    return(outputList)
  }

  funContiguityControl2<-function(x,AdjacencyMatrix){
    control=FALSE;
    newMatrix<-AdjacencyMatrix[x,x]
    gr1 <- graph.adjacency(newMatrix,mode ="undirected")
    if(vertex.connectivity(gr1)==1){
      control=TRUE;
    }
    return(control);
  }



















