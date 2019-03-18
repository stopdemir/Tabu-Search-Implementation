library(readxl)
library(xlsx)
library(igraph)



#the adjacency data must be taken from excel sheet
funAdjacency <- function() {
  #AdjMatrix <- as.matrix(read_excel("~/LondonAdjacency.xlsx"))
  #AdjMatrix <- as.matrix(read_excel("~/FatihAdjacency.xlsx")) 
  #AdjMatrix <- as.matrix(read_excel("~/TurkeyAdjacency.xlsx"))
  AdjMatrix <- as.matrix(read_excel("~/Asia Adjacency Data.xlsx"))
  return(AdjMatrix)
}

#election results for units
funElectionResults <- function() {
  #ElectionResults <- as.matrix(read_excel("~/LondonElectionResults.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/FatihElection.xlsx")) 
  #ElectionResults <- as.matrix(read_excel("~/Asia November Election Data Fake.xlsx"))
  ElectionResults <- as.matrix(read_excel("~/Asia November Election Data Fake_2.xlsx"))
  #ElectionResults <- as.matrix(read_excel("~/TurkeyElection.xlsx")) 
  
  
  return(ElectionResults)
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
  if(vertex.connectivity(gr1)>=1){
    control=TRUE;
  }
  return(control);
}

#The outputs of this method are all the districs 
funGenerateUsingNeigh1<-function(x,y,AdjacencyList,CurrentListF,theOnesUsedF){
  pastCheck<-c();
  GeneratedResults<-list()
  FinalListofLists<-list()
  for(i in 1:length(AdjacencyList)){
    if(!(AdjacencyList[[i]][1] %in% pastCheck))
    {
      xx<-setdiff(x,AdjacencyList[[i]][1])
      yy<-append(y,AdjacencyList[[i]][1])
      pastCheck<-append(pastCheck,AdjacencyList[[i]][1])
      if(funContiguityControl2(xx,funAdjacency()) && funContiguityControl2(yy,funAdjacency())){
        newFinalList<-list()
        newFinalList<-list(xx,yy)
        a<-theOnesUsedF[1]
        b<-theOnesUsedF[2]
        CurrentListNew<-CurrentListF[c(-a,-b)]
        oneSolutionList<-list()
        oneSolutionList<-append(CurrentListNew,newFinalList)
        FinalListofLists<-append(FinalListofLists,list(oneSolutionList))
      }
    }
    if(!(AdjacencyList[[i]][2] %in% pastCheck))
    {
      yyy<-setdiff(y,AdjacencyList[[i]][2])
      xxx<-append(x,AdjacencyList[[i]][2])
      pastCheck<-append(pastCheck,AdjacencyList[[i]][2])
      if(funContiguityControl2(xxx,funAdjacency()) && funContiguityControl2(yyy,funAdjacency())){
        newFinalListOther<-list()
        newFinalListOther<-list(xxx,yyy)
        a<-theOnesUsedF[1]
        b<-theOnesUsedF[2]
        CurrentListNew<-CurrentListF[c(-a,-b)]
        oneSolutionListOther<-list()
        oneSolutionListOther<-append(CurrentListNew,newFinalListOther)
        FinalListofLists<-append(FinalListofLists,list(oneSolutionListOther))
      }
    }
  }
  return(FinalListofLists)
}

funGenerateUsingNeigh1Alternative<-function(x,y,AdjacencyList){
  pastCheck<-c();
  FinalList<-list()
  for(i in 1:length(AdjacencyList)){
    if(!(AdjacencyList[[i]][1] %in% pastCheck))
    {
      xx<-setdiff(x,AdjacencyList[[i]][1])
      yy<-append(y,AdjacencyList[[i]][1])
      pastCheck<-append(pastCheck,AdjacencyList[[i]][1])
      if(funContiguityControl2(xx,funAdjacency()) && funContiguityControl2(yy,funAdjacency())){
        newSolution<-list()
        newSolution<-list(xx,yy)
        FinalList<-append(FinalList,list(newSolution))
      }
    }
    if(!(AdjacencyList[[i]][2] %in% pastCheck))
    {
      yyy<-setdiff(y,AdjacencyList[[i]][2] )
      xxx<-append(x,AdjacencyList[[i]][2] )
      pastCheck<-append(pastCheck,AdjacencyList[[i]][2] )
      if(funContiguityControl2(xxx,funAdjacency()) && funContiguityControl2(yyy,funAdjacency())){
        newSolutionOther<-list()
        newSolutionOther<-list(xxx,yyy)
        FinalList<-append(FinalList,list(newSolutionOther))
      }
    }
  }
  return(FinalList)
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

#################################
########Another neighborhood change structure-N2-2-Swap
funGenerateUsingNeigh2<-function(x,y,AdjacencyList){
  FinalList<-list()
  for(i in 1:length(AdjacencyList)){
    xx<-setdiff(x,AdjacencyList[[i]][1])
    xxx<-append(xx,AdjacencyList[[i]][2])
    yy<-setdiff(y,AdjacencyList[[i]][2] )
    yyy<-append(yy,AdjacencyList[[i]][1])
    if(funContiguityControl2(xxx,funAdjacency()) && funContiguityControl2(yyy,funAdjacency())){
      newSolution<-list()
      newSolution<-list(xxx,yyy)
      FinalList<-append(FinalList,list(newSolution))
    }
  }
  return(FinalList)
}

#########################################
#more general way to find neighbours
funGenerateUsingNeigh3<-function(x,y,AdjacencyList){
  
  firstDistrict<-c()
  secondDistrict<-c()
  for(i in 1:length(AdjacencyList)){
    firstDistrict<-append(firstDistrict,AdjacencyList[[i]][1])
    secondDistrict<-append(secondDistrict,AdjacencyList[[i]][2])
  }
  firstDistrict<-unique(firstDistrict)
  secondDistrict<-unique(secondDistrict)
  FinalList<-list()
  for(i in 1:length(firstDistrict)){
    for(j in 1:length(secondDistrict)){
      xx<-setdiff(x,firstDistrict[i])
      xxx<-append(xx,secondDistrict[j])
      yy<-setdiff(y,secondDistrict[j] )
      yyy<-append(yy,firstDistrict[i])
      if(funContiguityControl2(xxx,funAdjacency()) && funContiguityControl2(yyy,funAdjacency())){
        newSolution<-list()
        newSolution<-list(xxx,yyy)
        FinalList<-append(FinalList,list(newSolution))
      }
    }
  }
  return(FinalList)
}

#############################################################################3
GENERATE_NEIGHBORS<-function(CurrentList,BETA,adjMatrix,elecMat){
  key<-FALSE;
  BigKey<-FALSE;
  
  while(!BigKey){
  FINALLISTS<-list()
  FINALFINAL<-list()
  AdjLIST<-list()
  theOnesUsed<-c()
  addedRest<-c()
  while(!key){
    randomNumbers <- sample(1:length(CurrentList), 2, replace=F)
    key<-funAdjacencyControl(CurrentList[[randomNumbers[1]]],CurrentList[[randomNumbers[2]]],adjMatrix)
  }
  theOnesUsed<-randomNumbers;
  key<-FALSE;
  AdjLIST<-funFindAdjacentUnits(CurrentList[[theOnesUsed[1]]],CurrentList[[theOnesUsed[2]]],adjMatrix)
  #REST<-CurrentList[c((-1*theOnesUsed[1]),(-1*theOnesUsed[2]))]
  #FINALLISTS<-funGenerateUsingNeigh1Alternative(CurrentList[[theOnesUsed[1]]],CurrentList[[theOnesUsed[2]]],AdjLIST)
  #FINALLISTS<-funGenerateUsingNeigh2(CurrentList[[theOnesUsed[1]]],CurrentList[[theOnesUsed[2]]],AdjLIST)
  FINALLISTS<-funGenerateUsingNeigh3(CurrentList[[theOnesUsed[1]]],CurrentList[[theOnesUsed[2]]],AdjLIST)
  if(length(FINALLISTS)>0){
  FINALFINAL<-funPopulationConstraint(FINALLISTS,elecMat,BETA,length(CurrentList))}
  if(length(FINALFINAL)>0){
   BigKey=TRUE;
  }
  }
  newList <- list("USED" = theOnesUsed,"FINALRESULT" = FINALFINAL)
  #newList <- list("USED" = theOnesUsed,"FINALRESULT" = FINALLISTS)
  return(newList)
}


###########################################################################################

funPopulationConstraint<-function(CandidateList,PopulationData,BetaValue,NumberofDist){
  averagePopulation<-sum(PopulationData[nrow(PopulationData),])/NumberofDist
  whichones<-c()
  for(j in 1:length(CandidateList)){
    d<-c(0,0)
    for(k in 1:2){
      #dumm<-c()
      #dumm<-unlist(CandidateList[[j]][k])
      d[k]<-sum(PopulationData[nrow(PopulationData),unlist(CandidateList[[j]][k])])
    }
    if((d[1]>averagePopulation*(1+BetaValue))||(d[1]<averagePopulation*(1-BetaValue))
       ||(d[2]>averagePopulation*(1+BetaValue))||(d[2]<averagePopulation*(1-BetaValue))){
      whichones<-append(whichones,j)
    }
  }
  if(length(whichones)>0){
    RR<-CandidateList[(-1*whichones)]
  }else(
    RR<-CandidateList
  )
  return(RR)
}

#this function's inputs are "list of lists for every districts' units are keeped in a wider list
funCalculateTotalVotes<-function(oneSolutionListofLists,electionResults){
  NumberParty<-nrow(electionResults)-1;
  TotalResults <- matrix(0, NumberParty, length(oneSolutionListofLists))
  for(p in 1:NumberParty){
    for(i in 1:length(oneSolutionListofLists)){
      for(j in 1:length(oneSolutionListofLists[[i]])){
        TotalResults[p,i]<-TotalResults[p,i]+electionResults[p,oneSolutionListofLists[[i]][j]]
      }
    }
  }
  return(TotalResults)
}

#Main reason to code this function is that not calculating the REST at every iteration
funCalculateTotalVotesAlternative<-function(Candidates,NumberParty,electionResults){
  cat("Number of Alternatives: ",length(Candidates))
  TotalResultsforCand <- array(0, c(NumberParty, length(Candidates),2))
  for(p in 1:NumberParty){
    for(i in 1:length(Candidates)){
      for(j in 1:2){
        for(k in 1:length(unlist(Candidates[[i]][j]))){
          dummyVector<-c();
          dummyVector<-unlist(Candidates[[i]][j])
          TotalResultsforCand[p,i,j]<-TotalResultsforCand[p,i,j]+electionResults[p,dummyVector[k]]
        }
      }
    }
  }
  
  return(TotalResultsforCand)
}

funNumberofRepresentativesAlternative<-function(results,party){
  numberofRepresents<-rep(0, length(results[1,,1]))
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        numberofRepresents[i]<-numberofRepresents[i]+1;
      }
    }
  }
  return(numberofRepresents)
}

funScore<-function(results,party){
  ff<-results
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
       ff[which.max(ff[,i,j]),i,j]=0
      }
    }
  scores<-rep(0, length(results[1,,1]))
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        scores[i]<-scores[i]+100000000000-abs(results[party,i,j]-max(ff[,i,j]))^(1/2);
      }else{
        scores[i]<-scores[i]-abs(max(results[,i,j])-results[party,i,j])^1/2;
      }
    }
  }
  return(scores)
}

#including the affect of the number of representative, compactness, difference
funScorewithParameters<-function(results,party,compactnessScores){
  alpha=1000000;
  beta=-1;
  teta=200;
  pow<-1/2;
  ff<-results
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      ff[which.max(ff[,i,j]),i,j]=0
    }
  }
  scoresforNumofRepre<-rep(0, length(results[1,,1]))
  scoresforDifference<-rep(0, length(results[1,,1]))
  scoresforCompactness<-rep(0, length(results[1,,1]))
  scores<-rep(0, length(results[1,,1]))
  
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        scoresforNumofRepre[i]<-scoresforNumofRepre[i]+1;
      }else{
        scoresforNumofRepre[i]<-scoresforNumofRepre[i];
      }
    }
  }
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        scoresforDifference[i]<-scoresforDifference[i]+abs(results[party,i,j]-max(ff[,i,j]))^(pow);
      }else{
        scoresforDifference[i]<-scoresforDifference[i]+abs(max(results[,i,j])-results[party,i,j])^pow;
      }
    }
  }
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      scoresforCompactness[i]<-scoresforCompactness[i]+compactnessScores[i,j]
    }
  }
  scores<-(alpha*scoresforNumofRepre)+(beta*scoresforDifference)+(teta*scoresforCompactness)
  return(scores)
}

# Before finding the pareto frontier, we need to achieve the values of diffrence, compactness and number of representative
funScoreforPareto<-function(results,party,compactnessScores){
  pow<-1/2;
  ff<-results
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      ff[which.max(ff[,i,j]),i,j]=0
    }
  }
  scoresforNumofRepre<-rep(0, length(results[1,,1]))
  scoresforDifference<-rep(0, length(results[1,,1]))
  scoresforCompactness<-rep(0, length(results[1,,1]))
  scores<-list();
  
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        scoresforNumofRepre[i]<-scoresforNumofRepre[i]+1;
      }else{
        scoresforNumofRepre[i]<-scoresforNumofRepre[i];
      }
    }
  }
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        scoresforDifference[i]<-scoresforDifference[i]+abs(results[party,i,j]-max(ff[,i,j]))^(pow);
      }else{
        scoresforDifference[i]<-scoresforDifference[i]+abs(max(results[,i,j])-results[party,i,j])^pow;
      }
    }
  }
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      scoresforCompactness[i]<-scoresforCompactness[i]+compactnessScores[i,j]
    }
  }
  scores<-append(scores,list(scoresforNumofRepre))
  scores<-append(scores,list(scoresforDifference))
  scores<-append(scores,list(scoresforCompactness))
  return(scores)
}

######################################PARETO FRONTIER#############################################
funPARETO<-function(SCORES){
  frontierSet<-c()
  sortedbyDifferenceIndexandExtractedMinNoofRep<-c()
  maxNumberofRepValue<-max(SCORES[[1]])
  isGondaDeleted<-which(SCORES[[1]]!=maxNumberofRepValue,TRUE) #determine the indexes which don't have max number of representaive value
  
  sortedbyDifferenceIndex<-order(SCORES[[2]])
  if(length(isGondaDeleted)==0){
    sortedbyDifferenceIndexandExtractedMinNoofRep<-sortedbyDifferenceIndex
  }else{
    sortedbyDifferenceIndexandExtractedMinNoofRep<-setdiff(sortedbyDifferenceIndex,isGondaDeleted);
  }

  frontierSet<-append(frontierSet,sortedbyDifferenceIndexandExtractedMinNoofRep[1])
  
  if(length(sortedbyDifferenceIndexandExtractedMinNoofRep)==1){
    return(frontierSet);
  }else{
    for(i in 2:length(sortedbyDifferenceIndexandExtractedMinNoofRep)){
      if(SCORES[[3]][frontierSet[length(frontierSet)]]<SCORES[[3]][sortedbyDifferenceIndexandExtractedMinNoofRep[i]]){
        frontierSet<-append(frontierSet,sortedbyDifferenceIndexandExtractedMinNoofRep[i])}
    }
  }
  cat("Number of Element in Pareto Fr: ",length(frontierSet))
  
  return(frontierSet);
}
#typeOfMetric is written for choosing difference based, equally weighted, or compactness based
#1----->difference   2------>equally weighted  3------>compactness based
funcFindBestCandidateforPareto<-function(SET,typeOfMetric){

  x<-0;
  numberofElements<-length(SET)
  if(typeOfMetric==1){
    x<-SET[1];
  }else if(typeOfMetric==2){
    x<-SET[ceiling(numberofElements/2)]
  }else{
    x<-SET[numberofElements]
  }
  
  functionResult<-list("which"=x,"objective"=0)
  return(functionResult)
}
############################################################3###########################################

#if typeOfCompact==1 PolsbyPopperFunction ----if typeOfCompact==2 AreaOverConvexHullFunction
funCompactnessScore<-function(possibleMoves,typeOfCompact,WholeSpatial){
  Coscores<-matrix(0,nrow = length(possibleMoves),ncol=2)
  if(typeOfCompact==1){
    for(i in 1:length(possibleMoves)){
      for(j in 1:2){
        mergedShape<-aggregate(rbind(WholeSpatial[possibleMoves[[i]][[j]],]))
        score<-PolsbyPopperFunction(mergedShape)
        Coscores[i,j]<-score;
      }
    }
  }else(
    for(i in 1:length(possibleMoves)){
      for(j in 1:2){
        mergedShape<-aggregate(rbind(WholeSpatial[possibleMoves[[i]][[j]],]))
        score<-AreaOverConvexHullFunction(mergedShape)
        Coscores[i,j]<-score;
      }
    }
  )
  return(Coscores)
}

elimineDueToCompactness<-function(Scores,threshold){
  willBeEliminated<-c()
  for(i in 1:nrow(Scores)){
    for(j in 1:ncol(Scores)){
      if(Scores[i,j]<threshold){
        willBeEliminated<-append(willBeEliminated,i)
      }
    }
  }
  willBeEliminated<unique(willBeEliminated)
}


funScore2<-function(results,party){
  ff<-results
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      ff[which.max(ff[,i,j]),i,j]=0
    }
  }
  scores<-rep(0, length(results[1,,1]))
  for(i in 1:length(results[1,,1])){
    for(j in 1:2){
      if(results[party,i,j]==max(results[,i,j])){
        scores[i]<-scores[i]+1;
      }
    }
  }
  return(scores)
}

#this funciton is for calculating the number of two districts which are used in generating neighbors
NumberofRepresentativesforUSED2<-function(district1,district2,NumberParty,electionResults,party){
  Resultsfor2 <- matrix(0, NumberParty,2)
  for(p in 1:NumberParty){
    for(h in 1:length(district1)){
      Resultsfor2[p,1]<-Resultsfor2[p,1]+electionResults[p,district1[h]]
    }
    for(m in 1:length(district2)){
      Resultsfor2[p,2]<-Resultsfor2[p,2]+electionResults[p,district2[m]]
    }
  }
  #find the number of representatives 
  numberRepresent=0;
  for(i in 1:2){
    if(Resultsfor2[party,i]==max(Resultsfor2[,i])){
      numberRepresent<-numberRepresent+1;
    }
  }
  return(numberRepresent)
}

###################################################################################################
#this function is for calculating the total votes for all parties in a solution--output version--
funCalculateTotalVotesforAWholeSolution<-function(Solution,electionResults){
  NumberParty<-nrow(electionResults)-1;
  SolutionVoteResults <- matrix(0, NumberParty, length(Solution))
  for(p in 1:NumberParty){
    for(i in 1:length(Solution)){
      for(j in 1:length(Solution[[i]])){
        SolutionVoteResults[p,i]<-SolutionVoteResults[p,i]+electionResults[p,Solution[[i]][j]]
      }
    }
  }
  return(SolutionVoteResults)
}

#for a whole solution
funNumberofRepresentatives<-function(results,party){
  numberofRepresent=0;
  for(i in 1:ncol(results)){
    if(results[party,i]==max(results[,i])){
      numberofRepresent<-numberofRepresent+1;
    }
  }
  return(numberofRepresent)
}
#######################################################################################
funcFindBestCandidate<-function(NumberofRepresententsofCandidates){
  for(i in 1:length(NumberofRepresententsofCandidates)){
    if(NumberofRepresententsofCandidates[i]==max(NumberofRepresententsofCandidates)){
      x<-i
    }
  }
  functionResult<-list("which"=x,"objective"=NumberofRepresententsofCandidates[x])
  return(functionResult)
}

funcFindBestCandidate2<-function(NumberofRepresententsofCandidates){
  possibleones<-c();
  for(i in 1:length(NumberofRepresententsofCandidates)){
    if(NumberofRepresententsofCandidates[i]==max(NumberofRepresententsofCandidates)){
      possibleones<-append(possibleones,i)
    }
  }
  randomNumber <- sample(1:length(possibleones), 1)
  functionResult<-list("which"=randomNumber,"objective"=NumberofRepresententsofCandidates[randomNumber])
  return(functionResult)
}


