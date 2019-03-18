library(rgdal)
TABUSEARCH<-function(MaxIteration,TabuListLimit,numberofUnits,NumberofDistricts,NumberofParties,whichParty,Beta){
  #MaxIteration<-1000;
  myColors<-c("seagreen1","violetred1","turquoise1","mediumorchid1","greenyellow","brown1")
  t<-0;
  Asia<-readOGR(dsn="data5",layer="ASIAASIA")
  WholeSpatial<-Asia
  TabuList<-list();
  NonImprovingMoves<-0;
  maxNIM<-numberofUnits*3;
  Improved<-FALSE;
  ElectionResults<-funElectionResults();
  adjMatrix<-funAdjacency();
  Solution<-NO12TO16_3
  BestSolution<-Solution;
  bestObjective<-0;
  bestNewSolObjective<-0;
  #-----------------------------visualization--------------------------------
  #turkey<-readOGR(dsn="data2",layer="TUR_adm1")
  #turkey$ID_1<-as.numeric(as.factor(turkey$ID_1))
  #plot(turkey,col="lightgrey")
  #---------------------------------------------------------------------------
  while(t<MaxIteration){
    
    
    PossibleMoves<-list()
    NumofRepreforCandidates<-c();
    DUMMY3<-GENERATE_NEIGHBORS(Solution,Beta,adjMatrix,ElectionResults);
    PossibleMoves<-DUMMY3$FINALRESULT
    TheUsedDistrictsNumber<-DUMMY3$USED  #a vector containing 2 integers 
    #this for loop is to eliminate the elements which are in the TABU
    #for(i in length(PossibleMoves)){
    #if(!funcTabuListCheck(TabuList,PossibleMoves[[i]])){
    #   CandidateList<-append(CandidateList,list(PossibleMoves[[i]]))
    #}
    #}
    ############################
    #isGonnaEliminated<-c()
    #ComScores<-funCompactnessScore(PossibleMoves,2,WholeSpatial)
    #isGonnaEliminated<-elimineDueToCompactness(ComScores,0.66)
    #if(length(isGonnaEliminated)>0){
    #PossibleMoves<-PossibleMoves[-isGonnaEliminated]}
    
    a<-list()
    a<-append(a,list(Solution[[DUMMY3$USED[1]]]))
    a<-append(a,list(Solution[[DUMMY3$USED[2]]]))
    PossibleMoves<-append(PossibleMoves,list(a))
    ComScores<-funCompactnessScore(PossibleMoves,2,WholeSpatial)
    CandidateResults<-funCalculateTotalVotesAlternative(PossibleMoves,NumberofParties,ElectionResults)
    
    #NumofRepreforCandidates<-funScore(CandidateResults,whichParty,ComScores)
    
    if(1==2){
    ##########by using alfa beta parameters###############
    NumofRepreforCandidates<-funScorewithParameters(CandidateResults,whichParty,ComScores)
    #CurrentObjective<-NumberofRepresentativesforUSED2(Solution[[TheUsedDistrictsNumber[1]]],
    #Solution[[TheUsedDistrictsNumber[2]]],
    #NumberofParties,ElectionResults,whichParty)
    BESTNEWSOLUTION<-funcFindBestCandidate(NumofRepreforCandidates);}
    ###############################################
    
    #using pareto frontier###############
    if(1==1){
      ScoresForPareto<-funScoreforPareto(CandidateResults,whichParty,ComScores)
      ParetoFrontierSet<-funPARETO(ScoresForPareto)
      BESTNEWSOLUTION<-funcFindBestCandidateforPareto(ParetoFrontierSet,3) #1---> small difference, 2-->equallyweighted, 3--->compactbased
    }
    #Update the current solution
    #you use "possibleMoves", it should be CandidateList
    Solution[[TheUsedDistrictsNumber[1]]]<-unlist(PossibleMoves[[BESTNEWSOLUTION$which]][1])
    Solution[[TheUsedDistrictsNumber[2]]]<-unlist(PossibleMoves[[BESTNEWSOLUTION$which]][2])
    
    #COMPARE THE BEST OBJECTIVE AND BESTNEWSOLUTION OBJECTIVE
    bestObjective<-funNumberofRepresentatives(funCalculateTotalVotesforAWholeSolution(BestSolution,ElectionResults),whichParty)
    bestNewSolObjective<-funNumberofRepresentatives(funCalculateTotalVotesforAWholeSolution(Solution,ElectionResults),whichParty)
    cat("  bestnewSol: ",bestNewSolObjective,"  ") 
    print(" ")
    cat("best: ",bestObjective, "  ")
    
    if(bestNewSolObjective>bestObjective){
      BestSolution<-Solution;
      NonImprovingMoves<-0;
      Improved=TRUE;
    }else{
      NonImprovingMoves<-NonImprovingMoves+1;
      Improved=FALSE;
    }
    if(FALSE==TRUE){
#if(Improved==TRUE){
    for(i in 1:length(Solution)){
      for(j in 1:length(Solution[[i]])){
        a<-turkey$ID_1==Solution[[i]][j]
        plot(turkey[a,],col=myColors[i],add =TRUE) 
      }
    }
#}
    }
    #ADD THE SOLUTION TO TABU AND EXTRACT THE OLDEST ONE FROM TABU(if the limit is exceeded)
    dummyadded<-list()
    dummyadded<-list(unlist(PossibleMoves[[BESTNEWSOLUTION$which]][1]),unlist(PossibleMoves[[BESTNEWSOLUTION$which]][2]))
    TabuList<-append(TabuList,list(dummyadded))
    if(length(TabuList)>TabuListLimit){
      TabuList[[1]]=NULL
    }
    t<-t+1
    
  }
  return(BestSolution)
}















