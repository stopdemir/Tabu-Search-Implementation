



r<-InitializationFunc2(funAdjacency(),funElectionResults(),0.75,31)
rr<-funSecondStepforInitialization(rr,31)
funVisualizationofPopulationNumbers(rrr,funElectionResults(),0.2,31)
funFeasibilityControl(rrr,funAdjacency(),funElectionResults(),0.2)
rrr<-funThirdStepforInitialization(rrr,funAdjacency(),funElectionResults(),0.2)
rrrr<-funForthStepforInitialization(rrr,funAdjacency(),funElectionResults(),0.2)

funVisualizationofPopulationNumbers(rrr,funElectionResults(),0.2,31)
#funVisualization(rrr)
funFeasibilityControl(rrr,funAdjacency(),funElectionResults(),0.2)

funVisualizationofPopulationNumbers(rrrr,funElectionResults(),0.2,31)

