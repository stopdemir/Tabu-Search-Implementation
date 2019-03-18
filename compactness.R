library(geosphere)
library(rgeos)
#Compactness

PolsbyPopperFunction<-function(spatialObject){
  perimeterValue<-perimeter(spatialObject); #in meters
  areaValue<-areaPolygon(spatialObject);    #in square meters
  #bboxES<-spatialObject@bbox
  #distanceVertical<-distm(c(bboxES[1,2],bboxES[2,1]),c(bboxES[1,2],bboxES[2,2]),fun = distHaversine)
  #distanceHorizontal<-distm(c(bboxES[1,1],bboxES[2,1]),c(bboxES[1,2],bboxES[2,1]),fun = distHaversine)
  compactnessValue<-4*pi*areaValue/(perimeterValue*perimeterValue)
  return(compactnessValue)
}


AreaOverConvexHullFunction<-function(spatialObject){
  areaValue<-areaPolygon(spatialObject);    #in square meters
  ch<-gConvexHull(spatialObject)
  compactnessValue<-areaValue/area(ch)
  return(compactnessValue)
}

#if typeOfCompact==1 PolsbyPopperFunction ----if typeOfCompact==2 AreaOverConvexHullFunction
TotalCompactnessofaResult<-function(ASolution,typeCom){
  WholeSpatial<-readOGR(dsn="data5",layer="ASIAASIA")
  totalCompactScore=0;
  if(typeCom==1){
    for(i in 1:length(ASolution)){
      for(j in 1:2){
        mergedShape<-aggregate(rbind(WholeSpatial[ASolution[[i]],]))
        totalCompactScore<-PolsbyPopperFunction(mergedShape)+totalCompactScore;
      }
    }
  }else{
    for(i in 1:length(ASolution)){
      for(j in 1:2){
        mergedShape<-aggregate(rbind(WholeSpatial[ASolution[[i]],]))
        totalCompactScore<-AreaOverConvexHullFunction(mergedShape)+totalCompactScore;
      }
    }
  }
  return(totalCompactScore)
}






ch = gConvexHull(Europe)
plot(Europe,col='blue',border="blue");
plot(ch,add=TRUE)