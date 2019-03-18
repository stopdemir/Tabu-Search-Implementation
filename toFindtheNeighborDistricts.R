myFunction<-function(number,Matrix32){
  neighVec<-c()
  for(i in 1:nrow(Matrix32)){
    if(Matrix32[number,i]==1){
      neighVec<-append(neighVec,i)
    }
  }
  return(neighVec)
}