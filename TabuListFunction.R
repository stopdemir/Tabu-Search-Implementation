funcTabuListCheck<-function(tabuList, possibleSolution){
  control<-FALSE;
  print("Gird")
  for(i in 1:length(tabuList)){
    if ((length(unlist(tabuList[[i]][1]))==length(possibleSolution[[1]])  
         &&(length(unlist(tabuList[[i]][2]))==length(possibleSolution[[2]])))
        ||((length(unlist(tabuList[[i]][1]))==length(possibleSolution[[2]]))
           &&(length(unlist(tabuList[[i]][2]))==length(possibleSolution[[1]])))){
      print("Girdim ")
      if (((all(sort(unlist(tabuList[[i]][1]))==sort(possibleSolution[[1]])))  
           &&  (all(sort(unlist(tabuList[[i]][2]))==sort(possibleSolution[[2]]))))
          ||((all(sort(unlist(tabuList[[i]][1]))==sort(possibleSolution[[2]])))
             &&(all(sort(unlist(tabuList[[i]][2]))==sort(possibleSolution[[1]]))))){
        control<-TRUE;
        break;
      }
    }
  }
  return(control);
}

