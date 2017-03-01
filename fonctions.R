GetSubset <- function(date_debut, date_fin, vect){
  vector = c()
  for (i in 1:length(vect$Dates)){
    if(vect$Dates[i]<= date_fin && vect$Dates[i]>= date_debut){
      vector = c(vector,vect$Rt[i])
    }
    i= i+1;
  }
  return(vector)
} 

