GetSubset <- function(date_debut, date_fin){
  vect = c()
  for (i in 1:length(RentJ$Dates)){
    if(RentJ$Dates[i]<= date_fin && RentJ$Dates[i]>= date_debut){
      vect = c(vect,RentJ$Rt[i])
    }
    i= i+1;
  }
  return(vect)
} 