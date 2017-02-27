# Fonction permettant de récuperer une partie des données
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

# Fonction de calcul du R²
# Paramètres
# ARMA(p, q)
# GARCH(a, b)
# rentabilites : contient les donnees
calculRcarre <- function(modele, rentabilites) {
  variance <- var(modele@residuals[!is.na(modele@residuals)])
  Rcarre <- 1-(variance/var(rentabilites$Rt))
  return(Rcarre)
}