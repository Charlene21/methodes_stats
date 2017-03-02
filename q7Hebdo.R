library(tseries)
library(moments)
load("RentDJINDUS.RData")
source("fonctions.R")

Renta = GetSubset("1990-01-02", "2014-12-31", RentH)

# AIC = 0
# i_best = 0
# j_best = 0
# for (i in 1:4) {
#   for (j in 0:4) {
#     #Resultat : i=3,j=3, AIC=-5.105606
#     # formula = as.formula(paste("~garch(",i,",",j,")"))
#     #Resultat : i=4,j=0, AIC=-5.101092
#     formula = as.formula(paste("~aparch(",i,",",j,")"))
#     model = garchFit(formula = formula, data=Renta,trace = FALSE)
# 
#     if (model@fit$ics[[1]]<AIC) {
#       AIC = model@fit$ics[[1]]
#       i_best = i
#       j_best = j
#     }
#   }
# }
# cat("i_best : ", i_best, "\n")
# cat("j_best : ", j_best, "\n")
# cat("AIC : ", AIC)

best_model = garchFit(formula = ~garch(3,3), data=Renta,trace = FALSE)

##
## Significativité des paramètres du modèle
##
matcoefs = best_model@fit$matcoef

Rents = GetSubset("1990-01-02","2016-05-31", RentH)
Rentabilites = GetSubset("2015-01-01", "2016-05-31", RentH)
predictValues = predict(best_model,n.ahead = length(Rentabilites), plot=TRUE, nx= 10)
Rents = c(0,0,0,0,0,0,0,0,0,0, Rents)
lines(Rents, col='blue')

vect = c(0,0,0,0,0,0,0,0,0,0)
compteur = 0;
for (i in 1:length(Rentabilites)){
  plus = predictValues$meanForecast[i] +1.96*predictValues$standardDeviation[i]
  moins = predictValues$meanForecast[i] -1.96*predictValues$standardDeviation[i]
  vect= c(vect,predictValues$meanForecast[i] +1.96*predictValues$standardDeviation[i])
  if(Rentabilites[i] < plus && Rentabilites[i] > moins){
    compteur = compteur +1;
  }
}

cat("ratio = ", compteur/length(Rentabilites))
