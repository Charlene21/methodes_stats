.libPaths("/user/1/taram/Public/RLib") 
library(fGarch)

library(tseries)
source("fonctions.R")


Renta = GetSubset("1990-01-02", "2014-12-31", RentJ)

AIC = 0
i_best = 0
j_best = 0
for (i in 1:4) {
  for (j in 0:4) {
    #Resultat : i=2,j=2, AIC=-6.576749
    formula = as.formula(paste("~garch(",i,",",j,")"))
    #Resultat : i=4,j=0, AIC=-6.510748
    #formula = as.formula(paste("~aparch(",i,",",j,")"))
    model = garchFit(formula = formula, data=Renta,trace = FALSE)

    if (model@fit$ics[[1]]<AIC) {
      AIC = model@fit$ics[[1]]
      i_best = i
      j_best = j
    }
  }
}
print(i_best)
print(j_best)

best_model = garchFit(formula = ~garch(2,2), data=Renta,trace = FALSE)

##
## Significativité des paramètres du modèle
##
matcoefs = best_model@fit$matcoef

pdf("q7jour.pdf")
Rents = GetSubset("1990-01-02","2016-05-31", RentJ)
Rentabilites = GetSubset("2015-01-01", "2016-05-31", RentJ)

NX = 10
predict(best_model,n.ahead = length(Rentabilites), plot=TRUE , nx= NX)
Rentabilites = c(rep(0, NX), Rentabilites)
lines(Rentabilites, col='blue')
dev.off()

#### Verification intervalle de confiance 

# vect = c(0,0,0,0,0,0,0,0,0,0)
# compteur = 0;
# for (i in 1:length(Rentabilites)){
#   plus = predictValues$meanForecast[i] +1.96*predictValues$standardDeviation[i]
#   moins = predictValues$meanForecast[i] -1.96*predictValues$standardDeviation[i]
#   vect= c(vect,predictValues$meanForecast[i] +1.96*predictValues$standardDeviation[i])
#   if(Rentabilites[i] < plus && Rentabilites[i] > moins){
#     compteur = compteur +1;
#   }
# }
# 
# cat("ratio = ", compteur/length(Rentabilites))
