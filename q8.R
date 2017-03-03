library(fGarch)
library(tseries)
library(RRegArch)

Renta_set1 = GetSubset("1990-01-02", "2002-12-31", RentJ)
Renta_set2 = GetSubset("2003-01-01", "2016-12-31", RentJ)

model1 = garchFit(formula = ~aparch(2,1), data=Renta_set1,trace = FALSE)
model2 = garchFit(formula = ~aparch(2,1), data=Renta_set2,trace = FALSE)

ks.test(predict_1[,3],predict_1[,3])

tab_1 = model1@fit$matcoef[,1:2]
tab_1[,2] = tab_1 [,2]*1.96
colnames(tab_1)[2]="Half CI"

tab_2 = model2@fit$matcoef[,1:2]
tab_2[,2] = tab_1 [,2]*1.96
colnames(tab_2)[2]="Half CI"

#Bornes_i contient les intervalles de confiance pour les paramètres estimés
bornes_1 = tab_1
bornes_1[,1] = tab_1[,1] - tab_1[,2]
bornes_1[,2] = tab_1[,1] + tab_1[,2]

bornes_2 = tab_2
bornes_2[,1] = tab_2[,1] - tab_2[,2]
bornes_2[,2] = tab_2[,1] + tab_2[,2]

#On cherche les intersections des intervalles de confiance à 95%
intersection = logical(length = length(tab_1[,1]))
names(intersection) = names(tab_1[,1])
for (i in 1:length(intersection)) {
  if (bornes_1[i,2]-bornes_2[i,1] < 0) {
    intersection[i] = FALSE
  } else if (bornes_2[i,2]-bornes_1[i,1] < 0) {
    intersection[i] = FALSE
  } else {
    intersection[i] = TRUE
  }
}
print(intersection)

#Ecart relatif
ecart = seq(length(tab_1[,1]))
names(ecart) = names(tab_1[,1])
for (i in 1:length(ecart)) {
  ecart[i] = 100*(tab_1[i,1] - tab_2[i,1] )/ tab_1[i,1] 
}
print(ecart)