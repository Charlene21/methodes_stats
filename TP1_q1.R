#TP1
.libPaths("/user/1/taram/Public/RLib") 
library(tseries)
#install.packages("moments", lib="/user/9/.base/paviotch/home/R/")
library(moments, lib.loc="/user/9/.base/paviotch/home/R/")
load("RentDJINDUS.RData")

#Question 1  Etudiez la distribution empirique des rentabilités journalières, hebdomadaires, mensuelles de votre titre. Interprétez les résultats.

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

pdf("rentabilité_journaliere.pdf")
plot(RentJ$Rt, xlab='dates', ylab='R(t)', main="Rentabilités journalières du Dow Jones", type='l', col='blue')
dev.off()

#Rentabilités journalières
densityRtJ = density(RentJ$Rt)

pdf("densite_journaliere.pdf")
plot(densityRtJ,type="n",main="Graphique de distribution des rentabilités journalières")
lines(densityRtJ)
taille <- length(RentJ$Rt)
moy <- mean(RentJ$Rt)
variance <- var(RentJ$Rt)
lines(col='red',density(rnorm(taille, mean = moy, sd = sqrt(variance))))
lines(hist(RentJ$Rt, xlab=" ", main="Histogramme pour les rentabilités journalières"))
dev.off()


#Rentabilités hebdomadaires
densityRtH = density(RentH$Rt)


pdf("densite_hebdo.pdf")
plot(densityRtH,type="n",main="Graphique de distribution des rentabilités hebdomadaires")
lines(densityRtH)
tailleH <- length(RentH$Rt)
moyH <- mean(RentH$Rt)
varianceH <- var(RentH$Rt)
lines(col='red',density(rnorm(tailleH, mean = moyH, sd = sqrt(varianceH))))
lines(hist(RentH$Rt,xlab=" ",main="Histogramme pour les rentabilités hebdomadaires"))
dev.off()


#Rentabilités mensuelles
densityRtM = density(RentM$Rt)

pdf("densite_mensuel.pdf")
plot(densityRtM,type="n",main="Graphique de distribution des rentabilités mensuelles")
lines(densityRtM)
tailleM <- length(RentM$Rt)
moyM <- mean(RentM$Rt)
varianceM <- var(RentM$Rt)
lines(col='red',density(rnorm(tailleM, mean = moyM, sd = sqrt(varianceM))))
lines(hist(RentM$Rt,xlab=" ",main="Histogramme pour les rentabilités mensuelles"))
dev.off()

subset1 = GetSubset("1973-01-02", "1980-01-02")
subset2 = GetSubset("1980-01-03", "1987-01-03")
subset3 = GetSubset("1987-01-04", "1994-01-04")
subset4 = GetSubset("1994-01-05", "2001-01-05")
subset5 = GetSubset("2001-01-06", "2008-01-06")
subset6 = GetSubset("2008-01-07", "2016-05-31")

print("moyenne des échantillons : ")
mean1 = mean(subset1)
mean2 = mean(subset2)
mean3 = mean(subset3)
mean4 = mean(subset4)
mean5 = mean(subset5)
mean6 = mean(subset6)

print(mean1)
print(mean2)
print(mean3)
print(mean4)
print(mean5)
print(mean6)

print("moyenne annualisée : \n")
print(mean1*250)
print(mean2*250)
print(mean3*250)
print(mean4*250)
print(mean5*250)
print(mean6*250)

#Variance
print("Variance : ")
variance1 = var(subset1)
variance2 = var(subset2)
variance3 = var(subset3)
variance4 = var(subset4)
variance5 = var(subset5)
variance6 = var(subset6)

print(variance1)
print(variance2)
print(variance3)
print(variance4)
print(variance5)
print(variance6)

print("écart type : ")
print(sqrt(variance1))
print(sqrt(variance2))
print(sqrt(variance3))
print(sqrt(variance4))
print(sqrt(variance5))
print(sqrt(variance6))

print("ecart type annualisé :")
print(sqrt(variance1)*sqrt(250))
print(sqrt(variance2)*sqrt(250))
print(sqrt(variance3)*sqrt(250))
print(sqrt(variance4)*sqrt(250))
print(sqrt(variance5)*sqrt(250))
print(sqrt(variance6)*sqrt(250))

print("moment d'ordre 3 : ")
print(skewness(subset1))
print(skewness(subset2))
print(skewness(subset3))
print(skewness(subset4))
print(skewness(subset5))
print(skewness(subset6))

print("p-value : ")
print(agostino.test(subset1))
print(agostino.test(subset2))
print(agostino.test(subset3))
print(agostino.test(subset4))
print(agostino.test(subset5))
print(agostino.test(subset6))


print("moment d'ordre 4 : ")
print(kurtosis(subset1))
print(kurtosis(subset2))
print(kurtosis(subset3))
print(kurtosis(subset4))
print(kurtosis(subset5))
print(kurtosis(subset6))

print("p-value : ")
print(anscombe.test(subset1))
print(anscombe.test(subset2))
print(anscombe.test(subset3))
print(anscombe.test(subset4))
print(anscombe.test(subset5))
print(anscombe.test(subset6))


#tableau p12


