#TP1

load("RentDJINDUS.RData")
source("fonctions.R")
#Question 1  Etudiez la distribution empirique des rentabilités journalières, hebdomadaires, mensuelles de votre titre. Interprétez les résultats.
plot(RentJ$Rt, type='l', col='blue')

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

#moyenne
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
#tableau p12


