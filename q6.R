#Modélisez les rentabilités journalières et hebdomadaires à l’aide de modèle de type ARCH.
.libPaths("/user/1/taram/Public/RLib") 
library(fGarch)
library(tseries)
load("RentDJINDUS.RData")

#rentabilités journalières
archDowJones <- garchFit(formula = ~ arma(0,2) + garch(1,1), data=RentJ$Rt)
print(summary(archDowJones))

#archDowJones<-garch(RentJ$Rt, order = c(0, 11))
#print(summary(archDowJones))

pdf("densite_residus_journaliere_q6.pdf")
plot(density(archDowJones@residuals[!is.na(archDowJones@residuals)]))
moy <- mean(archDowJones@residuals[!is.na(archDowJones@residuals)])
variance <- var(archDowJones@residuals[!is.na(archDowJones@residuals)])
taille <- length(archDowJones@residuals[!is.na(archDowJones@residuals)])
lines(col='red',density(rnorm(taille, mean = moy, sd = variance)))
dev.off()

acf(archDowJones@residuals[!is.na(archDowJones@residuals)])
R2Arch<-1-(variance/var(RentJ$Rt))
cat("Var totale : ", var(RentJ$Rt), " \n")
cat("R² = " , R2Arch)

#rentabilités hebdomadaires
archDowJones <- garchFit(formula = ~ arma(0,1) + garch(2,1), data=RentH$Rt)
Box.test(archDowJones@residuals)
print(summary(archDowJones))


pdf("densite_residus_hebdo_q6.pdf")
plot(density(archDowJones@residuals[!is.na(archDowJones@residuals)]))
moy <- mean(archDowJones@residuals[!is.na(archDowJones@residuals)])
variance <- var(archDowJones@residuals[!is.na(archDowJones@residuals)])
taille <- length(archDowJones@residuals[!is.na(archDowJones@residuals)])
lines(col='red',density(rnorm(taille, mean = moy, sd = sqrt(variance))))
dev.off()
#qqnorm(archDowJones@residuals)

acf(archDowJones@residuals[!is.na(archDowJones@residuals)])
cat("TEST : ")
#print(Box.test(archDowJones@residuals, type="Ljung-Box"))

R2Arch<-1-(variance/var(RentH$Rt))
cat("Var totale : ", var(RentH$Rt), " \n")
cat("R² = " , R2Arch)
