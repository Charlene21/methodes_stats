# Etudiez la corrélation entre les valeurs absolues de deux rentabilités journalières consécutives, entre leurs carrés. Peut-on profiter de ces dépendances ? Comment ?
.libPaths("/user/1/taram/Public/RLib") 
library(tseries)

RentCarre <- (RentJ$Rt)^2

pdf("arma_journaliere_Carre.pdf")
pacf(RentCarre, plot = T, main='Autocorrelations partielles de la serie des rentabilites au carre',
xlab='Retards', ylab='Autocorrelations partielles')
dev.off()

ArmaCarre <- arma(RentCarre, order=c(5,0))
print(summary(ArmaCarre))
cat("variance totale : ", var(RentCarre), "\n")
R2DowJones<-1-var(ArmaCarre$residuals[!is.na(ArmaCarre$residuals)])/var(RentCarre) #R² = 0.05
cat("R² = ", R2DowJones, "\n")

RentAbs <- abs(RentJ$Rt)
pdf("arma_journaliere_abs.pdf")
pacf(RentAbs, plot = T, main='Autocorrelations partielles de la serie des valeurs absolues \n des rentabilites',
xlab='Retards', ylab='Autocorrelations partielles')
dev.off()

ArmaAbs <- arma(RentAbs, order=c(12,0))
print(summary(ArmaAbs))
cat("variance totale : ", var(RentAbs), "\n")
R2DowJones<-1-var(ArmaAbs$residuals[!is.na(ArmaAbs$residuals)])/var(RentAbs) #R² = 0.17, AIC = -76771.47
cat("R² = ", R2DowJones, "\n")

