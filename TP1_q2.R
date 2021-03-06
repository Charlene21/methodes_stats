# Question 2 : Etudiez la dépendance linéaire (modèle ARMA) des rentabilités journalières, hebdomadaires. Peut-on tirer profit de cette dépendance ? Comment ? Conséquences en termes d’efficience des marchés.

#library(forecast)

.libPaths("/user/1/taram/Public/RLib") 
library(tseries)

#Rentabilites journalieres

pdf("arma_journaliere_acf.pdf")
acf(RentJ$Rt, lag=200, main="Rentabilités journalières") #donne q = 2, MA = acf
dev.off()

pdf("arma_journaliere_pacf.pdf")
pacf(RentJ$Rt, lag = 200, main="Rentabilités journalières") #donne p = 2 AR = pacf
dev.off()

#print(auto.arima(RentJ$Rt, max.p=4, max.q=2,max.order=4, stepwise = FALSE)) #meilleur modele : arma(0,2)

cat("AIC pour les rentabilites journalieres : \n")
armaDowJones<-arma(RentJ$Rt, order = c(0,1));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentJ$Rt, order = c(1,0));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentJ$Rt, order = c(1,1));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentJ$Rt, order = c(0,2));
cat("AIC = ", summary(armaDowJones)$aic, "\n");
R2DowJones<-1-var(armaDowJones$residuals[!is.na(armaDowJones$residuals)])/var(RentJ$Rt)
cat("R² = ", R2DowJones, "\n") 

armaDowJones<-arma(RentJ$Rt, order = c(2,0));
cat("AIC = ", summary(armaDowJones)$aic, "\n");


#Rentabilites hebdomadaires
pdf("arma_hebdomadaire_acf.pdf")
acf(RentH$Rt, lag=200, main="Rentabilités hebdomadaires") #donne p = 2
dev.off()

pdf("arma_hebdomadaire_pacf.pdf")
pacf(RentH$Rt, lag=200, main="Rentabilités hebdomadaires") #donne q = 3
dev.off()

cat("AIC pour les rentabilites hebdomadaires : \n")
armaDowJones<-arma(RentH$Rt, order = c(0,1));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(0,2));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(0,3));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(1,0));
cat("AIC = ", summary(armaDowJones)$aic, "\n");
R2DowJones<-1-var(armaDowJones$residuals[!is.na(armaDowJones$residuals)])/var(RentH$Rt)
cat("R² = ", R2DowJones, "\n") 

armaDowJones<-arma(RentH$Rt, order = c(1,1));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(1,2));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(2,0));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(2,1));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

armaDowJones<-arma(RentH$Rt, order = c(3,0));
cat("AIC = ", summary(armaDowJones)$aic, "\n");

#print(auto.arima(RentH$Rt, max.p=2, max.q=3,max.order=3, stepwise = FALSE))
