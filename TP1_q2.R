# Question 2 : Etudiez la dépendance linéaire (modèle ARMA) des rentabilités journalières, hebdomadaires. Peut-on tirer profit de cette dépendance ? Comment ? Conséquences en termes d’efficience des marchés.


install.packages("forecast", lib="/user/9/.base/paviotch/home/R/x86_64-pc-linux-gnu-library/3.3")
library(forecast, lib.loc="/user/9/.base/paviotch/home/R/x86_64-pc-linux-gnu-library/3.3")



pdf("arma_journaliere.pdf")
acf(RentJ$Rt, lag=200) #donne p = 4, MA = acf
pacf(RentJ$Rt, lag = 200) #donne q = 2 AR = pacf
dev.off()
print(auto.arima(RentJ$Rt)) #meilleur modele : arma(2,2)

pdf("arma_hebdomadaire.pdf")
acf(RentH$Rt, lag=200) #donne p = 3
pacf(RentH$Rt, lag=200) #donne q = 3
dev.off()


#armaDowJones<-arma(RentJ$Rt, lag=list(ar=c(2,4), ma=c(2,4)))
#print(summary(armaDowJones))

#print(summary(arma(RentJ$Rt, order = c(0,1))))
#print(summary(arma(RentJ$Rt, order = c(1,0)))) # => meilleur modele ? les coeff sont le plus significativement non nuls + le + petit AIC
#si R2 tres proche de 0 alors la strategie est  gagnante => pas efficience des marchés ?