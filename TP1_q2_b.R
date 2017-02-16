
.libPaths("/user/1/taram/Public/RLib") 
library(tseries)

load("RentDJINDUS.RData")

armaDowJones<-arma(RentJ$Rt, order=c(0,4)) #AIC = -67812.91
print(summary(armaDowJones))

armaDowJones<-arma(RentJ$Rt, order=c(4,0)) #AIC = -67812.99
print(summary(armaDowJones))

armaDowJones<-arma(RentJ$Rt, order=c(3,1)) #AIC = -67809.94
print(summary(armaDowJones))

armaDowJones<-arma(RentJ$Rt, order=c(1,3)) #AIC = -67810.3
print(summary(armaDowJones))

armaDowJones<-arma(RentJ$Rt, order=c(2,2)) #AIC = -67814.24
print(summary(armaDowJones))

#meilleur modele : arma(2,2)

cat("variance totale : ", var(RentJ$Rt), "\n")
R2DowJones<-1-var(armaDowJones$residuals[!is.na(armaDowJones$residuals)])/var(RentJ$Rt)
cat("R² = ", R2DowJones, "\n") #R² = 0.002209015, AIC = -67814.24