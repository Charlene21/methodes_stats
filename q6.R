#Modélisez les rentabilités journalières et hebdomadaires à l’aide de modèle de type ARCH.
.libPaths("/user/1/taram/Public/RLib") 
library(fGarch)
library(tseries)
library(RRegArch)
load("RentDJINDUS.RData")

#rentabilités journalières
archDowJones <- garchFit(formula = ~ garch(2,1), data=RentJ$Rt,trace = FALSE)
print(summary(archDowJones))

#pdf("densite_residus_journaliere_q6.pdf")
plot(density(archDowJones@residuals[!is.na(archDowJones@residuals)]))

sample1 = archDowJones@residuals[!is.na(archDowJones@residuals)]
sample1 = sample1[1:5000]
cat("Shapiro 1 : ")
print(shapiro.test(sample1)[2]$p.value)

moy <- mean(archDowJones@residuals[!is.na(archDowJones@residuals)])
variance <- var(archDowJones@residuals[!is.na(archDowJones@residuals)])
taille <- length(archDowJones@residuals[!is.na(archDowJones@residuals)])
lines(col='red',density(rnorm(taille, mean = moy, sd = sqrt(variance))))
#dev.off()

#acf(archDowJones@residuals[!is.na(archDowJones@residuals)])
R2Arch<-1-(variance/var(RentJ$Rt))
cat("Var totale : ", var(RentJ$Rt), " \n")
cat("R² = " , R2Arch)


archDowJones <- garchFit(formula = ~ aparch(2,1), data=RentJ$Rt,trace = FALSE)
print(summary(archDowJones))
sample1 = archDowJones@residuals[!is.na(archDowJones@residuals)]
sample1 = sample1[5000:9999]
cat("shapiro 2 : \n")
print(shapiro.test(sample1)[2]$p.value)

Renta = GetSubset("1990-01-02", "2014-12-31", RentJ)
cat("length : ", length(Renta), "\n")
archDowJones1 <- garchFit(formula = ~ garch(2,1), data=Renta,trace = FALSE)
print(summary(archDowJones1))
sample1 = archDowJones1@residuals[!is.na(archDowJones1@residuals)]
sample1 = sample1[1:5000]
cat("shapiro Renta : \n")
print(shapiro.test(sample1)[2]$p.value)


Renta1 = GetSubset("1990-01-02", "2002-12-31", RentJ)
cat("length : ", length(Renta1), "\n")
archDowJones3 <- garchFit(formula = ~ garch(2,1), data=Renta1,trace = FALSE)
print(summary(archDowJones3))
sample3 = archDowJones3@residuals[!is.na(archDowJones3@residuals)]
cat("shapiro Renta1 : \n")
print(shapiro.test(sample3)[2]$p.value)


Renta2 = GetSubset("2003-01-02", "2016-05-31", RentJ)
cat("length : ", length(Renta2), "\n")
archDowJones2 <- garchFit(formula = ~ garch(2,1), data=Renta2,trace = FALSE)
print(summary(archDowJones2))
sample2 = archDowJones2@residuals[!is.na(archDowJones2@residuals)]
sample2 = sample2[1:5000]
cat("shapiro Renta2 : \n")
print(shapiro.test(sample2)[2]$p.value)









#rentabilités hebdomadaires
 archDowJones <- garchFit(formula = ~ garch(2,1), data=RentH$Rt)
 
# Box.test(archDowJones@residuals)
 print(summary(archDowJones))
 
 sample1 = archDowJones@residuals[!is.na(archDowJones@residuals)]
 sample1 = sample1[1:5000]
 cat("Shapiro HEBDO GARCH : ")
 print(shapiro.test(sample1)[2]$p.value)
 
 archDowJones <- garchFit(formula = ~ aparch(1,1), data=RentH$Rt)
 
 # Box.test(archDowJones@residuals)
 print(summary(archDowJones))
 
 sample1 = archDowJones@residuals[!is.na(archDowJones@residuals)]
 sample1 = sample1[1:5000]
 cat("Shapiro HEBDO APARCH : ")
 print(shapiro.test(sample1)[2]$p.value)


 
# pdf("densite_residus_hebdo_q6.pdf")
# plot(density(archDowJones@residuals[!is.na(archDowJones@residuals)]))
# moy <- mean(archDowJones@residuals[!is.na(archDowJones@residuals)])
# variance <- var(archDowJones@residuals[!is.na(archDowJones@residuals)])
# taille <- length(archDowJones@residuals[!is.na(archDowJones@residuals)])
# lines(col='red',density(rnorm(taille, mean = moy, sd = sqrt(variance))))
# dev.off()
# #qqnorm(archDowJones@residuals)
# 
# acf(archDowJones@residuals[!is.na(archDowJones@residuals)])
# cat("TEST : ")
# #print(Box.test(archDowJones@residuals, type="Ljung-Box"))
# 
# R2Arch<-1-(variance/var(RentH$Rt))
# cat("Var totale : ", var(RentH$Rt), " \n")
# cat("R² = " , R2Arch)



 
 
 
 
 
 
 
 
 
 
llh = -100000;
shapiro = 0;
I =0;
J=0;
K=0;
L=0;

# for (i in 1:5) {
#   for(j in 0:5){
#     print(as.formula(paste("~garch(",i,",",j,")")))
#     archDowJones <- garchFit(formula = as.formula(paste("~garch(",i,",",j,")")), data=RentJ$Rt,trace = FALSE)
#     sample1 = archDowJones@residuals[!is.na(archDowJones@residuals)]
#     sample1 = sample1[1:5000]
#     # cat("Shapiro : \n")
#     # print(shapiro.test(sample1)[2]$p.value)
#     # cat("\n")
#     if (shapiro.test(sample1)[2]$p.value > shapiro){
#       shapiro = shapiro.test(sample1)[2]$p.value;
#       I = i;
#       J= j;
#     }
#       
#       
#     if(archDowJones@fit$llh > llh){
#       llh = archDowJones@fit$llh;
#       K= i;
#       L = j;
#         
#     }
#    
#   }
# }

# cat("shapiro : ", shapiro, " pour ");
# print(as.formula(paste("~garch(",I,",",J,")")))
# cat("\n")
# cat("LLH : ", llh, " pour ")
# print(as.formula(paste("~garch(",K,",",L,")")), "\n");