#espérance des rentabilités journalières sur [1990-2002] et [2003-2016]
.libPaths("/user/1/taram/Public/RLib") 
library(stringr)
# mean1 = 0;
# mean2 = 0;
# i = 0;
# j = 0;
# for (i in 1:length(RentJ$Dates)){
# 	if(str_sub(RentJ$Dates[i], 1, 4) <= 2002){
# 		mean1 = mean1 + RentJ$Rt[i];
# 		i= i+1;
# 	}
# 	else {
# 		mean2 = mean2 + RentJ$Rt[i];
# 		j=j+1;
# 		}
# }
# cat("mean 1 = ", mean1/i, "\n")
# cat("mean 2 = ", mean2/j, "\n")

#Variance des rentabilités journalières sur [1990-2002] et [2003-2016]
# var1 = 0;
# n1=0;
# var2 = 0;
# n2=0;
# for (i in 1:length(RentJ$Dates)){
# 	if(str_sub(RentJ$Dates[i], 1, 4) <= 2002){
# 		n1 = n1 + 1;
# 		var1 = var1 + (RentJ$Rt[i] - mean1)^2;
# 	}
# 	else {
# 		n2 = n2 + 1
# 		var2 = var2 + (RentJ$Rt[i] - mean2)^2;
# 		}
# 				}
# 
# var1 = var1/n1;
# var2 = var2/n2;
# cat("var1 = ",var1, "\n");
# cat("var2 = ",var2, "\n");

#### Correction avec la fonction GetSubset
source("fonctions.R")
vect1 = GetSubset("1990-01-01", "2002-12-31");
vect2 = GetSubset("2003-01-01", "2016-12-31");
mean1 = mean(vect1);
mean2 = mean(vect2);
cat("mean 1 = ", mean1, "\n");
cat("mean 2 = ", mean2, "\n");
var1 = var(vect1);
var2 = var(vect2);
cat("var1 = ", var1, "\n");
cat("var2 = ", var2, "\n");

