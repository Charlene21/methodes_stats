#espérance des rentabilités journalières sur [1990-2002] et [2003-2016]
.libPaths("/user/1/taram/Public/RLib") 
library(stringr)
# mean1 = 0;
# mean2 = 0;
# i = 0;
# j = 0;
# vect1 = c()
# vect2 = c()
# for (i in 1:length(RentJ$Dates)){
# 	if(str_sub(RentJ$Dates[i], 1, 4) <= 2002 && str_sub(RentJ$Dates[i], 1, 4) >= 1990){
# vect1 = c(vect1,RentJ$Rt[i])
# 	}
# 	else if (str_sub(RentJ$Dates[i], 1, 4) >= 2003){
# 	  vect2 = c(vect2,RentJ$Rt[i])
# 		}
# }
# cat("mean 1 = ", mean(vect1), "\n")
# cat("mean 2 = ", mean(vect2), "\n")
# cat("var 1 = ", var(vect1), "\n")
# cat("var 2 = ", var(vect2), "\n")
# 

#### Correction avec la fonction GetSubset
source("fonctions.R")
vect1 = GetSubset("1990-01-01", "2002-12-31");
vect2 = GetSubset("2003-01-01", "2016-05-31");
mean1 = mean(vect1);
mean2 = mean(vect2);
cat("mean 1 = ", mean1, "\n");
cat("mean 2 = ", mean2, "\n");
var1 = var(vect1);
var2 = var(vect2);
cat("var1 = ", var1, "\n");
cat("var2 = ", var2, "\n");

