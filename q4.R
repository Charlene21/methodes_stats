#Espérance des rentabilités journalières sur [1990-2002] et [2003-2016]

source("fonctions.R")

vect1 = GetSubset("1990-01-01", "2002-12-31",RentJ);
vect2 = GetSubset("2003-01-01", "2016-05-31",RentJ);
mean1 = mean(vect1);
mean2 = mean(vect2);
cat("mean 1 = ", mean1, "\n");
cat("mean 2 = ", mean2, "\n");
cat("mean annualisée 1 = ", mean1*250, "\n");
cat("mean annualisée 2 = ", mean2*250, "\n");
var1 = var(vect1);
var2 = var(vect2);
cat("var1 = ", var1, "\n");
cat("var2 = ", var2, "\n");
cat("var annualisée 1 = ", var1*sqrt(250), "\n");
cat("var annualisée 2 = ", var2*sqrt(250), "\n");

#tests de Student et Fisher
t.test(vect1,vect2)

var.test(vect1,vect2)
