
##
## Recherche du 'meilleur modèle'
##
  AIC = 0
  i_best = 0
  j_best = 0
  for (i in 1:4) {
    for (j in 0:4) {
      #Resultat : i=3,j=3, AIC=-6.520538
      #formula = as.formula(paste("~garch(",i,",",j,")"))
      #Resultat : i=1,j=1, AIC=-6.522729
      formula = as.formula(paste("~aparch(",i,",",j,")"))
      model = garchFit(formula = formula, data=RentJ$Rt,trace = FALSE)
      
      if (model@fit$ics[[1]]<AIC) {
        AIC = model@fit$ics[[1]]
        i_best = i
        j_best = j
      }
    }
  }
  print(i_best)
  print(j_best)
  
  best_model = garchFit(formula = ~aparch(1,1), data=RentJ$Rt,trace = FALSE)

##
## Significativité des paramètres du modèle
##
  matcoefs = best_model@fit$matcoefbest_model

##
## Nature du bruit
##
  residuals = residuals(best_model)
  
  #
  # Indépendance p-value > 0.05 donc indépendants
    Box.test(residuals)
  
  #
  # Normalité
  
    #Pas de loi normale
    qqplot(residuals,rnorm(10000))
    
    #Loi de Student
    qqplot(residuals,rt(df=4,n=10000))

