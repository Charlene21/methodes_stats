load("RentDJINDUS.RData");
#RentX : dates + rentas(hebdo, jour, mens)

# cf p.16 du poly, section 1.4.9

.libPaths("/user/1/taram/Public/RLib")
load("RHmm")
library(RHmm)

HMM = HMMFit(RentJ$Rt)
vit = viterbi(HMM,RentJ$Rt)
#Graphic diagnostic of the HMM estimation
HMMGraphicDiag(vit,HMM,RentJ$Rt)

#Distribution parameters:
#                 mean          var
#State 1 -0.0008767625 3.844501e-04
#State 2  0.0005011179 6.138199e-05
model=summary(HMM)
m1 = model$coef[7,1]
m2 = model$coef[9,1]
#m1 != m2 => asymétrique

#Plot univariates series in each estimated states
HMMPlotSerie(RentJ$Rt,vit,dates=RentJ$Dates,color='black')