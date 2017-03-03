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
p21 = model$coef["transMat[2,1]",1]
p11 =  model$coef["transMat[1,1]",1]

p = p21 / (1-p11+p21)
table(vit$states)[1] / (table(vit$states)[1]+table(vit$states)[2])

#m1 != m2 => asymétrique

#Plot univariates series in each estimated states
HMMPlotSerie(RentJ$Rt,vit,dates=RentJ$Dates,color='black')

#### 3 états
HMM = HMMFit(RentJ$Rt,nStates = 3)
model=summary(HMM)
m1 = model$coef[13,1]*250*100
m2 = model$coef[15,1]*250*100
m3 = model$coef[17,1]*250*100

v1 = sqrt(model$coef[14,1]*250)*100
v2 = sqrt(model$coef[16,1]*250)*100
v3 = sqrt(model$coef[18,1]*250)*100

HMMPlotSerie(RentJ$Rt,vit,dates=RentJ$Dates,color='black',oneFig = TRUE)
