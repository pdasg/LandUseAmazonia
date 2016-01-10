setwd("E:/SEMESTER_4/REDDPACdata/autocor")

source("E:/SEMESTER_4/Thesis/Rwork/loadingPKGS.R")
source("read.geoda.R")
source("moran.plotData.R")
source("plotCor.R")
require(spdep)
require(lctools)

#function to read geoda files
data1 <- read.geoda("E:/SEMESTER_4/REDDPACdata/autocor/data1.csv")
summary(data1)
attach(data1)
#plot(data1)
####################################################
################creating spatial wts################
####################################################
#loading the weights file in R
datagal <- read.gal("dataqueen.gal", override.id=TRUE)#the neighbor list
#the freq distribution is the same as observed in geoda also. class = nb (neighborlist)
summary.nb(datagal) 
class(datagal)
print(datagal)
print(is.symmetric.nb(datagal)) #check symmetry of neighborlist
str(data1)
#nb to spatial weights file(listw) for autocorrelation
wtsqueen <- nb2listw(datagal, zero.policy=FALSE)
summary(wtsqueen)
class(wtsqueen)
wtsqueen$weights

####################################################
################Moran's I, autocorrelation##########
####################################################
#for single variables
help(moran.test)
morbov<-moran.test(bov10.00, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morbov)
morcrop<-moran.test(changeCrop, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morcrop)
morPA<-moran.test(fra_PAs, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morPA)
mordef<-moran.test(D10.00, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(mordef)

#moran's I for logs(single variables)
morLbov<-moran.test(lBov10.00, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morLbov)
morLcrop<-moran.test(lchangecrp, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morLcrop)
morLPA<-moran.test(logfra_PA, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morLPA)
morLdef<-moran.test(lD10.00, listw=wtsqueen, randomisation=TRUE, alternative="two.sided")
print(morLdef)
#ls() function for seeing workspace elements

#permutation based test (moran for single variables)
#set.seed(123456)
morpmbov<-moran.mc(bov10.00, listw=wtsqueen, 999)
morpmbov
morpmchcrop<-moran.mc(changeCrop, listw=wtsqueen, 999)
morpmchcrop
morpmPA<-moran.mc(fra_PAs, listw=wtsqueen, 999)
morpmPA
morpmdef<-moran.mc(D10.00, listw=wtsqueen, 999)
morpmdef

#permutation based tests of log single variables
morpmLbov<-moran.mc(lBov10.00, listw=wtsqueen, 999)
morpmLbov
morpmLchcrop<-moran.mc(lchangecrp, listw=wtsqueen, 999)
morpmLchcrop
morpmLPA<-moran.mc(logfra_PA, listw=wtsqueen, 999)
morpmLPA
morpmLdef<-moran.mc(lD10.00, listw=wtsqueen, 999)
morpmLdef

#check statistics
morpmbov$statistic

#plotting reference distribution for permutation based moran of log variables 
#because they showed better moran I values 
help(moran.mc)
help(density)

#bovines

#create a density object
morfilter<- morpmLbov$res[1:length(morpmLbov$res)-1]
bovDen <- density(morfilter)#density object
#plotting graphs
plot.density(bovDen, main="Moran's I permutation test for Bovines", xlab="Reference Distribution", xlim=c(-0.3,0.6), ylim=c(0,30), lwd=2,col=2)
summary(morpmLbov$res)
hist(morfilter, freq=F, add=T)
abline(v=morpmLbov$statistic, lwd=2, col=4)

#change crop

morfilter1<- morpmLchcrop$res[1:length(morpmLchcrop$res)-1]
chcrpDen <- density(morfilter1)#density object
#plotting graphs
plot.density(chcrpDen, main="Moran's I permutation test for Crop", xlab="Reference Distribution", xlim=c(-0.3,0.7), ylim=c(0,30), lwd=2,col=2)
summary(morpmLchcrop$res)
hist(morfilter1, freq=F, add=T)
abline(v=morpmLchcrop$statistic, lwd=2, col=4)

#PAs

#create a density object
morfilter2<- morpmLPA$res[1:length(morpmLPA$res)-1]
paDen <- density(morfilter2)#density object
#plotting graphs
plot.density(paDen, main="Moran's I permutation test for PAs", xlab="Reference Distribution", xlim=c(-0.3,0.7), ylim=c(0,30), lwd=2,col=2)
summary(morpmLPA$res)
hist(morfilter2, freq=F, add=T)
abline(v=morpmLPA$statistic, lwd=2, col=4)

#defor

#create a density object
morfilter3<- morpmLdef$res[1:length(morpmLdef$res)-1]
defDen <- density(morfilter3)#density object
#plotting graphs
plot.density(defDen, main="Moran's I permutation test for Deforestation", xlab="Reference Distribution", xlim=c(-0.3,0.7), ylim=c(0,30), lwd=2,col=2)
summary(morpmLdef$res)
hist(morfilter3, freq=F, add=T)
abline(v=morpmLdef$statistic, lwd=2, col=4)

####################################################
####Moran I for residual spatial autocorrelation####
####################################################
#linear model natural variables
data.lm <- lm(D10.00 ~ bov10.00 + changeCrop + fra_PAs, data=data1)
summary(data.lm)
#resid autocor
data.moran <- lm.morantest(data.lm,wtsqueen,alternative="two.sided")
print(data.moran)
data.resid <- resid(data.lm)
data.moran2 <- moran.test(data.resid,wtsqueen,randomisation=FALSE,alternative="two.sided")
print(data.moran2)

#linear model log variables
datalog.lm <- lm(lD10.00 ~ lBov10.00 + lchangecrp + logfra_PA, data=data1)
summary(datalog.lm)
#resid autocor
data.moran3 <- lm.morantest(datalog.lm,wtsqueen,alternative="two.sided")
print(data.moran3)
data.resid1 <- resid(datalog.lm)
data.moran4 <- moran.test(data.resid1,wtsqueen,randomisation=FALSE,alternative="two.sided")
print(data.moran4)

####################################################
################spatial lag model###################
####################################################
#constructing the lag variable
str(data1)
vars.mat <- cbind(bov10.00,lBov10.00,D10.00,lD10.00,changeCrop,lchangecrp,fra_PAs,logfra_PA)
summary(vars.mat)
lagvars.mat <- lag.listw(wtsqueen,vars.mat)
summary(lagvars.mat)
#moran's scatter plot
moran.plot(bov10.00,wtsqueen)
#create standardised variable
bov<-bov10.00
stdbov<-(bov-mean(bov))/sd(bov)
mean(stdbov)
var(stdbov)
#create spatial lag for std variable
wtbov<-wtsqueen
lagwtbov<-lag.listw(wtbov,stdbov)
#compute intercept and slope
morlm<-lm(lagwtbov ~ stdbov)
aa <- morlm$coefficients[1]
intcpt <- morlm$coefficients[2]
aa
intcpt
par(pty="s") # make sure it's square
plot(stdbov,lagwtbov,xlab="Bovines",ylab="Spatial Lag of Bovines")
abline(aa,intcpt)
abline(h=0,lty=2)
abline(v=0,lty=2)
title(paste("Moran Scatterplot I= ",format(round(intcpt,4))))

#executing the moran plotting package
moran.plotData(lD10.00,wtsqueen)
str(data1)

####################################################
################Local moran's I#####################
####################################################
locMbov<-localmoran(bov10.00, listw=wtsqueen)
summary(locMbov)

##manually creating a moran plot standardized variable
#data1$sBov<-scale(data1$bov10.00)
#create a lagged variable
#data1$lag_sBov<-lag.listw(wtsqueen,data1$sBov)
#summary(data1$sBov)
#summary(data1$lag_sBov)
#plot(x=data1$sBov, y=data1$lag_sBov, main="Moran scatterplot Bovines")
#abline(h = 0, v = 0)
#abline(lm(data1$lag_sBov ~ data1$sBov), lty = 3, lwd = 4, col = "red")

####################################################
#########Local moran's I correlograms###############
####################################################

# correlograms for Moran's I and the autocorrelation coefficient
sp.plotCor(datagal,D10.00)
sp.plotCor(datagal,bov10.00)
sp.plotCor(datagal,changeCrop)
sp.plotCor(datagal,fra_PAs)
sp.plotCor(datagal,lD10.00)
sp.plotCor(datagal,lBov10.00)
sp.plotCor(datagal,logfra_PA)
sp.plotCor(datagal,lchangecrp)

# examining correlation as a function of the distance
sp.plotCorr(datagal,D10.00)
sp.plotCorr(datagal,bov10.00)
sp.plotCorr(datagal,changeCrop)
sp.plotCorr(datagal,fra_PAs)
sp.plotCorr(datagal,lD10.00)
sp.plotCorr(datagal,lBov10.00)
sp.plotCorr(datagal,logfra_PA)
sp.plotCorr(datagal,lchangecrp)


####################################################
##################SAR model#########################
####################################################
ols_lmnat<-lm(D10.00 ~ bov10.00 + changeCrop + fra_PAs,data=data1)
plot(ols_lmlog)
ols_lmlog<-lm(lD10.00 ~ lBov10.00 + lchangecrp + logfra_PA,data=data1)
summary(ols_lmlog)
list(summary(ols_lmnat),summary(ols_lmlog))
list(plot(ols_lmnat),plot(ols_lmlog))
#ols_lmnat$resid<-resid(ols_lmnat)
#ols_lmlog$resid<-resid(ols_lmlog)
morNat<-lm.morantest(ols_lmnat,wtsqueen)
morNat
morLog<-lm.morantest(ols_lmlog,wtsqueen)
morLog

lag_sarnat<-lagsarlm(D10.00 ~ bov10.00 + changeCrop + fra_PAs,data=data1,wtsqueen,tol.solve=1.0e-12)
summary(lag_sarnat)
lag_sarlog<-lagsarlm(lD10.00 ~ lBov10.00 + lchangecrp + logfra_PA,data=data1,wtsqueen,tol.solve=1.0e-12)
summary(lag_sarlog)
sar_sarnat<-spautolm(D10.00 ~ bov10.00 + changeCrop + fra_PAs,data=data1,wtsqueen,family="SAR")
sar_sarlog<-spautolm(lD10.00 ~ lBov10.00 + lchangecrp + logfra_PA,data=data1,wtsqueen,family="SAR")
anova(lag_sarlog)
#varsdf<-data.frame(D10.00,bov10.00,changeCrop,fra_PAs)
#class(varsdf)
summary(sar_sarlog)
anova(lag_sarnat,ols_lmnat)
anova(lag_sarlog,ols_lmlog)
AIC(sar_sarnat,sar_sarlog)
AIC(ols_lmlog)
