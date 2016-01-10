source("E:/SEMESTER_4/Thesis/Rwork/loadingPKGS.R")
####################################################
###########visualizing the distribution#############
####################################################
setwd("E:/SEMESTER_4/REDDPACdata/reddpacDS")
def_past<-read.csv("defnpastAmBm.csv")
#csvPDdata <- read.csv("E:/SEMESTER_4/REDDPACdata/defNpast.csv")
str(def_past)
hist(def_past$D10.00,main="Histogram of observed data")
hist(log(def_past$D10.00 + 1),main="Histogram of log trans observed data")
plot(density(def_past$D10.00),main="Density estimate of deforestation data")
plot(ecdf(def_past$D10.00),main = "ECD deforestation")
skewness(def_past$D10.00)


####################################################
###########checking for normality###################
####################################################
qqnorm(def_past$D10.00)
qqline(def_past$D10.00)

#formal testing of normality using Shapiro wilk test
shapiro.test(def_past$D10.00)
shapiro.test(csvData$pasture)
shapiro.test(csvData$soypl)
shapiro.test(csvData$cornpl)
shapiro.test(csvData$secVeg)

#formal testing of normality using Anderson-Darling test
ad.test(csvData$defor_all)
ad.test(csvData$pasture)
ad.test(csvData$soypl)
ad.test(csvData$cornpl)
ad.test(csvData$secVeg)

####################################################
###########Correlation analysis#####################
####################################################

#original Variables
cor(csvData$pasture, csvData$defor_all)
cor(csvData$fra_crplnd, csvData$defor_all)
cor(csvData$fra_PAs, csvData$defor_all)
cor(csvData$cornpl, csvData$defor_all)
cor(csvData$soypl, csvData$defor_all)
cor(csvData$sugarcpl, csvData$defor_all)
cor(csvData$secVeg, csvData$defor_all)
cor(csvData$areaBurnt, csvData$defor_all)
cor(csvData$areaRefor, csvData$defor_all)
cor(csvData$modUrban, csvData$defor_all)
cor(csvData$areaMining, csvData$defor_all)

#log transformed variables
cor(csvData$logPasture, csvData$defor_all)
cor(csvData$logpcrplnd, csvData$defor_all)
cor(csvData$logpPA, csvData$defor_all)
cor(csvData$logCornpl, csvData$defor_all)
cor(csvData$logSoypl, csvData$defor_all)
cor(csvData$logSugc, csvData$defor_all)
cor(csvData$logSecveg, csvData$defor_all)
cor(csvData$logBurnt, csvData$defor_all)
cor(csvData$logRefor, csvData$defor_all)
cor(csvData$logUrbanBU, csvData$defor_all)
cor(csvData$logMining, csvData$defor_all)

#log transformed dependent variable
cor(csvData$logPasture, csvData$logDeforal)
plot(csvData$logPasture, csvData$logDeforal)
abline(lm(logDeforal~logPasture, data = csvData ), col="red") # regression line (y~x) 
lines(lowess(csvData$logPasture, csvData$logDeforal), col="blue") # lowess line (x,y)

cor(csvData$logpcrplnd, csvData$logDeforal)
cor(csvData$logpPA, csvData$logDeforal)
cor(csvData$logCornpl, csvData$logDeforal)
cor(csvData$logSoypl, csvData$logDeforal)
cor(csvData$logSugc, csvData$logDeforal)
cor(csvData$logSecveg, csvData$logDeforal)
cor(csvData$logBurnt, csvData$logDeforal)
cor(csvData$logRefor, csvData$logDeforal)
cor(csvData$logUrbanBU, csvData$logDeforal)
cor(csvData$logMining, csvData$logDeforal)

####################################################
###########dataframes for corr. matix###############
####################################################

#original Variables
origVars <- data.frame(csvData$defor_all,csvData$pasture,csvData$fra_crplnd, csvData$fra_PAs, csvData$cornpl, csvData$soypl, csvData$sugarcpl, csvData$secVeg, csvData$areaBurnt, csvData$areaRefor, csvData$modUrban, csvData$areaMining)

write.csv(origVars, "E:/SEMESTER_4/Thesis/Rwork/origVars.csv")
origVars.csv <- read.csv("E:/SEMESTER_4/Thesis/Rwork/origVars.csv")
summary(origVars.csv)

pairs(origVars.csv)

origVarsR <- cor(origVars.csv)
class(origVarsR)
write.csv(origVarsR, "E:/SEMESTER_4/Thesis/Rwork/origVarsR.csv")

#log transformed variables

logVars <- data.frame(csvData$defor_all, csvData$logPasture, csvData$logpcrplnd, csvData$logpPA, csvData$logCornpl, csvData$logSoypl, csvData$logSugc, csvData$logSecveg, csvData$logBurnt, csvData$logRefor, csvData$logUrbanBU, csvData$logMining)
DVlogVars <- data.frame(csvData$logDeforal, csvData$logPasture, csvData$logpcrplnd, csvData$logpPA, csvData$logCornpl, csvData$logSoypl, csvData$logSugc, csvData$logSecveg, csvData$logBurnt, csvData$logRefor, csvData$logUrbanBU, csvData$logMining)

write.csv(logVars, "E:/SEMESTER_4/Thesis/Rwork/logVars.csv" )
write.csv(DVlogVars, "E:/SEMESTER_4/Thesis/Rwork/DVlogVars.csv" )

logVars.csv <- read.csv("E:/SEMESTER_4/Thesis/Rwork/logVars.csv")
DVlogVars.csv <- read.csv("E:/SEMESTER_4/Thesis/Rwork/DVlogVars.csv")
summary(logVars.csv)
summary(DVlogVars.csv)
pairs(logVars.csv)
pairs(DVlogVars.csv)
logVarsR <- cor(logVars.csv)
DVlogVarsR <- cor(DVlogVars.csv)
class(logVarsR)
class(DVlogVarsR)
write.csv(logVarsR, "E:/SEMESTER_4/Thesis/Rwork/logVarsR.csv")
write.csv(DVlogVarsR, "E:/SEMESTER_4/Thesis/Rwork/DVlogVarsR.csv")

####################################################
###########correlation in terms of change###########
####################################################

csvPDdata <- read.csv("E:/SEMESTER_4/REDDPACdata/defNpast.csv")
summary(csvPDdata)
cor(csvPDdata$pastlog, csvPDdata$deforLog)
plot(csvPDdata$pastlog, csvPDdata$deforLog)
abline(lm(deforLog~pastlog, data = csvPDdata ), col="red") # regression line (y~x) 
#lines(lowess(csvPDdata$past10.00, csvPDdata$D10.00), col="blue") # lowess line (x,y)


csvPDdatapos <- read.csv("E:/SEMESTER_4/REDDPACdata/defNpastpos.csv")
summary(csvPDdatapos)
pos.Linear <- lm(deforLog~pastlog, data=csvPDdatapos )
summary(pos.Linear)
cor(csvPDdatapos$pastlog, csvPDdatapos$deforLog)
plot(csvPDdatapos$pastlog, csvPDdatapos$deforLog)
abline(lm(deforLog~pastlog, data = csvPDdatapos ), col="red") # regression line (y~x) 
#lines(lowess(csvPDdata$past10.00, csvPDdata$D10.00), col="blue") # lowess line (x,y)


csvPDdataNeg <- read.csv("E:/SEMESTER_4/REDDPACdata/defNpastneg.csv")
cor(csvPDdataNeg$past10.00, csvPDdataNeg$D10.00)
plot(csvPDdataNeg$past10.00, csvPDdataNeg$D10.00)
abline(lm(D10.00~past10.00, data = csvPDdataNeg ), col="red") # regression line (y~x) 
#lines(lowess(csvPDdata$past10.00, csvPDdata$D10.00), col="blue") # lowess line (x,y)

####################################################
###########rationale for using bovines##############
####################################################

cor(reqvars3$pastall10, reqvars3$t_bov10, method = "spearman")
par(bg="grey84")
plot(reqvars3$pastall10~reqvars3$t_bov10, xlab="Total Bovines", ylab="Total Livestock")

abline(lm(pastall10~t_bov10, data = reqvars3 ))
legend("bottomright", bty="n", legend=paste(("cor = 0.9986 " ), "\n",legend="R2 = 0.9973 " ))
?text()
?lm()
summary(lm(pastall10~t_bov10, data = reqvars3 ))
