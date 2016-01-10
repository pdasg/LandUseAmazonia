source("E:/SEMESTER_4/Thesis/Rwork/loadingPKGS.R")
####################################################
################complete dataset####################
####################################################
reqvars1 <- read.csv("E:/SEMESTER_4/REDDPACdata/reqvariable/reqvars1.csv")
reqvars3 <- read.csv("E:/SEMESTER_4/REDDPACdata/reqvariable/reqVars3.csv")
str(reqvars1)
str(reqvars3)
summary(reqvars3)
plot(density(reqvars3$D10.00), main = "x = Density curve (change in deforestation)", lwd = 2)
plot(density(reqvars3$bov10.00), main = "x = Density curve (change in bovines)", lwd = 2)
summary(reqvars3)
qqnorm(reqvars3$D10.00)
qqPlot(reqvars3$D10.00, "gamma", xlab ="Quantiles for Deforestation" , ylab = "Quantiles from gamma distribution")
qqnorm(reqvars3$D2010)
###which distribution fits the data?
descdist(reqvars3$D2010, boot = 1000)
descdist(reqvars3$D2000, boot = 1000)
descdist(reqvars3$D10.00, boot = 500)
descdist(reqvars3$bov10.00)
descdist(reqvars3$changeCrop)
descdist(reqvars3$logfra_PA)

par(bg="grey84")
plotdist(reqvars3$D2010, histo = TRUE, demp = TRUE)
plotdist(reqvars3$D10.00, histo = TRUE, demp = TRUE)
####################################################
################Bovines Data########################
####################################################
#correlation with change Data (normal and log)
cor(reqvars3$bov10.00, reqvars3$D10.00, method = "spearman")
plot(reqvars3$bov10.00, reqvars3$D10.00, xlab = "Bovines", ylab = "deforestation")
lines(lowess(reqvars3$bov10.00, reqvars3$D10.00), col = "blue")
#hist(reqvars3$bov10.00, reqvars3$D10.00)
cor(reqvars3$lBov10.00, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$lBov10.00, reqvars3$lD10.00, xlab = "log Bovines", ylab = "log deforestation"xlab = "Bovines change", ylab = "deforestation change")
lines(lowess(reqvars3$lBov10.00, reqvars3$lD10.00), col = "blue")

reqBovines <- read.csv("E:/SEMESTER_4/REDDPACdata/reqvariable/reqBovines.csv")
cor(reqBovines$bov10.00, reqBovines$D10.00)

#correlation with accumulated Data (normal and log)
cor(reqvars3$t_bov10, reqvars3$D2010, method = "spearman")
plot(reqvars3$t_bov10, reqvars3$D2010)
cor(reqvars3$lBov10, reqvars3$lD2010, method = "spearman")
plot(log(reqvars3$lBov10+1), log(reqvars3$lD2010+1))
cor(log(reqvars3$lBov10+1), log(reqvars3$lD2010+1))
lines(lowess(log(reqvars3$lBov10+1), log(reqvars3$lD2010+1)), col = "blue", lwd = 2)
####################################################
################Protected areas#####################
####################################################
#correlation with change in deforestation (normal and log)
cor(reqvars3$fra_PAs, reqvars3$D10.00, method = "spearman")
plot(reqvars3$fra_PAs, reqvars3$D10.00, xlab = "PAs", ylab = "deforestation")
lines(lowess(reqvars3$fra_PAs, reqvars3$D10.00), col = "blue")
cor(reqvars3$logfra_PA, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$logfra_PA, reqvars3$lD10.00)

#correlation with accumulated in deforestation (normal and log)
cor(reqvars3$fra_PAs, reqvars3$D2010, method = "spearman")
plot(reqvars3$fra_PAs, reqvars3$D2010)
cor(reqvars3$logfra_PA, reqvars3$lD2010, method = "spearman")
plot(reqvars3$logfra_PA, reqvars3$lD2010)

####################################################
######################crop data#####################
####################################################

#correlation with change data (normal and log)
cor(reqvars3$changeCrop, reqvars3$D10.00, method = "spearman")
plot(reqvars3$changeCrop, reqvars3$D10.00, xlab = "Cropland", ylab = "deforestation")
lines(lowess(reqvars3$changeCrop, reqvars3$D10.00), col = "blue")
cor(log(reqvars3$changeCrop+1), reqvars3$lD10.00, method = "spearman")
plot(log(reqvars3$changeCrop+1), reqvars3$lD10.00)
lines(lowess(log(reqvars3$changeCrop+1), reqvars3$lD10.00), col = "blue", lwd = 1)
#correlation with accumulated data (normal and log)
cor(reqvars3$cropACM10, reqvars3$D2010, method = "spearman")
plot(reqvars3$cropACM10, reqvars3$D2010)
cor(log(reqvars3$cropACM10 +1), reqvars3$lD2010, method = "spearman" )
plot(log(reqvars3$cropACM10 +1), reqvars3$lD2010)


####################################################
##############Secondary Vegetation##################
####################################################
#correlation with change in deforestation (normal and log)
cor(reqvars3$VS.2010, reqvars3$D10.00, method = "spearman")
plot(reqvars3$VS.2010, reqvars3$D10.00)
cor(reqvars3$logVS_10, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$logVS_10, reqvars3$lD10.00)

#correlation with accumulated in deforestation (normal and log)
cor(reqvars3$VS.2010, reqvars3$D2010, method = "spearman")
plot(reqvars3$VS.2010, reqvars3$D2010)
cor(reqvars3$logVS_10, reqvars3$lD2010, method = "spearman")
plot(reqvars3$logVS_10, reqvars3$lD2010)


####################################################
###########dataframes for corr. matix###############
####################################################

#original Variables
vars <- data.frame( reqvars3$D10.00, reqvars3$bov10.00, reqvars3$fra_PAs, reqvars3$changeCrop )

write.csv(vars, "E:/SEMESTER_4/Thesis/Rwork/vars.csv")
vars.csv <- read.csv("E:/SEMESTER_4/Thesis/Rwork/vars.csv")
summary(vars.csv)

pairs(vars)

varsR <- cor(vars)
varsR
write.csv(varsR, "E:/SEMESTER_4/Thesis/Rwork/varsR.csv")










####################################################
#######mining, burnt, reforested, Urban area########
####################################################
#mining
cor(reqvars3$miningArea, reqvars3$D10.00, method = "spearman")
plot(reqvars3$miningArea, reqvars$D10.00)
cor(reqvars3$logMining, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$logMining, reqvars3$lD10.00)

cor(reqvars3$miningArea, reqvars3$D2010, method = "spearman")
plot(reqvars3$miningArea, reqvars3$D2010)
cor(reqvars3$logMining, reqvars3$lD2010, method = "spearman")
plot(reqvars3$logMining, reqvars3$lD2010)

#Burnt area
cor(reqvars3$burntArea, reqvars3$D10.00, method = "spearman")
plot(reqvars3$burntArea, reqvars3$D10.00)
cor(reqvars3$logBurnt, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$logBurnt, reqvars3$lD10.00)

cor(reqvars3$burntArea, reqvars3$D2010, method = "spearman")
plot(reqvars3$burntArea, reqvars3$D2010)
cor(reqvars3$logBurnt, reqvars3$lD2010, method = "spearman")
plot(reqvars3$logBurnt, reqvars3$lD2010)

#reforest
cor(reqvars3$reforArea, reqvars3$D10.00, method = "spearman")
plot(reqvars3$reforArea, reqvars3$D10.00)
cor(reqvars3$logRefor, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$logRefor, reqvars3$lD10.00)

cor(reqvars3$reforArea, reqvars3$D2010, method = "spearman")
plot(reqvars3$reforArea, reqvars3$D2010)
cor(reqvars3$logRefor, reqvars3$lD2010, method = "spearman")
plot(reqvars3$logRefor, reqvars3$lD2010)

#urban
cor(reqvars3$mod10Urbn, reqvars3$D10.00, method = "spearman")
plot(reqvars3$mod10Urbn, reqvars3$D10.00)
cor(reqvars3$logUrban, reqvars3$lD10.00, method = "spearman")
plot(reqvars3$logUrban, reqvars3$lD10.00)

cor(reqvars3$mod10Urbn, reqvars3$D2010, method = "spearman")
plot(reqvars3$mod10Urbn, reqvars3$D2010)
cor(reqvars3$logUrban, reqvars3$lD2010, method = "spearman")
plot(reqvars3$logUrban, reqvars3$lD2010)
