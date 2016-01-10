reqVars3 <- read.csv("E:/SEMESTER_4/REDDPACdata/reqvariable/reqVars3.csv")
reqBovines <- read.csv("E:/SEMESTER_4/REDDPACdata/reqvariable/reqBovines.csv")

####################################################
###########gamma / weibull model####################
###########generalized linear model#################
####################################################

str(reqVars3)
par(bg="grey84")

exp.gamma.model1 <- glm(reqVars3$D10.00 ~ reqVars3$bov10.00, family = Gamma(link = "log"))
summary(exp.gamma.model1)
plot(exp.gamma.model1)


exp.gamma.model2 <- glm(reqVars3$D10.00 ~ reqVars3$changeCrop, family = Gamma(link = "log"))
summary(exp.gamma.model2)
plot(exp.gamma.model2)

exp.gamma.model3 <- glm(reqVars3$D10.00 ~ reqVars3$fra_PAs, family = Gamma(link = "log"))
summary(exp.gamma.model3)
plot(exp.gamma.model3)

####################################################
###########multivariate gamma model#################
####################################################
attach(data1)
exp.gamma.model <- glm(D10.00 ~ bov10.00 + changeCrop + fra_PAs, family = Gamma, data=data1)
summary(exp.gamma.model)
plot(exp.gamma.model)
resid.model1 <- resid(exp.gamma.model)
hist(resid.model1, main = "Histogram of model residuals", xlab = "Residuals")
anova(exp.gamma.model)


exp.gamma.logmodel <- glm(lD10.00 ~ lBov10.00 + lchangecrp + logfra_PA, family = Gamma, data=data1)
plot(exp.gamma.logmodel)
summary(exp.gamma.logmodel)
AIC(exp.gamma.model,exp.gamma.logmodel)

AICc(exp.gamma.model)


exp.gamma.model11 <- glm(D10.00 ~ bov10.00 + changeCrop , family = Gamma, data=data1)
summary(exp.gamma.model11)

exp.gamma.model22 <- glm(lD10.00 ~ lBov10.00 + lchangecrp , family = Gamma, data=data1)
summary(exp.gamma.model22)
