########################################################################
# PROBABILITY OF GETTING PEP - PP

library(lme4)
library("ggplot2")

pPEP=function(deaths, exposures, RP){
  # Function to calculate PP based on deaths, exposures, & RP (probability animal rabid based on vacc coverage)
  # Requires DP (prob death | exposure & no PEP)
  totalexposures=((1/RP)*(deaths*(1/DP)))+exposures
  exposures/totalexposures
}

DP = 0.19 #from Shim et al. 2009
incidence$PPraw=pPEP(incidence$Deaths, incidence$Exposures, incidence$RP)

incidence <- within(incidence, {
  ctry <- as.factor(Country) 
  treat <- round(Exposures.yr * RP)
  untreat <- round(Deaths.yr / DP)
  tot <- treat+untreat
  tprop <- treat/tot
  uprop <- untreat/tot
})

# Fit random effects mobel to assess whether HDI predicts the probability of receiving PEP (PP)
yd <- na.omit(incidence[,c("ctry", "tprop", "HDI", "tot")])
tMod <- glmer(tprop ~ HDI + (1|ctry), family=binomial, weight=tot, data=yd)
print(summary(tMod))

# Examine data and model
lattice::dotplot(ranef(tMod,condVar=TRUE)) 
g1 <- ggplot(yd, aes(HDI, tprop, colour=ctry)) + geom_point(aes(size=log10(tot)), alpha=0.5)
print(summary(tMod))


# Bootstrap to generate prediction intervals for plot
nd <- data.frame(HDI=seq(0.25,0.85,length=51))
rf <- function(x) {
  predict(update(tMod, data=transform(yd,tprop=x)),
          newdata=nd,
          re.form=NA,
          type="response")
}

set.seed(101)
tBoot <- sapply(simulate(tMod, 1000), rf)
pp <- t(apply(tBoot, 1, quantile, c(0.025,0.975)))
pp <- setNames(as.data.frame(pp),c("lwr","upr"))
pp <- data.frame(nd,tprop=predict(tMod,newdata=nd,re.form=NA,
                                  type="response"),pp)

g1 + geom_line(data=pp, colour="black") +
  geom_ribbon(data=pp, aes(ymin=lwr, ymax=upr), colour=NA, alpha=0.3)


# Bootstrap to generate prediction intervals 
RPcountry$HDI = pop$hdi[match(RPcountry$CODE, pop$CODE)]
RPcountry$pop = pop$X2010[match(RPcountry$CODE, pop$CODE)]

nd <- data.frame(HDI=sort(na.omit(RPcountry$HDI)))
set.seed(101)
tBoot2 <- sapply(simulate(tMod, 1000), rf)
PP <- t(apply(tBoot2, 1, quantile, c(0.025,0.975)))
PP <- setNames(as.data.frame(PP),c("lwr","upr"))
PP <- data.frame(nd,tprop=predict(tMod, newdata=nd, re.form=NA, type="response"), PP)
write.csv(PP, "PP.csv", row.names=FALSE)

# y=cbind(incidence$Exposures*incidence$RP, incidence$Deaths/DP) #logistic regression, y takes 2 columns
# mPP=glm(y~incidence$HDI, binomial); summary(mPP) #log(incidence$HDI) is no better (resid dev larger)

#Generate bootstraps for all countries given their HDI to generate CIs
nd <- data.frame(HDI=na.omit(RPcountry$HDI))
set.seed(101)
tBoot3 <- sapply(simulate(tMod, 500), rf)
PPrange <- t(apply(tBoot3, 1, quantile, seq(0,1,length=1000)))
PPcode <- RPcountry$CODE


