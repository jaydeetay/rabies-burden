########################################################################
# Examine relationship between coverage and incidence in dogs and livestock:
########################################################################

# Libraries, packages, data
library(bbmle) 

# takes files:
  # CovInc.csv
  # vcountry2.csv; vcluster2.csv
  # livestock_rabies.csv

# produces 
  # LAC_params.csv; livestock_params.csv
  # RPcluster.csv; RPcountry.csv <-- not true, but vars are created.

require("Hmisc")  # Added by johntaylor

#functions fitting coverage to incidence assuming Gamma errors
meanFun <- function(maxInc, respShape, cov){
  return(maxInc*(1-cov)^respShape)
}

incGamma=function(Imax, Iresp, Igam, inc, cov){ #Log to improve convergence
  maxInc<-exp(Imax)
  respShape<-exp(Iresp)
  gamShape<-exp(Igam)
  m = meanFun(maxInc, respShape, cov)
  scale = m/gamShape
  -sum(dgamma(inc, scale=scale, shape=gamShape, log=TRUE))
}


# Data on vaccination coverage (2 yr average) and incidence from Latin America & Caribbean (LAC)
LAC=read.csv("CovInc.csv")  # from Belotto et al. 2005 in Virus Research & REDIPRA reports
LAC <- subset(LAC, !is.na(coverage) & (incidence>0))

# Initial fit:
mLAC=mle2(incGamma,
	start=list(Imax=log(0.003), Iresp=0, Igam=log(3)), 
	data=list(cov=LAC$cov/100, inc=LAC$inc)
)
summary(mLAC)
LAC_ci=confint(mLAC)
LACfit=as.list(exp(coef(mLAC)))
LAClci=as.list(exp(LAC_ci[,1]))
LACuci=as.list(exp(LAC_ci[,2]))
write.csv(cbind(LACfit, LAClci, LACuci), "LAC_params.csv")


# Calculate relationship for probability that bite is by rabid animal (RP)
rrp = binconf(55, 74) # Rabies recognition probability and CIs - from Lembo et al. 08
RRP <- rrp[1]
RRPmin <- rrp[2]; RRPmax <- rrp[3]
VC=seq(0,1,0.01)  # Vaccination coverage from 0-100%
RP=(RRP/mLAC@coef[["Imax"]]) * meanFun(mLAC@coef[["Imax"]], mLAC@coef[["Iresp"]], VC)


# Import country & cluster vaccination coverage data from literature & questionnaires
country=read.csv("vcountry2.csv") 
cluster=read.csv("vcluster2.csv")

# Add RP and CIs for countries and clusters 
mRP = RRP*meanFun(LACfit$Imax, LACfit$Iresp, country$Cov)/ LACfit$Imax
uRP=RRPmax*meanFun(LACfit$Imax, LACfit$Iresp, country$Cov)/LACfit$Imax
lRP=RRPmin*meanFun(LACfit$Imax, LACfit$Iresp, country$Cov)/LACfit$Imax
RPcountry=cbind(country, mRP, lRP, uRP)

mRP = RRP*meanFun(LACfit$Imax, LACfit$Iresp, cluster$Cov)/ LACfit$Imax
uRP=RRPmax*meanFun(LACfit$Imax, LACfit$Iresp, cluster$Cov)/LACfit$Imax
lRP=RRPmin*meanFun(LACfit$Imax, LACfit$Iresp, cluster$Cov)/LACfit$Imax
RPcluster=cbind(cluster, mRP, lRP, uRP)

# mRP=(RRP/mLAC@coef[["Imax"]] * meanFun(mLAC@coef[["Imax"]], mLAC@coef[["Iresp"]], cluster$Cov))
# uRP=(RRPmax/LAC_ci[1,2]) * meanFun(LAC_ci[1,2], LAC_ci[2,2], cluster$Cov); uRP=replace(uRP, which(uRP>1), 1)
# lRP=(RRPmin/LAC_ci[1,1]) * meanFun(LAC_ci[1,1], LAC_ci[2,1], cluster$Cov)

# RELATIONSHIP WITH LIVESTOCK INCIDENCE
livestock=read.csv("livestock_rabies.csv")
livestock$incidence[which(livestock$inc==0.0053408)]=NA  # Severe outbreak in previously free area - exclude bc relatively high incidence? 
livestock <- subset(livestock, !is.na(incidence)&!is.na(vaccination.coverage))

mLivestock=mle2(incGamma,
         start=list(Imax=log(0.003), Iresp=0, Igam=log(3)), 
         data=list(cov=livestock$vac, inc=livestock$inc),
)
summary(mLivestock)
mLivestock_ci=confint(mLivestock)
Lfit=as.list(exp(coef(mLivestock)))
Llci=as.list(exp(mLivestock_ci[,1]))
Luci=as.list(exp(mLivestock_ci[,2]))
write.csv(cbind(Lfit, Llci, Luci), "livestock_params.csv")

# livestock losses per country estimated from extrapolated coverage in burden_1.R


