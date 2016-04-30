# Sensitivity analyses:
require("VGAM")

# For rabies free areas create proxies for RP and coverage
BI0 = which(d$BI==0)
d$BI = replace(d$BI, BI0 , 1/100000000)
d$BI_U = replace(d$BI_U, BI0 , 2/100000000)

RP0 = which(is.na(d$mRP))
d$mRP = replace(d$mRP, RP0 , 1/100000000)
d$lRP = replace(d$lRP, RP0 , 0/100000000)
d$uRP = replace(d$uRP, RP0 , 2/100000000)

cov0 = which(is.na(d$Coverage))
d$Coverage = replace(d$Coverage, cov0, 0.7)
d$Cov_L = replace(d$Cov_L, cov0, 0)
d$Cov_U = replace(d$Cov_U, cov0, 1)


# Binomial permutation probability 
sprob <- function(ns, nf){
  vals <- c(0, sort(runif(ns+nf)), 1)
  return(runif(n=1, min=vals[ns+1], max=vals[ns+2]))
}

permProb <- function(ns, nf, reps){
  return(replicate(reps, sprob(ns=ns, nf=nf)))
}
# TESTING:
# sample(permProb(ns=55, nf=74-55, reps=1000), 1)

# Reverse engineer binomial probabilities from Shim et al. 2009
DPl <- 0.13; DPu <- 0.28 # from Shim et al. 2009
binconf(23,121)

CalcBurdenUncertainty = function(output){
  # Estimate uncertainty associated with burden estimates by drawing from parameter ranges
  # Inputs: ranges of parameter values and measure of interest
  # Returns: random draw of measure of interest
  
  bi = qtriangle(p=runif(1), theta=d$BI, lower=d$BI_L*.9, upper=d$BI_U*1.1) # BI delphi range
  cr = qtriangle(p=runif(1), theta=d$Coverage, lower=d$Cov_L, upper=d$Cov_U) # Coverage delphi range
  rp = sample(permProb(ns=55, nf=74-55, reps=1000), 1) * 
    meanFun(LACfit$Imax, LACfit$Iresp, cr)/ LACfit$Imax  # RP - Probability that exposure is by a rabid animal
  pp = PPrange[order(RPcountry$NUM), round(runif(1, 1, 1000))] # PP - Probability of receiving PEP given an exposure
  dp = sample(permProb(ns=23, nf=121-23, reps=1000), 1) # DP - probability that in absence of PEP develop rabies
  
  if(output == "deaths"){return(d$RISK * d$pop * bi * rp * (1-pp) * dp)}
  if(output == "exposures"){return(d$RISK * d$pop * bi * rp)}
  if(output == "PEP"){return(d$RISK * d$pop * bi * pp)}
  if(output == "prevented"){return(d$RISK * d$pop * bi * rp * pp * dp)}   
  
  if(output == "DALYdeath"){
    ds = d$RISK * d$pop * bi * rp * (1-pp) * dp
    return(ds * YLLcalc(LTvar="LE", LTvalues=GBD2010$LE, cause="rabies", discount = 0, C = 1, Beta = 0, alpha = 0)) # GBD2010 use of LE  
  }
  
  if(output == "DALYntv"){
    ntvs = d$RISK * d$pop * bi * pp * d$NTV
    semple_death = YLLcalc(LTvar="LE", LTvalues=GBD2010$LE, cause="vaccine", discount = 0, C = 1, Beta = 0, alpha = 0) * ntvs * d$SMP * DALYvacc$Perc[1]   
    smb_death = YLLcalc(LTvar="LE", LTvalues=GBD2010$LE, cause="vaccine", discount = 0, C = 1, Beta = 0, alpha = 0) * ntvs * d$SMB * DALYvacc$Perc[2]      
    semple_disability =  d$SMP * ntvs * YLDcalc(DALYvacc$duration[3], DALYvacc$level[3], DALYvacc$Percentage[3])
    smb_disability = d$SMB * ntvs * YLDcalc(DALYvacc$duration[4], DALYvacc$level[4], DALYvacc$Percentage[4])
    return(semple_death + smb_death + semple_disability + smb_disability)
  }
  
  if(output == "DALYanxiety"){
    pep = d$RISK * d$pop * bi * pp
    return(pep * YLDcalc(DALYvacc$duration[5], DALYvacc$level[5], DALYvacc$Percentage[5])) 
  } 
}
# sum(CalcBurdenUncertainty("deaths"))

CalcCIs=function(n, output, scale){
  # Calculate burden confidence intervals
  # Inputs: measure of interest, number of iterations
  # Returns: mean, median and 95% CIs of measure of interest
  
  sens_matrix=matrix(nrow=nrow(d), ncol=n)
  for(i in 1:n){sens_matrix[,i] = CalcBurdenUncertainty(output)}
  
  if(scale=="country"){
    m = apply(sens_matrix, 1, mean, na.rm=TRUE)
    med = apply(sens_matrix, 1, quantile, probs=0.5, na.rm=TRUE)
    lci = apply(sens_matrix, 1, quantile, probs=0.025, na.rm=TRUE)
    uci = apply(sens_matrix, 1, quantile, probs=0.975, na.rm=TRUE)
    return(cbind(m, med, lci, uci))
  }
  
  if(scale=="global"){
    c(mean(apply(sens_matrix, 2, sum, na.rm=TRUE)), 
      quantile(apply(sens_matrix, 2, sum, na.rm=TRUE), c(0.5, 0.025, 0.975)))
  }
}

deaths_CI = CalcCIs(1000, "deaths", "global"); deaths_CI
exp_CI = CalcCIs(1000, "exposures", "global"); exp_CI
prev_death_CI = CalcCIs(1000, "prevented", "global"); prev_death_CI
DALYrabies_CI = CalcCIs(1000, "DALYdeath", "global"); DALYrabies_CI
DALYntv_CI = CalcCIs(1000, "DALYntv", "global"); DALYntv_CI
DALYanxiety_CI = CalcCIs(1000, "DALYanxiety", "global"); DALYanxiety_CI
DALY_CI = DALYrabies_CI+DALYntv_CI

deaths_country_CI = CalcCIs(1000, "deaths", "country")
d$deaths_lci=deaths_country_CI[,3]
d$deaths_uci=deaths_country_CI[,4]

exp_country_CI = CalcCIs(1000, "exposures", "country")
d$exp_lci=exp_country_CI[,3]
d$exp_uci=exp_country_CI[,4]

PEP_country_CI = CalcCIs(1000, "PEP", "country")
d$PEP_lci=PEP_country_CI[,3]
d$PEP_uci=PEP_country_CI[,4]

prev_death_country_CI = CalcCIs(1000, "prevented", "country")
d$prev_death_lci=prev_death_country_CI[,3]
d$prev_death_uci=prev_death_country_CI[,4]

DALYdeath_country_CI = CalcCIs(1000, "DALYdeath", "country")
d$DALYdeath_lci=DALYdeath_country_CI[,3]
d$DALYdeath_uci=DALYdeath_country_CI[,4]

DALYntv_country_CI = CalcCIs(1000, "DALYntv", "country")
d$DALYntv_lci=DALYntv_country_CI[,3]
d$DALYntv_uci=DALYntv_country_CI[,4]

DALYanxiety_country_CI = CalcCIs(1000, "DALYanxiety", "country")


# Extrapolate uncertainty to costs:
# DIRECT
dpv=4
rig = with(d, ((HRIG * HRIGcost) + (ERIG * ERIGcost))) # RIG
ntv = with(d, (NTV * ((NTVcost * ND_NTV) + (FIRST_CONSULT + ((ND_NTV-1)*SUB_CONSULT))) ))  # NTV
im = with(d, (TCV_IM * ((TCVcost * ND_IM) + ( ((ND_IM-1)*SUB_CONSULT)+FIRST_CONSULT )) ))  # IM
id = with(d, ((TCV_ID * TCVcost/dpv * ND_ID) + (TCV_ID * ((ND_ID-1)*SUB_CONSULT)+FIRST_CONSULT))) # ID
DC = CalcCIs(1000, "PEP", "country") * apply(cbind(rig, ntv, im, id), 1, sum); sum(DC[,1])
d$DClci = DC[,3]; d$DCuci = DC[,4]
  
TC = CalcCIs(1000, "PEP", "country") * (n_trips + trip_cost); sum(TC[,1]) # TRAVEL
d$TClci = TC[,3]; d$TCuci = TC[,4]

li = CalcCIs(1000, "PEP", "country") * n_trips * d$GDP.US./365; sum(li[,1])  # ST Lost income
d$LIlci = li[,3]; d$LIuci = li[,4]

pd = CalcCIs(1000, "DALYdeath", "country") * d$GDP.US.; sum(pd[,1])  # Premature deaths   
d$PDlci = pd[,3]; d$PDuci = pd[,4]

d$dog_vacc = d$Human * d$DVAC * d$DVAC_C  # Dog vaccination 
d$dpm = with(d, Human_population_2010 * ((DSTE*DSTE_C) + (DKIL*DKIL_C)))  # Dog pop mgmt 
d$surveillance = d$Human * d$TEST * d$TEST_C  # Surveillance - delphi estimates for tests performed
d$LLu = with(d, LLpcL * Human_population_2010 * L.cost.per) # Livestock losses 
d$LLl = with(d, LLpcU * Human_population_2010 * L.cost.per)

costsL=cbind(DC[,3], TC[,3], li[,3], pd[,3], d$dog_vacc, d$dpm, d$surveillance, d$LLl)
costsU=cbind(DC[,4], TC[,4], li[,4], pd[,4], d$dog_vacc, d$dpm, d$surveillance, d$LLu)
sum(costsL, na.rm=TRUE)
sum(costsU, na.rm=TRUE)

write.csv(d, "burden.csv", row.names=FALSE)

#################################################################
#Sensitivity plot - for the number of human rabies deaths
#################################################################

# function to calculate deaths
CB=function(BI, RRP, VC, PP, DP){
  RP = RRP*meanFun(LACfit$Imax, LACfit$Iresp, VC)/ LACfit$Imax   
  deaths = d$RISK * d$pop * BI * RP * (1-PP) * DP
  sum(deaths, na.rm=TRUE)
}
CB(BI = d$BI, RRP = RRP, VC = d$Coverage, PP = d$PP, DP = DP)

# 1. BI
CBsensBI=function(n){
  bi = qtriangle(p=runif(1), theta=d$BI, lower=d$BI_L*.9, upper=d$BI_U*1.1)  # BI delphi range
  CB(BI = bi, RRP = RRP, VC = d$Coverage, PP = d$PP, DP = DP)
}
sBI=replicate(1000, CBsensBI(1))


# 2. RRP - Rabies recognition probability and CIs - from Lembo et al. 08
rrp=binconf(55, 74) 
CBsensRRP = function(n){
  rrp = sample(permProb(ns=55, nf=74-55, reps=1000), 1)
  CB(BI = d$BI, RRP = rrp, VC = d$Coverage, PP = d$PP, DP = DP)
}
sRRP=replicate(1000, CBsensRRP(1))

# 3. Vaccination Coverage - change by delphi choices
CBsensVC=function(){  
  vc = qtriangle(p=runif(1), theta=d$Coverage, lower=d$Cov_L, upper=d$Cov_U) # Coverage delphi range
  CB(BI = d$BI, RRP = RRP, VC = vc, PP = d$PP, DP = DP)
}
sVC=replicate(1000, CBsensVC())

# 4. PP - Probability of receiving PEP given an exposure
CBsensPEP=function(){
  PPn = PPrange[order(RPcountry$NUM), ceiling(runif(1, 0, 1000))]
  CB(BI = d$BI, RRP = RRP, VC = d$Coverage, PP = PPn, DP = DP)
  }
sPEP=replicate(1000, CBsensPEP()); hist(sPEP)

# 5. DP = 0.19 #from Shim et al. 2009
DPl <- 0.13; DPu <- 0.28
CBsensP3=function(n){
  dp=sample(permProb(ns=23, nf=121-23, reps=1000), 1)
  CB(BI = d$BI, RRP = RRP, VC = d$Coverage, PP = d$PP, DP = dp)
}
sDP=replicate(1000, CBsensP3(1)); hist(sDP)

#################################################################
#RANGES
BIrange=range(sBI); var(sBI)
RRPrange=range(sRRP); var(sRRP)
VCrange=range(sVC); var(sVC)
PEPrange=range(sPEP); var(sPEP)
DPrange=range(sDP); var(sDP)

ranges=rbind(BIrange, RRPrange, VCrange, PEPrange, DPrange)

postscript(file="burden_sensitivity.eps", width=5, height=4, horizontal=F)
par(mfrow=c(1,1), mgp=c(2,0.25,0), tck=0.025, cex=0.7, plt=c(0.5, 0.8, 0.05, 0.95))
boxplot(cbind(sBI, sRRP, sVC, sPEP, sDP), outline=FALSE,
        col="black", 
        ylim=c(0,190000), 
        axes=FALSE, staplewex = 0.25, boxwex=0.8)
axis(4)
mtext(side=4, text="Variation in mortality estimate due to parameter uncertainty", line=1.5, cex=0.7)
text(x=1:5, y=rep(160000, 5), c("Bite incidence", expression(italic("RP"[max])),
                                "Vaccination Coverage", expression(italic("PP")), 
                                expression(italic("DP"))), srt=90)
dev.off()


