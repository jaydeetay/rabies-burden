##########     ARRANGE DATA    ###############

# ESTIMATE PARAMETERS AND FORMAT DATA FOR MODEL:
# RP = Probability that exposure is by a rabid animal
# PP = Probability of receiving PEP given an exposure
# DP - probability that in the absence of PEP bite victim develops rabies


# 1) RELATIONSHIP BETWEEN VACCINATION COVERAGE vs INCIDENCE
source("FitCovInc.R")  # Creates: RPcountry.csv and RPcluster.csv using country and cluster coverage data (warnings generated but parameter estimates ok)

# Extrapolate country estimates of coverage and RP from cluster values
for(i in 1:length(RPcountry$Cov)){
  index = which(RPcluster$Cluster == RPcountry$Cluster[i])
  RPcountry$Coverage[i] = ifelse(is.na(RPcountry$Cov[i]), RPcluster$Cov[index], RPcountry$Cov[i])
  RPcountry$Cov_L[i] = ifelse(is.na(RPcountry$Cov_L[i]), RPcluster$Cov_L[index], RPcountry$Cov_L[i])
  RPcountry$Cov_U[i] = ifelse(is.na(RPcountry$Cov_U[i]), RPcluster$Cov_U[index], RPcountry$Cov_U[i])
  RPcountry$mRP[i] = ifelse(is.na(RPcountry$mRP[i]), RPcluster$mRP[index], RPcountry$mRP[i])
  RPcountry$lRP[i] = ifelse(is.na(RPcountry$lRP[i]), RPcluster$lRP[index], RPcountry$lRP[i])
  RPcountry$uRP[i] = ifelse(is.na(RPcountry$uRP[i]), RPcluster$uRP[index], RPcountry$uRP[i])
}
head(RPcountry)

# DATA:
# SOCIOECONOMIC: HDI, population, country codes and clusters
HDI = read.csv("HDI.csv")  # http://www.imf.org/external/ns/cs.aspx?id=28 ** WITH WIKI EXTRAPOLATIONS **
pop = read.csv("CountriesPop.csv")  # http://esa.un.org/wpp (pop$X2010)
cluster = read.csv("Clusters.csv")  # Determined using Delphi method during PRP meeting
pop$cluster = cluster$CLUSTER[match(cluster$NUM, pop$NUM)]
pop = merge(HDI, pop, by = c("CODE", "CODE"))

# EPIDEMIOLOGY: incidence, vaccination coverage
inc = read.csv("Epi.csv")  # Literature & questionnaire data
incidence = subset(inc, inc$PP == "Y")  # Data for calculating PP
incidence <- within(incidence, {
  HDI <- pop$hdi[match(corrected_Country, pop$COUNTRY)]  # uses consolidated HDI
  pop <- pop$X2010[match(corrected_Country, pop$COUNTRY)]  # Uses UN population data
  Cov <- RPcountry$Coverage[match(Country, RPcountry$Country)]  # DOG RABIES INCIDENCE - RP
  RP <- RPcountry$mRP[match(Country, RPcountry$Country)]
})
pop$hdi <- replace(pop$hdi, which(is.na(pop$hdi)), pop$hdi[which(pop$COUNTRY=="New Zealand")])

        
# 2) PROBABILITY OF PEP - PP
source("FitPP.R")

source("RabiesBurdenFig2.R")  # Creates Figure 2
pp_order = match(RPcountry$HDI, PP$HDI)
RPcountry$PP = PP$tprop[pp_order]
RPcountry$PPl = PP$lwr[pp_order]
RPcountry$PPu = PP$upr[pp_order]
# test=PPrange[match(PP$HDI, RPcountry$HDI),]

for (i in 1:nrow(RPcluster)){
	index = which(RPcountry$Cluster == RPcluster$Cluster[i])
  RPcluster$PP[i] = mean(RPcountry$PP[index], na.rm=TRUE)
}



# 3) BITE INCIDENCE - from literature & questionnaires
bites = subset(inc, inc$Incidence == "Y")
for(i in 1:nrow(RPcountry)){
	index = which(as.character(bites$Country) == as.character(RPcountry$Country[i]))	
	RPcountry$BI[i] = ifelse(length(index) > 0, bites$Bite.rate[index], NA)	
}

for (i in 1:nrow(RPcluster)){ # Generate cluster bite incidence and CIs
	index = which(RPcountry$Cluster == RPcluster$Cluster[i])
	RPcluster$BI[i] = ifelse(sum(!is.na(RPcountry$BI[index]))==0, 0, mean(RPcountry$BI[index], na.rm = TRUE))
	RPcluster$BI_L[i] = ifelse(sum(!is.na(RPcountry$BI[index]))==0, 0, min(RPcountry$BI[index], na.rm = TRUE))
	RPcluster$BI_U[i] = ifelse(sum(!is.na(RPcountry$BI[index]))==0, 0, max(RPcountry$BI[index], na.rm = TRUE))	
	RPcluster$pop[i] = mean(RPcountry$pop[index], na.rm = TRUE)
}

# Very little research on bite incidence from India - reported incidence gives unrealistically high deaths (Delphi) 
# Use average bite incidence across Asia (reasonably consistent)
RPcountry$BI[which(RPcountry$CODE == "IND")] = mean(RPcluster$BI[2:7], na.rm = TRUE)

# Bite incidence for W Africa not believable for all countries (Delphi) - replace with continent average
RPcluster$BI[which(RPcluster$Cluster == "Francophone")] = mean(RPcluster$BI[15:18], na.rm = TRUE)

# For countries that are clusters (India, Indonesia, China, Brazil) or have v little data (Congo, W Africa)
# use ranges across clusters for CIs (Delphi)
RPcluster$BI_L[which(RPcluster$Cluster == "India")] = min(RPcluster$BI[2:7], na.rm = TRUE)
RPcluster$BI_U[which(RPcluster$Cluster == "India")] = max(RPcluster$BI[2:7], na.rm = TRUE)

RPcluster$BI_L[which(RPcluster$Cluster == "Indonesia")] = min(RPcluster$BI[2:7], na.rm = TRUE)
RPcluster$BI_U[which(RPcluster$Cluster == "Indonesia")] = max(RPcluster$BI[2:7], na.rm = TRUE)

RPcluster$BI_L[which(RPcluster$Cluster == "China")] = min(RPcluster$BI[2:7], na.rm = TRUE)
RPcluster$BI_U[which(RPcluster$Cluster == "China")] = max(RPcluster$BI[2:7], na.rm = TRUE)

RPcluster$BI_L[which(RPcluster$Cluster == "Brazil")] = min(RPcluster$BI[10:14], na.rm = TRUE)
RPcluster$BI_U[which(RPcluster$Cluster == "Brazil")] = max(RPcluster$BI[10:14], na.rm = TRUE)

RPcluster$BI_L[which(RPcluster$Cluster == "Congo")] = min(RPcluster$BI[15:18], na.rm = TRUE)
RPcluster$BI_U[which(RPcluster$Cluster == "Congo")] = max(RPcluster$BI[15:18], na.rm = TRUE)

RPcluster$BI_L[which(RPcluster$Cluster == "Francophone")] = min(RPcluster$BI[15:18], na.rm = TRUE)
RPcluster$BI_U[which(RPcluster$Cluster == "Francophone")] = max(RPcluster$BI[15:18], na.rm = TRUE)

for(i in 1:nrow(RPcountry)){ # Generate CIs for every country...
  index = which(RPcluster$Cluster == RPcountry$Cluster[i])	
  RPcountry$BI[i] = ifelse(is.na(RPcountry$BI[i]), RPcluster$BI[index], RPcountry$BI[i])
  RPcountry$BI_L[i] = RPcluster$BI_L[index]
  RPcountry$BI_U[i] = RPcluster$BI_U[index]
}
RPcountry$BI_L[c(34,105)] <- RPcountry$BI[c(34,105)]*0.9 # Chad and Mali are special case

# 4) LIVESTOCK LOSSES
L = read.csv("FAO_Livestock_GLiPHA.csv")  # Downloaded Sept 2011

# Calculate Livestock Units (LU) - shoats ~1/7 cattle
# Chilonda & Otte, Livestock Research for Rural Development, 18 (8), 2006, Article #117
LU = apply(cbind(L$Cattle, cbind(L$Sheep, L$Goat)/7), 1, na.rm = TRUE, sum)
LU_avg_price=mean(L$cost.per.head.of.cattle, na.rm = TRUE)

RPcountry$LL = meanFun(Lfit$Imax, Lfit$Iresp, RPcountry$Coverage) * LU * L$Endemic 
RPcountry$LLpc = RPcountry$LL/L$Human.pop  # Livestock losses per capita
RPcountry$LLpcU = meanFun(Llci$Imax, Llci$Iresp, RPcountry$Coverage) * LU * L$Endemic/ L$Human.pop 
RPcountry$LLpcL = meanFun(Luci$Imax, Luci$Iresp, RPcountry$Coverage) * LU * L$Endemic/ L$Human.pop 

for (i in 1:nrow(RPcountry)){ # Use country specific or avg LU prices as available
  index = which(RPcountry$Cluster == RPcountry$Cluster[i])
  RPcountry$L.cost.per[i] = ifelse(sum(!is.na(L$cost.per.head.of.cattle[index])) > 0, 
                                   mean(L$cost.per.head.of.cattle[index], na.rm = TRUE),
                                   LU_avg_price)  
}


# 5) PEP COSTS
price <- read.csv("BiologicalsPrices.csv") # Market data
price$Cluster = RPcountry$Cluster[match(price$CODE, RPcountry$CODE)]
price$Cluster[which(price$Country == "SADEC")] <- "SADC"

item_cost = function(code, cluster, item, type){
  # Calculates PEP costs by country (or cluster)
  # Take country identifier and cluster, price data and biological of interest (vaccine or RIG)
  # Returns cost of PEP
  
  cost_country = item[which(price$CODE == as.character(code))]
  cost_country <- ifelse(length(cost_country == 1), cost_country, NA)
  cost_cluster = mean(item[which(price$Cluster == as.character(cluster))], na.rm = TRUE)
  cost_cluster <- ifelse(is.na(cost_cluster), NA, cost_cluster) 
  cost_cluster <- ifelse(length(cost_cluster == 1), cost_cluster, NA) 
  
  if (type == "vaccine"){ cost_other = item[67] }  # ASSIGN UNICEF VALUES (applies primarily to Caribbean)
  if (type == "RIG"){ cost_other = item[which(price$CODE == "OTH")] }
  
  cost = ifelse(is.na(cost_country), cost_cluster, cost_country)
  cost = ifelse(is.na(cost), cost_other, cost)
  return(cost)
}

# Assign unit costs of NTV, CCV and RIG 
RPcountry$NTVcost = rep(price$NTV[which(!is.na(price$NTV))], nrow(RPcountry))
for(i in 1:nrow(RPcountry)){
  RPcountry$HRIGcost[i] = item_cost(RPcountry$CODE[i], RPcountry$Cluster[i], price$HRIG, "RIG")
  RPcountry$ERIGcost[i] = item_cost(RPcountry$CODE[i], RPcountry$Cluster[i], price$ERIG, "RIG")
  RPcountry$TCVcost[i] = item_cost(RPcountry$CODE[i], RPcountry$Cluster[i], price$TCV, "vaccine")    
}

# INPUT COSTS - Work out administration of PEP for each country
c_country <- read.csv("cost_input_Country.csv")  # Questionnaire data
c_cluster <- read.csv("cost_input_Cluster.csv")  # Delphi-derived data for clusters

c_fill = function(variable, cluster_variable, country_cluster, cluster){
  # Function to assign cluster values to countries
  c_index = match(country_cluster, cluster)
  missing = which(is.na(variable))
  new_variable = replace(variable, missing, cluster_variable[c_index][missing])
  return(new_variable)
}

# Generate country level inputs for use of HRIG, ERIG, NTVs & CCVs (% use, doses travels & travel costs in rural & urban areas)
c_country$HRIG = c_fill(c_country$HRIG, c_cluster$HRIG, c_country$Cluster, c_cluster$Cluster)
c_country$ERIG = c_fill(c_country$ERIG, c_cluster$ERIG, c_country$Cluster, c_cluster$Cluster)
c_country$NTV = c_fill(c_country$NTV, c_cluster$NTV, c_country$Cluster, c_cluster$Cluster)
c_country$ND_NTV = c_fill(c_country$ND_NTV, c_cluster$ND_NTV, c_country$Cluster, c_cluster$Cluster)
c_country$TCV_IM = c_fill(c_country$TCV_IM, c_cluster$TCV_IM, c_country$Cluster, c_cluster$Cluster) 
c_country$ND_IM = c_fill(c_country$ND_IM, c_cluster$ND_IM, c_country$Cluster, c_cluster$Cluster) 
c_country$TCV_ID = c_fill(c_country$TCV_ID, c_cluster$TCV_ID, c_country$Cluster, c_cluster$Cluster) 
c_country$ND_ID = c_fill(c_country$ND_ID, c_cluster$ND_ID, c_country$Cluster, c_cluster$Cluster) 
c_country$NTRAV = c_fill(c_country$NTRAV, c_cluster$NTRAV, c_country$Cluster, c_cluster$Cluster) 
c_country$TRAV_R = c_fill(c_country$TRAV_R, c_cluster$TRAV_R, c_country$Cluster, c_cluster$Cluster) 
c_country$TRAV_U = c_fill(c_country$TRAV_U, c_cluster$TRAV_U, c_country$Cluster, c_cluster$Cluster) 
c_country$DVAC = c_fill(c_country$DVAC, c_cluster$DVAC, c_country$Cluster, c_cluster$Cluster) 
c_country$DVAC_C = c_fill(c_country$DVAC_C, c_cluster$DVAC_C, c_country$Cluster, c_cluster$Cluster) 
c_country$DSTE = c_fill(c_country$DSTE, c_cluster$DSTE, c_country$Cluster, c_cluster$Cluster) 
c_country$DSTE_C = c_fill(c_country$DSTE, c_cluster$DSTE, c_country$Cluster, c_cluster$Cluster) 
c_country$DKIL = c_fill(c_country$DKIL, c_cluster$DKIL, c_country$Cluster, c_cluster$Cluster) 
c_country$DKIL_C = c_fill(c_country$DKIL_C, c_cluster$DKIL_C, c_country$Cluster, c_cluster$Cluster) 
c_country$CAT_C = c_fill(c_country$CAT_C, c_cluster$CAT_C, c_country$Cluster, c_cluster$Cluster) 
c_country$TEST = c_fill(c_country$TEST, c_cluster$TEST, c_country$Cluster, c_cluster$Cluster) 
c_country$TEST_C = c_fill(c_country$TEST_C, c_cluster$TEST_C, c_country$Cluster, c_cluster$Cluster) 


########################################################################
# WRITE BURDEN INPUT FILES:
write.csv(cbind(RPcountry[order(RPcountry$NUM),], c_country[,c(2,4,6:40)]), "pPEPcountry.csv", row.names=FALSE)
write.csv(RPcluster[order(RPcluster$NUM),], "pPEPcluster.csv", row.names=FALSE)
write.csv(PPrange[order(RPcountry$NUM),], "PPrange.csv", row.names=FALSE)







