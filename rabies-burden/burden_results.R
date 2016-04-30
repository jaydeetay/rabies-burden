#Present results from output of burden model
require(plyr) # for ddply
require(RColorBrewer)

burden=read.csv("burden.csv")
burden_cluster <- ddply(burden, .(Cluster), summarise, 
                        pop=sum(Human_population_2010),
                        deaths=sum(deaths), deaths_lci=sum(deaths_lci), deaths_uci=sum(deaths_uci),
                        exposures=sum(exp), exp_lci=sum(exp_lci), exp_uci=sum(exp_uci),
                        PEP=sum(PEP), PEP_lci=sum(PEP_lci), PEP_uci=sum(PEP_uci),
                        prev_death=sum(prev_death), prev_death_lci=sum(prev_death_lci), prev_death_uci=sum(prev_death_uci),
                        
                        YLL=sum(YLLle), DALYdeath_lci=sum(DALYdeath_lci), DALYdeath_uci=sum(DALYdeath_uci),
                        NTV=sum(NTVgbd, na.rm=TRUE), DALYntv_lci=sum(DALYntv_lci, na.rm=TRUE), DALYntv_uci=sum(DALYntv_uci, na.rm=TRUE),
                        Anxiety=sum(Anxiety), # Not in table 2
                        DALY=sum(DALYgbd),
                        
                        Cov=mean(Cov, na.rm=TRUE),
                        RP=mean(mRP, na.rm=TRUE),
                        PP=mean(PP, na.rm=TRUE),
                        
                        DC=sum(direct), DClci=sum(DClci), DCuci=sum(DCuci),
                        travel=sum(travel), travel_lci=sum(TClci), travel_uci=sum(TCuci),
                        LI=sum(lost_income), LIlci=sum(LIlci), LIuci=sum(LIuci),
                        PD=sum(prem_death), PDlci=sum(PDlci), PDuci=sum(PDuci),
                        dog_vacc=sum(dog_vacc),
                        dpm=sum(dpm),
                        livestock=sum(Livestock), LLci=sum(LLl), LLuci=sum(LLu),
                        surveillance=sum(surveillance)) # Consolidate burden output by cluster
burden_cluster$DALYlci = with(burden_cluster, DALYdeath_lci + DALYntv_lci)
burden_cluster$DALYuci = with(burden_cluster, DALYdeath_uci + DALYntv_uci)
write.csv(burden_cluster, "bc.csv", row.names=FALSE)

#Breakdown by continent
africa=c(grep("NorthAfrica", burden$Cluster), 
         grep("Congo", burden$Cluster),
         grep("Francophone", burden$Cluster),
         grep("SADC", burden$Cluster))
asia=c(grep("Asia_2", burden$Cluster), 
       grep("Asia_3", burden$Cluster),
       grep("Asia_4", burden$Cluster),
       grep("India", burden$Cluster),
       grep("China", burden$Cluster),
       grep("Indonesia", burden$Cluster))
america=c(grep("Carribbean", burden$Cluster), 
          grep("SouthernCone", burden$Cluster),
          grep("CentralAmericaMexico", burden$Cluster),
          grep("Andean", burden$Cluster),
          grep("Brazil", burden$Cluster))
other=c(grep("EasternEurope", burden$Cluster), 
          grep("MiddleEast", burden$Cluster),
          grep("Eurasia", burden$Cluster))

regions=rep("RABIES-FREE", length(burden$Cluster))
regions[africa]="AFRICA"
regions[asia]="ASIA"
regions[america]="AMERICAS"
regions[other]="OTHER"
burden$regions=regions

burden_regions <- ddply(burden, .(regions), summarise, 
                        pop=sum(as.numeric(Human_population_2010)),
                        deaths=sum(deaths),
                        dLCI=sum(deaths_lci),
                        dUCI=sum(deaths_uci),
                        exposures=sum(exp),
                        PEP=sum(PEP),
                        prev_death=sum(prev_death),
                        YLL=sum(YLL),
                        NTV=sum(NTVgbd),
                        Anxiety=sum(Anxiety),
                        DALY=sum(DALYgbd),
                        DC=sum(direct),
                        travel=sum(travel),
                        LI=sum(lost_income),
                        PD=sum(prem_death, na.rm=TRUE),
                        dog_vacc=sum(dog_vacc),
                        dpm=sum(dpm),
                        livestock=sum(Livestock, na.rm=TRUE),
                        surveillance=sum(surveillance)) # Consolidate burden output by regions

# RESULTS:
# DEATHS
total=sum(burden$deaths, na.rm=TRUE); total
cbind(burden_regions$region, burden_regions$deaths, burden_regions$deaths*100/total, 
      burden_regions$dLCI, burden_regions$dUCI)
haiti=burden$deaths[which(as.character(burden$Country)=="Haiti")]
haiti/burden_regions$deaths[which(burden_regions$region=="AMERICAS")]
d$Country[which(d$DR*100000>5)]
d$Country[order(d$deaths, decreasing=TRUE)][1:10]
max(d$deaths, na.rm=TRUE)
max(d$deaths, na.rm=TRUE)/total
burden$deaths[which(as.character(burden$Country)=="China")]/total
burden$deaths[which(as.character(burden$Country)=="India")]/total

# DALYs
sum(d$DALYgbd)
sum(d$YLLle, na.rm=TRUE)/sum(d$DALYgbd)
sum(d$YLLle, na.rm=TRUE)
sum(d$NTVgbd, na.rm=TRUE)
sum(d$NTVgbd, na.rm=TRUE)/sum(d$DALYgbd)
cbind(burden_regions$region, burden_regions$DALY, burden_regions$DALY*100/sum(d$DALYgbd))
sum(d$Anxiety, na.rm=TRUE)
sum(d$Anxiety, na.rm=TRUE)/sum(d$DALYgbd)

# Costs
costs=burden_regions[c(3,1,2,4), c("PD", "DC", "travel", "LI", "dog_vacc", "dpm", "surveillance", "livestock")]
apply(costs, 2, sum)*100/sum(costs)
apply(costs, 2, sum)
societal = sum(apply(costs, 2, sum)[c(1,3,4)]); societal
societal/sum(costs)
apply(costs, 2, sum)[2]/sum(costs)
sum(apply(costs, 2, sum)[c(5,6,8)])/sum(costs)
apply(costs, 2, sum)[7]/sum(costs)
sum(apply(costs, 2, sum)[c(2,3)])

rnames=burden_regions$region[c(3,1,2,4)]
cost_class=c("Premature death","Direct costs","Travel costs","Lost income","Dog vaccination","Dog population management", "Surveillance", "Livestock losses")
Ccol <- c(brewer.pal(8, "RdYlBu")[c(1:4, 8,7,6)], brewer.pal(8, "BrBG")[1])
regionC=apply(costs, 1, sum)
sum(regionC)

# Figure 5
postscript(file="burden_breakdown.eps", width=5, height=4, horizontal=F)
par(mfrow=c(1,1), cex=0.85, lwd=0.1, mar=c(0,0,0,0), plt=c(0.15, 0.95, 0.1, 0.8))
barplot(t(costs)/1000000, names.arg=rnames, col=Ccol, border=NA, ylab="Rabies burden in USD (millions)")
legend(3, 2200, cost_class, cex=0.7, bty="n", fill=Ccol, border=NA) 
par(new=T, plt=c(0.45, 0.9, 0.5, .9), lwd=0.1, cex=0.55, mgp=c(1, 0.2, 0))
mp=barplot(t((costs)/regionC), names.arg=rnames, border=NA, ylab="% of burden", col=Ccol) #legend=cost_class, 
dev.off()

# Supplementary Figure
Ccosts=burden_cluster[c(15, 9, 16, 5, 4, 3, 10, 14, 21, 18, 7, 1, 8, 6, 22, 13, 17, 11), 
                      c("PD", "DC", "travel", "LI", "dog_vacc", "dpm", "surveillance", "livestock")]
clusters=burden_cluster$Cluster[c("India", "China", "Indonesia", "Asia_4", "Asia_3", "Asia_2",
                                  "Congo","Francophone","SADC","NorthAfrica",
                                  "Caribbean","Andean","CentralAmericaMexico","Brazil", "SouthernCone", 
                                  "Eurasia", "MiddleEast", "EasternEurope")]
cnames=c("India", "China", "Indonesia", "Asia 4", "Asia 3", "Asia 2",
         "Congo basin","W Africa","SADC","North Africa",
         "Caribbean","Andean","Central America","Brazil", "Southern Cone", 
         "Eurasia", "Middle East", "Eastern Europe")

cost_class=c("Premature death","Direct costs","Travel costs","Lost income","Dog vaccination","Dog population management", "Surveillance", "Livestock losses")
Ccol <- c(brewer.pal(8, "RdYlBu")[c(1:4, 8,7,6)], brewer.pal(8, "BrBG")[1])
clusterC=apply(Ccosts, 1, sum)

postscript("SuppFig1.eps", width=8, height=4)
par(cex=0.85, mgp=c(1.5,.5,0), lwd=0.4, plt=c(0.15, 0.9, 0.3, 0.98), tck=-0.02)
mp=barplot(t(Ccosts)/1000000, names.arg=rep("", length(cnames)), col=Ccol, border=NA, ylab="Costs in USD (millions)") #legend=cost_class, 
text(mp+.5, par("usr")[2]-25, labels = cnames, srt = 90, pos = 2, xpd = TRUE, cex=0.75)
legend(17, 1.4e+9/1000000, cost_class, cex=0.7, bty="n", fill=Ccol, border=NA) 

par(new=TRUE, cex=0.7, lwd=0.4, plt=c(0.35, 0.7, 0.65, 0.95), tck=-0.03, mgp=c(1.3,0.3,0)) #Breakdown as %
mp=barplot(t((Ccosts)/clusterC), names.arg=rep("", length(cnames)), col=Ccol, border=NA, ylab="Proportion of burden") #legend=cost_class, 
#text(mp+1, par("usr")[2]-31.2, labels = clusters[index], srt = 90, pos = 2, xpd = TRUE, cex=0.5)
text(mp+1, rep(0, length(cnames)), labels = cnames, srt = 90, pos = 2, xpd = TRUE, cex=0.7)
dev.off()

# Per capita expenditure on dog vaccination
burden_regions$dog_vacc/burden_regions$pop
costs/regionC

# Additional details
E=sum(burden$exp, na.rm=TRUE); E
PEP=sum(burden$PEP, na.rm=TRUE); PEP
Drate=burden_cluster$deaths*100000/burden_cluster$pop




