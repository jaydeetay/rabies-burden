# Creates Figure 2: fitted relationships - run within burden_1.R

postscript("Fig2.eps", width=8, height=4)

# Main panel with coverage vs incidence data, fit & CIs
o <- order(LAC$cov)

par(cex = 0.85, mgp = c(1.5,.5,0), lwd = 0.4, plt = c(0.1, 0.5, 0.15, 0.95), tck = -0.02)
plot(NA, NA, xlab = "Vaccination coverage %", ylab = "Rabies incidence", 
     pch = 20, ylim = c(0, 0.004), xlim = c(0,100), axes = FALSE, tck = -0.1)
axis(1); axis(2)

polygon(c(LAC$cov[o], rev(LAC$cov[o])),
        c(meanFun(LAClci$Imax, LAClci$Iresp, LAC$cov/ 100)[o], 
          rev(meanFun(LACuci$Imax, LACuci$Iresp, LAC$cov/ 100)[o])),
        col = grey(0.8), border = NULL, density = 100)

GBA = which(LAC$site == "GBA"); points(LAC$cov[GBA], LAC$inc[GBA], pch = 20)
SP = which(LAC$site == "SP"); points(LAC$cov[SP], LAC$inc[SP], pch = 20, col = "blue")
LC = which(LAC$site == "LC"); points(LAC$cov[LC], LAC$inc[LC], pch = 20, col = "red")
Mex = which(LAC$site == "Mexico"); points(LAC$cov[Mex], LAC$inc[Mex], pch = 20, col = "dark green")
lines(LAC$cov[o], meanFun(LACfit$Imax, LACfit$Iresp, LAC$cov/ 100)[o], col = grey(0.5), lwd = 2)
legend(-3, 0.004, legend = c("Mexico","Greater Buenos Aires", "Lima & Callao", "Sao Paulo"), 
       col = c("dark green","black","red","blue"), pch = 20, bty = "n", cex = 0.85)
mtext(side = 1, line = -16, at = -15, text = "A", cex = 1.5, font = 2)


#INSET TO SHOW OUTLIER ON LOG PLOT
par(new = TRUE, cex = 0.7, lwd = 0.4, plt = c(0.33, 0.48, 0.65, 0.85), tck = -0.03, mgp = c(1.3,0.3,0))
plot(LAC$cov, log(LAC$inc), 
     xlab = "", ylab = "log(incidence)", 
     pch = 20, xlim = c(0,100), axes = FALSE, cex = .6)
axis(1); axis(2)
polygon(c(LAC$cov[o], rev(LAC$cov[o])),
        log(c(meanFun(LAClci$Imax, LAClci$Iresp, LAC$cov/ 100)[o], 
          rev(meanFun(LACuci$Imax, LACuci$Iresp, LAC$cov/ 100)[o]))),
        col = grey(0.8), border = NULL,
        density = 100)
lines(LAC$cov[o], log(meanFun(LACfit$Imax, LACfit$Iresp, LAC$cov/ 100)[o]), col = grey(0.5), lwd = 1.5)


#INSET TO SHOW RELATIONSHIP WITH LIVESTOCK
par(new = TRUE, cex = 0.7, lwd = 0.4, plt = c(0.33, 0.48, 0.4, 0.6), tck = -0.03, mgp=c(1.3,0.3,0))
plot(NA, NA,
     xlab = "", ylab = "Livestock \n incidence", 
     pch = 20, xlim = c(0,100), ylim = c(0, 7e-04), axes = FALSE)
axis(1); axis(2)
polygon(c(0:100, 100:0),
        c(meanFun(Luci$Imax, Luci$Iresp, (0:100)/ 100), 
          rev(meanFun(Llci$Imax, Llci$Iresp, (0:100)/ 100))),
        col = grey(0.8), border = NULL,
        density = 100)
lines(0:100, meanFun(Lfit$Imax, Lfit$Iresp, (0:100)/ 100),  col = grey(0.5), lwd = 1.5)
points(livestock$vac * 100, livestock$inc, xlim = c(0, 100), pch = 20, cex = .6)


#Probability of PEP, PP
par(new = TRUE, cex = 0.85, mgp = c(1.5,.5,0), lwd = 0.4, plt = c(0.55, 0.85, 0.15, 0.9), tck = -0.02)
plot(NA, NA, ylab = "Probability of receiving PEP", xlab = "Human Development Index", 
     xlim = c(0.25,0.85), ylim = c(0.3,1), axes = FALSE)
axis(1); axis(2)
polygon(c(pp$HDI, rev(pp$HDI)), c(pp$lwr, rev(pp$upr)),
        col = grey(0.8), border = NULL, density = 100)
lines(pp$HDI, pp$tprop, lwd = 2,  col = grey(0.5))
points(incidence$HDI, incidence$tprop, pch = 20, cex = log10(incidence$tot)/3)
mtext(side = 1, line = -16, at = 0.13, text = "B", cex = 1.5, font = 2)
dev.off()

