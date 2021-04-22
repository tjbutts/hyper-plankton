# Excretion rate Plots # 
# Run GV_Excretion Rates.R first # 
# load tidyverse 
library(tidyverse)

# data sets for plotting  
mydat = as.data.frame(gv19_excretion_rate_dailymean2) # uM/L N and P excretion grouped per day with sd of uM/L based on taxa grouping 

# Contribution of N w/ inorganic N plotted together 
#Line 
# '#004ebe','#2a81ff', '#94c0ff'
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(mydat$doy, mydat$meandaily_N_umol_L, type = 'o', lwd =3, 
     pch = 19, col = '#2a81ff', ylim = c(0,10),
     ylab = 'Zoop N excretion (umol/L_d_indv)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)

#Line w/ inorganic N 
# read in code from GV_stock analysis to get gv_uML_stocks data object
gv_uML_stocks

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, gv_uML_stocks$inorgN_uML, type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', 
     ylab = 'Nitrogen concentration (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
points(mydat$doy, mydat$meandaily_N_umol_L, type='o', lwd=3, col = '#2a81ff')

#log scale
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log(gv_uML_stocks$inorgN_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', yaxt='n', ylim=c(log(1), log(200)),
     xlab = 'Day of Year, 2019', ylab ='', cex.lab = 1.5, cex.axis = 1.5)
par(new=T)
plot(mydat$doy, log(mydat$meandaily_N_umol_L+1), type='o', lwd=3, col = '#2a81ff', pch=19, yaxt='n', ylab ='', xlab = '',
     ylim=c(log(1), log(200)), 
     cex.lab = 1.5, cex.axis=1.5, xaxt='n')
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', ''), las=2, cex.axis=1.5)
axis(side=4,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', ''), las=2, cex.axis=1.5)
mtext('Inorganic N concentration (umol/L)', side=2, line =3, cex = 1.5 )
mtext('Zoop N excretion per d per indv (umol/L)', side=4, line =3, cex = 1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Inorganic N', 'ZP excreted N'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#004ebe','#2a81ff'))


# '#c24ad7', '#d786e4'
#Line w/ inorganic P 
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(mydat$doy, mydat$meandaily_P_umol_L, type = 'o', lwd =3, 
     pch = 19, col = '#d786e4', ylim = c(0,5),
     ylab = 'Zoop P excretion (umol/L_d_indv)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)

gv_uML_stocks

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, gv_uML_stocks$inorgP_uML, type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', 
     ylab = 'Inorganic P concentration (umol/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5,ylim = c(0,8))
par(new = T)
plot(mydat$doy, mydat$meandaily_P_umol_L, type='o', lwd=3, col = '#d786e4', ylim = c(0,8),
     pch=19, ylab='',xlab='', xaxt ='n', yaxt='n')
axis(side = 4, at=c(0,2,4,6,8), labels = c('0', '2','4','6','8'), cex.axis=1.5, las=2)
mtext('Zoop P excretion per d per indv (umol/L)', side=4, line=3, cex=1.5)

#log scale
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log(gv_uML_stocks$inorgP_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', yaxt='n', ylim=c(log(1), log(10)),
     ylab = 'Phosphorus concentration (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
points(mydat$doy, log(mydat$meandaily_N_umol_L+1), type='o', lwd=3, col = '#d786e4')
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10'), las=2, cex.axis=1.5)
arrows(mydat$doy, log(mydat$meandaily_N_umol_L+1), mydat$doy, log((mydat$meandaily_N_umol_L+mydat$N_se)+1), length=0.05, angle=90)
arrows(mydat$doy, log(mydat$meandaily_N_umol_L+1), mydat$doy, log((mydat$meandaily_N_umol_L-mydat$N_se)+1), length=0.05, angle=90)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Inorganic P', 'ZP excreted P'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7','#d786e4'))

# Using alternate calculation method ===============================
# load tidyverse 
library(tidyverse)

# data sets for plotting 
gv19_excretion_rate_uML  #uM/L N and P excretion grouped by taxa type per day 
mydat = as.data.frame(gv19_excretion_rate_dailymean2) # uM/L N and P excretion grouped per day with sd of uM/L based on taxa grouping 

# Contribution of N w/ inorganic N plotted together 
#Line 
# '#004ebe','#2a81ff', '#94c0ff'
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(mydat$doy, mydat$meandaily_N_umol_L, type = 'o', lwd =3, 
     pch = 19, col = '#2a81ff', ylim = c(0,10),
     ylab = 'Zooplankton daily N excretion (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)

#Line w/ inorganic N 
# read in code from GV_stock analysis to get gv_uML_stocks data object
gv_uML_stocks

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, gv_uML_stocks$inorgN_uML, type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', 
     ylab = 'Nitrogen concentration (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
points(mydat$doy, mydat$meandaily_N_umol_L, type='o', lwd=3, col = '#2a81ff')

#log scale
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log(gv_uML_stocks$inorgN_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', yaxt='n', ylim=c(log(1), log(200)),
     ylab = 'Nitrogen concentration (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
points(mydat$doy, log(mydat$meandaily_N_umol_L+1), type='o', lwd=3, col = '#2a81ff')
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', ''), las=2, cex.axis=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Inorganic N', 'ZP excreted N'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#004ebe','#2a81ff'))

#Stacked 
# '#c24ad7', '#d786e4'
#Line w/ inorganic P 
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(mydat$doy, mydat$meandaily_P_umol_L, type = 'o', lwd =3, 
     pch = 19, col = '#d786e4', ylim = c(0,5),
     ylab = 'Zooplankton daily P excretion (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
arrows(mydat$doy, mydat$meandaily_P_umol_L, mydat$doy, (mydat$meandaily_P_umol_L+mydat$P_se), length=0.05, angle=90)
arrows(mydat$doy, mydat$meandaily_P_umol_L, mydat$doy, (mydat$meandaily_P_umol_L-mydat$P_se), length=0.05, angle=90)
# read in code from GV_stock analysis to get gv_uML_stocks data object

gv_uML_stocks

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, gv_uML_stocks$inorgP_uML, type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', 
     ylab = 'Phosphorus concentration (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
points(mydat$doy, mydat$meandaily_P_umol_L, type='o', lwd=3, col = '#d786e4')
arrows(mydat$doy, mydat$meandaily_P_umol_L, mydat$doy, (mydat$meandaily_P_umol_L+mydat$P_se), length=0.05, angle=90)
arrows(mydat$doy, mydat$meandaily_P_umol_L, mydat$doy, (mydat$meandaily_P_umol_L-mydat$P_se), length=0.05, angle=90)

#log scale
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log(gv_uML_stocks$inorgP_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', yaxt='n', ylim=c(log(1), log(10)),
     ylab = 'Phosphorus concentration (uM/L)', xlab = 'Day of Year, 2019', cex.lab = 1.5, cex.axis = 1.5)
points(mydat$doy, log(mydat$meandaily_N_umol_L+1), type='o', lwd=3, col = '#d786e4')
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10'), las=2, cex.axis=1.5)
arrows(mydat$doy, log(mydat$meandaily_N_umol_L+1), mydat$doy, log((mydat$meandaily_N_umol_L+mydat$N_se)+1), length=0.05, angle=90)
arrows(mydat$doy, log(mydat$meandaily_N_umol_L+1), mydat$doy, log((mydat$meandaily_N_umol_L-mydat$N_se)+1), length=0.05, angle=90)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Inorganic N', 'ZP excreted P'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7','#d786e4'))
