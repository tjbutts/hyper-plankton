# DOY 150 # 
# Plot assessing Biomass/AvgLength v. avgBiomass/Length 
# 2 x 2 plots 
# Average Length v. average derived biomass ===========================
# Daphnia Equation
daphL = c(548.1,1155.4,1265,	1179.8,	621.4	,1142,	1241.5,	1093.1,	686.7	,567.5,	793,	617.9,	1047.2,	551,	662.8,	652.6,	943,
      898.9,	1059.4,	916.5,	744.4,	615.8,	526	,964.4,	748.9)
daphmeanL = mean(daphL)
daph_al_b = 3.127476
daph_ab_l = 3.7890964

# Bosmina Equation 
bosL = c(406.7, 216.9,265.2,311.3,246,308.7, 241,271.2, 297.2,270.6,287.7,264.1,300)
bosmeanL = mean(bosL)
bos_al_b = 0.210466547
bos_ab_l = 0.242648057


# Calanoida Equation 
calL = c(984.6,895.4, 1060.3,476.8,875,759.2,913.7,984.4,731.8,695.7, 794.6,783.9,1000.9)
calmeanL = mean(calL)
cal_al_b = 5.182699198
cal_ab_l = 5.440892499

# Keratella Equation 
kerL = c(109.2,104.5,96.1,103,110.6,102,110.1,95.8,97.9)
kermeanL = mean(kerL)
ker_al_b = 2.20105E-05
ker_ab_l = 2.21996E-05

# Combine vectors into dataframes 
DaphniaBiomass_vec <- as.numeric((1.5*10^-8)*(daphL^2.84)) # Vector of Daphnia Biomass 
BosminaBiomass_vec <- as.numeric(26.6*((bosL/1000)^3.13)) # Vector of Bosmina Biomass
CalnoidaBiomass_vec <- as.numeric((7.9*10^-7)*(calL^2.33)) # Vector of Calanoid Biomass
KeratellaBiomass_vec <- as.numeric((0.02)*((kerL/1000)^3)) # Vector of Keratella Biomass

daph <- data.frame(daphL, DaphniaBiomass_vec)
bos <- data.frame(bosL, BosminaBiomass_vec)
cal <- data.frame(calL, CalnoidaBiomass_vec)
ker <- data.frame(kerL, KeratellaBiomass_vec)

windows(height= 12, width = 16)
par(mfrow = c(2,2), mai=c(0.9,1,0.2,0.1))
plot(DaphniaBiomass_vec~daphL, pch=20, data=daph) # variations in length don't seem to be insanely disproportional
abline(h = 3.127476, col = 'mediumseagreen', lwd=3) # Average Length then take biomass 
abline(h = 3.7890964, col = 'dodgerblue3', lwd=3) # Derive biomass per length measurement then take average 
legend('topleft', legend = c('Average length -> biomass', 'Biomass per length -> average', 'Biomass per length measurement'), lty=c(1, 1, 3), col=c('mediumseagreen', 'dodgerblue3', 'black'), lwd=2)

plot(BosminaBiomass_vec~bosL, pch=20, data=bos)
abline(h = bos_al_b, col = 'mediumseagreen', lwd=3) # Average Length then take biomass 
abline(h = bos_ab_l, col = 'dodgerblue3', lwd=3) # Derive biomass per length measurement then take average 
legend('topleft', legend = c('Average length -> biomass', 'Biomass per length -> average', 'Biomass per length measurement'), lty=c(1, 1, 3), col=c('mediumseagreen', 'dodgerblue3', 'black'), lwd=2)

plot(CalnoidaBiomass_vec~calL, pch=20, data=bos)
abline(h = cal_al_b, col = 'mediumseagreen', lwd=3) # Average Length then take biomass 
abline(h = cal_ab_l, col = 'dodgerblue3', lwd=3) # Derive biomass per length measurement then take average 
legend('topleft', legend = c('Average length -> biomass', 'Biomass per length -> average', 'Biomass per length measurement'), lty=c(1, 1, 3), col=c('mediumseagreen', 'dodgerblue3', 'black'), lwd=2)

plot(KeratellaBiomass_vec~kerL, pch=20, data=bos)
abline(h = ker_al_b, col = 'mediumseagreen', lwd=3) # Average Length then take biomass 
abline(h = ker_ab_l, col = 'dodgerblue3', lwd=3) # Derive biomass per length measurement then take average 
legend('topleft', legend = c('Average length -> biomass', 'Biomass per length -> average', 'Biomass per length measurement'), lty=c(1, 1, 3), col=c('mediumseagreen', 'dodgerblue3', 'black'), lwd=2)
