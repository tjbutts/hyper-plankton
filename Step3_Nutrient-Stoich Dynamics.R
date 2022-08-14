## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts October 2021

#================================================================##
# STEP 3: VISUALIZE NUTREINT DYNAMICS AND ORGANISM STOICHIOMETRY
#================================================================##
## NOTE: Be sure to run Steps 1-2 first ## 

# Required Datasets 
gv_nutrients # Surface water concentrations of total and dissolved N and P
zp_stoich # Zooplankton stoichiometry measurements 
zp_raw # Zooplankton Biomass data 

# Calculate Green Valley Lake Organic and Inorganic Stocks # 
#Subset to the study year, site, and surface depth 
gvl19 = gv_nutrients %>%
  filter(sampleDepth == 0.25) %>%
  select(doy, SRP_ugL, TP_ugL, TN_mgL, NOx_mgL) %>%
  filter(!(doy == 162)) # DOY Removed to match with zooplankton data 
gvl19

# Replace 0s in SRP and NOx estimate with detection limit/2 of AQ2 (3.9 and 0.03, respectively) 
gvl19$SRP_ugL<-replace(gvl19$SRP_ugL, gvl19$SRP_ugL<3.9, 3.9)
gvl19$SRP_ugL
gvl19$NOx_mgL<-replace(gvl19$NOx_mgL, gvl19$NOx_mgL<0.03, 0.03)
gvl19$NOx_mgL

# Assuming that TP = SRP + org P + inorg P sorbed to particles 
# Inorg P is not applicable at this site (assume zero)
# Calculate org P concentration by subtracting SRP from TP and convert to moles 
gvl19$orgP_ugL = gvl19$TP_ugL - gvl19$SRP_ugL
gvl19$orgP_mol = gvl19$orgP_ugL/(1000000*30.97)
gvl19$TP_mol = gvl19$TP_ugL/(1000000*30.97)
gvl19$SRP_mol = (gvl19$SRP_ugL+0.01)/(1000000*30.97)

#Assuming that TN = org N + NOx + NHx
#NHx is assumed to be zero (some data from ALM)
#Then subtract NOx from TN to get org N and convert to moles
gvl19$orgN_mgL = gvl19$TN_mgL - gvl19$NOx_mgL
gvl19$orgN_mol = gvl19$orgN_mgL/(1000*14.01)
gvl19$TN_mol = gvl19$TN_mgL/(1000*14.01)
gvl19$NOx_mol = (gvl19$NOx_mgL+0.01)/(1000*14.01)

#Calculate the organic N to organic P ratio in moles
gvl19$org_NP = gvl19$orgN_mol/gvl19$orgP_mol
gvl19$total_NP = gvl19$TN_mol/gvl19$TP_mol
gvl19$inorg_NP = gvl19$NOx_mol/gvl19$SRP_mol

# Data for N:P ratio of nutrient stocks in Green Valley Lake # 
gvl19 # N:P ratios for Total, Organic, and Inorganic N:P 

# Calculate Zooplankton Body N:P Ratio over the growing season #==============================
# Additional Dataset 
zp_stoich # Zooplankton stoichiometry data
zp_raw # Zooplankton biomass data 

zp_cnp = zp_stoich %>% rename(perc_c = `%C`) %>% # Rename columns for ease 
  rename(perc_n = `%N`) %>%
  rename(perc_p = `%P`) %>%
  as_tibble()

zp_cnp = select(zp_cnp, taxon, group, perc_c, perc_n, perc_p) %>% as_tibble()
zp_cnp

zp_raw # Zooplankton biomass information 
zp_raw$group <- as.factor(zp_raw$group) # makes the group column a factor, easier for later analysis 

gv19_zp = zp_raw %>% select(sampleid,doy, taxon,group,biomass)
as_tibble(gv19_zp)

# Combine biomass with cnp data 
zp_stoic <- left_join(gv19_zp, zp_cnp, by='taxon') # Join np with biomass data 
as_tibble(zp_stoic)
zp_stoic %<>% select(sampleid, doy, taxon, group.y, biomass, perc_c, perc_n, perc_p) %>% # Combine into one dataset 
  rename(group = group.y) %>% as_tibble()
as_tibble(zp_stoic)

# Multiply C, N, and P by biomass to get C, N, and P storage weighted by biomass by taxon
zp_stoic_molar <- zp_stoic %>% select(sampleid, doy, taxon, group, biomass, perc_c, perc_n, perc_p) %>% # Select appropriate columns 
  mutate(p_storage = (perc_p/100)*biomass) %>% #ug/L P in zooplankton
  mutate(n_storage = (perc_n/100)*biomass) %>% #ug/L N in zooplankton
  mutate(c_storage = (perc_c/100)*biomass) %>% #ug/L C in zooplankton 
  mutate(n_molar = n_storage/14010000) %>% # Get molar ratio (Moles of N in zooplankton)
  mutate(p_molar = p_storage/30970000) %>% # Get molar ratio (Moles of P in zooplankton)
  select(sampleid, doy, taxon, group, biomass, n_molar, p_molar) %>%
  as_tibble()
zp_stoic_molar

# Get Zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum <- zp_stoic_molar %>% 
  group_by(sampleid, doy) %>%
  summarise(
    nstorage_molar = sum(n_molar), # Sum of moles of N in zooplankton
    pstorage_molar = sum(p_molar)) %>% # Sum of moles of N in zooplankton 
  ungroup() %>%
  as_tibble()
zp_stoic_sum 

# Calculate community N:P  
zp_stoic_sum = mutate(zp_stoic_sum,zp_np = (nstorage_molar)/(pstorage_molar)) %>% arrange(doy)
zp_stoic_sum

# N and p stocks in uM #======================
zp_stoic
# Multiply N and P by biomass to get N and P storage weighted by biomass by taxon 
zp_stoic2 = zp_stoic %>%
  mutate(p_storage = (perc_p/100)*biomass, 
         n_storage = (perc_n/100)*biomass, 
         p_molar = (p_storage/30970000), 
         n_molar = (n_storage/14010000),
         n_uM = n_molar*1000000, # Get uM
         p_uM = p_molar*1000000) %>% 
  select(sampleid, doy, taxon, group, biomass, n_uM, p_uM) %>%
  as_tibble()
zp_stoic2

# Get daily micromoles of N and P per zooplankton taxonomic group 
zp_uM_sum = zp_stoic2 %>% 
  group_by(sampleid, doy) %>%
  summarise(
    n_uM_zp = sum(n_uM), 
    p_uM_zp = sum(p_uM)) %>%
  ungroup() %>%
  arrange(doy) %>%
  as_tibble() 
zp_uM_sum

# Convert nutrient concentrations to uM 
gvl19_join = gvl19 %>%
  select(doy, SRP_mol, NOx_mol, orgP_mol, orgN_mol, TN_mol, TP_mol) %>%
  mutate(inorgP_uM = (SRP_mol*1000000), 
         inorgN_uM = (NOx_mol*1000000), 
         orgP_uM = (orgP_mol*1000000), 
         orgN_uM = (orgN_mol*1000000), 
         totalN_uM = (TN_mol*1000000),
         totalP_uM = (TP_mol*1000000)) %>%
  select(doy, inorgP_uM, inorgN_uM, orgP_uM, orgN_uM, totalP_uM, totalN_uM) %>%
  as_tibble()
gvl19_join

gv_pools = left_join(gvl19_join, zp_uM_sum, by='doy')
gv_pools

gv_uM_stocks = gv_pools %>% 
  select(sampleid, doy, totalN_uM, inorgN_uM, orgN_uM, n_uM_zp, 
         totalP_uM, inorgP_uM, orgP_uM, p_uM_zp) %>% # select useful columns
  as_tibble() 
gv_uM_stocks

# Plot figures #=========================================
# Top Figure (A & B)
windows(height=3, width=6)
par(mfrow=c(1,2), mai=c(0.8,0.5,0.06,0.05))

# N uM
# '#004ebe','#2a81ff', '#94c0ff'
plot(gv_uM_stocks$doy, log10(gv_uM_stocks$totalN_uM+1),  type = "o", lwd = 2, yaxt='n', xlim = c(120, 280), xaxp = c(140,280,7),
     pch = 19, col = '#004ebe', ylim = c(log10(1), log10(500)),
     ylab = '', 
     xlab = "", cex = 1.5, cex.axis = 1.1)
mtext(expression(N~Concentration~"("*mu*M*")"), side = 2, line = 1.5, cex = 1)
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), 
          log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)
points(gv_uM_stocks$doy, log10(gv_uM_stocks$inorgN_uM+1), type = 'o', lwd=2, 
       pch = 17, col = '#2a81ff', cex=1.5)
points(gv_uM_stocks$doy, log10(gv_uM_stocks$n_uM_zp+1), type = 'o', lwd=2,
       pch = 18, col = 'gray60', cex=1.5)


# P uM
# '#c24ad7', '#d786e4', '#ebc3f2'
plot(gv_uM_stocks$doy, log10(gv_uM_stocks$totalP_uM+1),  type = "o", lwd = 2, yaxt='n', xlim = c(120, 280), xaxp = c(140,280, 7),
     pch = 19, col = '#c24ad7', ylim = c(log10(1), log10(20)),ylab = '',
     #ylab = expression(N~Concentration~"("*mu*M*")"), 
     xlab = "", cex = 1.5, cex.axis = 1.1)
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), 
          log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','20'), las=2, cex.axis=0.8)
mtext(expression(P~Concentration~"("*mu*M*")"), side = 2, line = 1.5, cex = 1)
points(gv_uM_stocks$doy, log10(gv_uM_stocks$inorgP_uM+1), type = 'o', lwd=2.2, 
       pch = 17, col = '#d786e4', cex=1.5)
points(gv_uM_stocks$doy, log10(gv_uM_stocks$p_uM_zp+1), type ='o', lwd=2, 
       pch = 18, col = 'gray60', cex=1.5)

# Bottom Figure (C & D)
windows(height=3, width=6)
par(mfrow=c(1,2), mai=c(0.8,0.5,0.06,0.05), mgp=c(3,0.5,0))

# Ecosystem Nutrient Ratios # 
plot(gvl19$doy, log10(gvl19$total_NP), type = "o", lwd = 2, yaxt='n', xaxp = c(140,280, 7), xlim = c(120, 280), 
     pch = 19, col = "gray30", ylim = c(log10(0.1), log10(1000)),
     ylab = "", xlab = "",cex.axis = 1.1, cex=1.5)
text(125, log10(200), "Phosphorus \nLimited", srt = 90, cex = 0.8)
text(125, log10(1), "Nitrogen \nLimited", srt = 90, cex = 0.8)
mtext('N:P', side = 2, line = 1.5, cex = 1)
points(gvl19$doy, log10(gvl19$inorg_NP), 
       type = "o", lwd = 2, col = "gray50", pch = 17, cex = 1.5)
abline(log10(20),0, lty = 3, lwd = 2)
legend("topright", legend = c("Total N:P", "Inorganic N:P"), bty='n',
       pch = c(19, 17),cex = 0.8 ,pt.cex = 1.5, col = c("gray30", "gray50"))
axis(side=2,
     at=c(log10(0.1),
          log10(0.2),log10(0.3),log10(0.4),log10(0.5),log10(0.6),log10(0.7),log10(0.8),log10(0.9),
          log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000)), #Where the tick marks should be drawn
     labels = c('0.1', '', '','','','','','','','1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)
mtext('Day of Year, 2019', side = 1, line =3, cex = 1)
axis(1, at=c(140,152,182, 
             213, 244, 
             274, 280),
     line = 1.6, lwd.ticks = 0.5,
     labels=c("","Jun.","Jul.",
              'Aug.','Sep.','Oct.', ''), cex.axis=1)

# Zooplankton body nutrient ratios # 
# Plot for Green Valley Lake Zooplankton N:P # 
plot(zp_stoic_sum$doy, zp_stoic_sum$zp_np, type = 'o', xlim = c(120, 280), xaxp = c(140,280, 7),
     pch=18, col='gray60', lwd=2, xlab='', ylab='', cex.axis = 1.1, yaxt='n', cex=1.5) 
mtext('Zooplankton N:P', side = 2, line=1.5, cex=1)
mtext('Day of Year, 2019', side = 1, line =3, cex = 1)
axis(side=2,
     at=c(12,14,16,18,20,22,24),
     labels = c('','','','','','',''),
     las=2, cex.axis=0.8,gap.axis = 0.1)
axis(1, at=c(140,152,182, 
             213, 244, 
             274, 280),
     line = 1.6, lwd.ticks = 0.5,
     labels=c("","Jun.","Jul.",
              'Aug.','Sep.','Oct.', ''), cex.axis=1)


# N Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Total N', 'Inorganic N', 'Zooplankton N'), 
       pch=c(19, 17, 18), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#004ebe','#2a81ff', 'gray60'))

# P Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Total P', 'Inorganic P' ,'Zooplankton P'), 
       pch=c(19, 17, 18), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7', '#d786e4', 'gray60'))

# Plot just N and P body storage #========================
windows(height=3, width=6)
par(mfrow=c(1,2), mai=c(0.6,0.8,0.06,0.05))

# N uM
# '#004ebe','#2a81ff', '#94c0ff'
plot(gv_uM_stocks$doy, gv_uM_stocks$n_uM_zp,  type = "o", lwd = 2, xlim = c(120, 280), xaxp = c(140,280,7),
     pch = 19, col = '#004ebe', ylim = c(0,2),
     ylab = '', 
     xlab = "", cex = 1.5, cex.axis = 1.1)
mtext(expression(N~Storage~"("*mu*M*")"), side = 2, line = 2.5, cex = 1)

# P uM
# '#c24ad7', '#d786e4', '#ebc3f2'
plot(gv_uM_stocks$doy, gv_uM_stocks$p_uM_zp,  type = "o", lwd = 2, xlim = c(120, 280), xaxp = c(140,280, 7),
     pch = 19, col = '#c24ad7', ylim = c(0,2),ylab = '',
     #ylab = expression(N~Concentration~"("*mu*M*")"), 
     xlab = "", cex = 1.5, cex.axis = 1.1)
mtext(expression(P~Storage~"("*mu*M*")"), side = 2, line = 2.5, cex = 1)


# N uM
# '#004ebe','#2a81ff', '#94c0ff'
plot(gv_uM_stocks$doy, gv_uM_stocks$totalN_uM,  type = "o", lwd = 2, xlim = c(120, 280), xaxp = c(140,280,7),
     pch = 19, col = '#004ebe', ylim = c(0, 1),
     ylab = '', 
     xlab = "", cex = 1.5, cex.axis = 1.1)
mtext(expression(N~Concentration~"("*mu*M*")"), side = 2, line = 1.5, cex = 1)
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), 
          log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=0.8)
points(gv_uM_stocks$doy, gv_uM_stocks$inorgN_uM, type = 'o', lwd=2, 
       pch = 17, col = '#2a81ff', cex=1.5)
points(gv_uM_stocks$doy, gv_uM_stocks$n_uM_zp, type = 'o', lwd=2,
       pch = 18, col = 'gray60', cex=1.5)


# P uM
# '#c24ad7', '#d786e4', '#ebc3f2'
plot(gv_uM_stocks$doy, gv_uM_stocks$totalP_uM,  type = "o", lwd = 2, xlim = c(120, 280), xaxp = c(140,280, 7),
     pch = 19, col = '#c24ad7', ylim = c(log10(1), log10(20)),ylab = '',
     #ylab = expression(N~Concentration~"("*mu*M*")"), 
     xlab = "", cex = 1.5, cex.axis = 1.1)
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), 
          log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','20'), las=2, cex.axis=0.8)
mtext(expression(P~Concentration~"("*mu*M*")"), side = 2, line = 1.5, cex = 1)
points(gv_uM_stocks$doy, log10(gv_uM_stocks$inorgP_uM+1), type = 'o', lwd=2.2, 
       pch = 17, col = '#d786e4', cex=1.5)
points(gv_uM_stocks$doy, log10(gv_uM_stocks$p_uM_zp+1), type ='o', lwd=2, 
       pch = 18, col = 'gray60', cex=1.5)

