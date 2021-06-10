# GV Figure 2 - System molar N:P ratios and zooplankton molar N:P ratios # 
rm(list=ls())

# GVL organic - inorganic stocks #====================================
# Code originally written by Dr. Grace Wilkinson # 
# Purpose: Calculate stocks of organic, inorganic, and organic nutrients bound in zooplankton 
library(tidyverse)
# Read in the data 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Tyler GVL")

gvl = read.csv("GVL_nutrients_w_ALM.csv")

#Subset to the study year, site, and surface depth 
gvl19 = gvl %>%
  filter(year==2019, siteID==4, depthID==1) %>%
  filter(project=="GVLmonitoring") %>%
  select(doy:SRP_ugL, TP_ugL, TN_mgL, NOx_mgL, microcystin_ugL) %>%
  filter(!(doy == 162))
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


windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1))
plot(gvl19$doy, log10(gvl19$org_NP), type = "o", lwd = 3, yaxt='n', xlim = c(120, 280), 
     pch = 15, col = "gray30", ylim = c(log10(0.1), log10(1000)),
     ylab = "", xlab = "Day of Year, 2019", cex = 3, cex.lab = 1.5)
text(125, log10(200), "Phosphorus \nLimited", srt = 90, cex = 1.5)
text(125, log10(1), "Nitrogen \nLimited", srt = 90, cex = 1.5)
mtext('log10(N:P)', side = 2, line = 2.5, cex = 1.5)
points(gvl19$doy, log10(gvl19$total_NP), 
       type = "o", lwd = 3, col = "gray10", pch = 19, cex = 3)
points(gvl19$doy, log10(gvl19$inorg_NP), 
       type = "o", lwd = 3, col = "gray50", pch = 17, cex = 3)
abline(log10(20),0, lty = 3, lwd = 2)
legend("topright", legend = c("Total N:P", "Organic N:P", "Inorganic N:P"),
       pch = c(19, 15, 17),cex = 1.5 ,pt.cex = 3, col = c("gray10", "gray30", "gray50"))
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2)

# Zooplankton N:P Ratio over the growing season #========================
# Set wd 
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
setwd("C:/Users/Tyler/Box Sync/Active/Active GV Research/Trait compilation")

# Load in libraries 
library(tidyverse)
library(magrittr)

# Load in dataset with n and p information 
zp_cnp <- read.csv('moody_zooplankton_cnp.csv')
as_tibble(zp_cnp)

zp_cnp %<>% rename(perc_c = X.C) %>%
  rename(perc_n = X.N) %>%
  rename(perc_p = X.P) %>%
  as_tibble()
zp_cnp %<>% select(taxon, group, perc_c, perc_n, perc_p) %>% as_tibble()
zp_cnp

# read in zp biomass 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
setwd("C:/Users/Tyler/Box Sync/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")


# Turn raw R output into a cleaned zoop data
zp_raw <- read.csv('2019_site4_gv_ZoopBiomass_25Feb2021.csv')


zp_raw2 <- zp_raw %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID) %>%
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(biomass = BIOMASS.UG.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

gv19_zp = zp_raw2 %>% select(sampleid,doy, taxon,group,biomass) %>% 
  mutate(biomass = replace_na(biomass, 0)) %>% # Replace NAs with 0s 
  filter(!(doy == 162 | doy == 157 | doy == 211)) # Remove the ALM sampling dates plus DOY 157 which we determined was wonky
as_tibble(gv19_zp)

# Combine biomass with cnp data 
zp_stoic <- left_join(gv19_zp, zp_cnp, by='taxon') # Join np with biomass data 
as_tibble(zp_stoic)
zp_stoic %<>% select(sampleid, doy, taxon, group.y, biomass, perc_c, perc_n, perc_p) %>%
  rename(group = group.y) %>% as_tibble()
as_tibble(zp_stoic)

# Group taxon together in stoich groupings, then calculate ugX_L 
zp_stoic_ugL <- zp_stoic %>% 
  mutate(p_storage = (perc_p/100)*biomass) %>%
  mutate(n_storage = (perc_n/100)*biomass) %>%
  mutate(c_storage = (perc_c/100)*biomass) %>% 
  group_by(doy, group) %>%
  summarise(ugp_L = sum(p_storage),
            ugn_L = sum(n_storage), 
            ugc_L = sum(c_storage)) %>%
  as_tibble()
zp_stoic_ugL  

# Multiply C, N, and P by biomass to get C, N, and P storage weighted by biomass by taxon
zp_stoic_molar <- zp_stoic %>% select(sampleid, doy, taxon, group, biomass, perc_c, perc_n, perc_p) %>% # Select appropriate columns 
  mutate(p_storage = (perc_p/100)*biomass) %>%
  mutate(n_storage = (perc_n/100)*biomass) %>% 
  mutate(c_storage = (perc_c/100)*biomass) %>% 
  mutate(n_molar = n_storage/14010000) %>% # Get molar ratio 
  mutate(p_molar = p_storage/30970000) %>% # Get molar ratio 
  select(sampleid, doy, taxon, group, biomass, n_molar, p_molar) %>%
  as_tibble
zp_stoic_molar

# Get Zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum <- zp_stoic_molar %>% 
  group_by(sampleid, doy) %>%
  summarise(
    nstorage = sum(n_molar), 
    pstorage = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble()
zp_stoic_sum

# Calculate community N:P 
zp_stoic_sum %<>% mutate(zp_np = (nstorage)/(pstorage)) 
zp_stoic_sum

windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1))
plot(zp_stoic_sum$doy, zp_stoic_sum$zp_np, type = 'o', xlim = c(120, 280),
     pch=19, col='black', cex=3, cex.axis = 2, lwd=3, xlab='', ylab='') 
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Day of Year, 2019', side = 1, line =3, cex = 2)
#legend('topright', legend=c('Zoop N:P'), col = c('black'), cex=1.5, pt.cex=3, pch=c(19))



# N and p stocks in umol 
# (Potentially relevant information) ==============================
## Visualize GVL inorganic - organic - zooplankton stocks week to week (umol/L) 

library(tidyverse)

# set working directory 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Tyler GVL")

zp_cnp = read.csv('moody_zooplankton_cnp.csv')

zp_cnp = zp_cnp %>%
  rename(perc_c = X.C, 
         perc_n = X.N, 
         perc_p = X.P)

# read in zp biomass 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
setwd("C:/Users/Tyler/Box Sync/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")


# Turn raw R output into cleaned zoop data
zp_raw = read.csv('2019_site4_gv_ZoopBiomass_25Feb2021.csv')
zp_raw2 = zp_raw %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID,
         taxon = TAXON,
         lakeno = LAKE.NO,
         biomass = BIOMASS.UG.L,
         group = GROUP,
         doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 

gv19_zp = zp_raw2 %>% 
  select(sampleid,doy, taxon, group, biomass) %>% 
  mutate(biomass = replace_na(biomass, 0)) %>% # Replace NAs with 0s 
  filter(!(doy == 162 | doy == 157)) # Remove the ALM sampling dates plus DOY 157 which we determined was wonky

# Combine biomass with cnp data 
zp_stoic <- left_join(gv19_zp, zp_cnp, by='taxon') # Join np with biomass data 

# Multiply N and P by biomass to get N and P storage weighted by biomass by taxon 
zp_stoic2 = zp_stoic %>%
  mutate(p_storage = (perc_p/100)*biomass, 
         n_storage = (perc_n/100)*biomass, 
         p_molar = (p_storage/30970000), 
         n_molar = (n_storage/14010000),
         n_uML = n_molar*1000000, # Get uM/L 
         p_uML = p_molar*1000000) %>% 
  rename(group = group.x) %>% 
  select(sampleid, doy, taxon, group, biomass, n_uML, p_uML) %>%
  as_tibble()
zp_stoic2

# Get daily micromoles of N and P per taxonomic group 
zp_uM_sum = zp_stoic2 %>% 
  group_by(sampleid, doy) %>%
  summarise(
    n_uML_zp = sum(n_uML), 
    p_uML_zp = sum(p_uML)) %>%
  ungroup() %>%
  arrange(doy) %>%
  as_tibble() 
zp_uM_sum

# Write general gv stocks .csv 
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")

gvl19 = read.csv('gv19_nutrientstocks.csv')

# Columns: doy, SRP_mol, NOx_mol, orgP_mol, orgN_mol 
# Convert values to micromol/L 
gvl19_join = gvl19 %>%
  select(doy, SRP_mol, NOx_mol, orgP_mol, orgN_mol, TN_mol, TP_mol) %>%
  mutate(inorgP_uML = (SRP_mol*1000000), 
         inorgN_uML = (NOx_mol*1000000), 
         orgP_uML = (orgP_mol*1000000), 
         orgN_uML = (orgN_mol*1000000), 
         totalN_uML = (TN_mol*1000000),
         totalP_uML = (TP_mol*1000000)) %>%
  select(doy, inorgP_uML, inorgN_uML, orgP_uML, orgN_uML, totalP_uML, totalN_uML) %>%
  as_tibble()
gvl19_join

gv_pools = left_join(gvl19_join, zp_uM_sum, by='doy')
gv_pools

gv_uML_stocks = gv_pools %>% 
  select(sampleid, doy, totalN_uML, inorgN_uML, orgN_uML, n_uML_zp, 
         totalP_uML, inorgP_uML, orgP_uML, p_uML_zp) %>% # select useful columns
  as_tibble() 
gv_uML_stocks

# N uM/L 
# '#004ebe','#2a81ff', '#94c0ff'
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log10(gv_uML_stocks$totalN_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', ylim = c(log10(1), log10(1000)), 
     ylab = 'log10 N concentration (umol N/L))', xlab = 'Day of Year, 2019', yaxt = 'n', cex.lab = 1.5, cex.axis = 1.5)
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), 
          log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1.5)
points(gv_uML_stocks$doy, log10(gv_uML_stocks$inorgN_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#2a81ff')
points(gv_uML_stocks$doy, log10(gv_uML_stocks$orgN_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#94c0ff')
points(gv_uML_stocks$doy, log10(gv_uML_stocks$n_uML_zp+1), type = 'o', lwd=3,
       pch = 19, col = 'gray60')

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Total N', 'Inorganic N', 'Organic N', 'Zooplankton N'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#004ebe','#2a81ff', '#94c0ff', 'gray60'))

# P uM/L 
# '#c24ad7', '#d786e4', '#ebc3f2'
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log10(gv_uML_stocks$totalP_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', ylim = c(log10(1), log10(20)), 
     ylab = 'ln(Phosphorus concentration (umol P/L))', xlab = 'Day of Year, 2019', yaxt = 'n', cex.lab = 1.5, cex.axis = 1.5)
axis(side=2,
     at=c(log10(0.1), log10(0.2), log10(0.3), log10(0.4), log10(0.5), log10(0.6), log10(0.7), log10(0.8), 
          log10(0.9), log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20)), #Where the tick marks should be drawn
     labels = c('0.1','','','','','','','','','1', '', '','','','','','','','10','20'), las=2, cex.axis=1.5)

points(gv_uML_stocks$doy, log10(gv_uML_stocks$inorgP_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#d786e4')
points(gv_uML_stocks$doy, log10(gv_uML_stocks$orgP_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#ebc3f2')
points(gv_uML_stocks$doy, log10(gv_uML_stocks$p_uML_zp+1), type ='o', lwd=3, 
       pch = 19, col = 'gray60')

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Total P', 'Inorganic P', 'Organic P' ,'Zooplankton P'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7', '#d786e4', '#ebc3f2', 'gray60'))


