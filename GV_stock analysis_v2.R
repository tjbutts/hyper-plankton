# Organic - Seston - Inorganic Stock Analysis_v2 #
rm(list = ls()) 

# set working directory 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")

# load libraries 
library(tidyverse)

# Zooplankton N:P ==============================
# Purpose: Assess zooplankton N:P ratios for Green Valley Zooplankton community 

# load in dataset with n and p information 
zp_cnp = read.csv('moody_zooplankton_cnp.csv')

zp_cnp = zp_cnp %>%
  rename(perc_c = X.C, 
         perc_n = X.N, 
         perc_p = X.P)

# read in zp biomass 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")

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
         n_molar = n_storage/14010000, 
         p_molar = p_storage/30970000) %>% # Get molar ratio 
  rename(group = group.x) %>%
  as_tibble()
zp_stoic2

# Get Zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum = zp_stoic2 %>% 
  group_by(sampleid, doy) %>%
  summarise(
    nstorage_molar = sum(n_molar), 
    pstorage_molar = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble() 
zp_stoic_sum

# Calculate community N:P 
zp_stoic_sum2 = zp_stoic_sum %>%
  mutate(zp_np = (nstorage_molar)/(pstorage_molar))
zp_stoic_sum2 = zp_stoic_sum2 %>% arrange(doy)
zp_stoic_sum2

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(zp_stoic_sum2$doy, zp_stoic_sum2$zp_np, type = 'o', pch=17, col='black', cex=3, cex.axis=2 , lwd=4, xlab='', ylab='') 
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Day of Year, 2019', side = 1, line =3, cex = 2)

# GVL inorganic - inorganic stocks #====================================
# Code originally written by Dr. Grace Wilkinson # 
# Purpose: Calculate stocks of organic, inorganic, and organic nutrients bound in zooplankton 
library(tidyverse)
# Read in the data 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")
gvl = read.csv("GVL_nutrients_w_ALM.csv")

#Subset to the study year, site, and surface depth 
gvl19 = gvl %>%
  filter(year==2019, siteID==4, depthID==1) %>%
  filter(project=="GVLmonitoring") %>%
  select(doy:SRP_ugL, TP_ugL, TN_mgL, NOx_mgL, microcystin_ugL) %>%
  filter(!(doy == 162))
gvl19

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

windows(height= 5, width = 6)
plot(gvl19$doy, log10(gvl19$org_NP), type = "o", lwd = 2,
     ylim = c(log10(0.1), log10(1000)), xlim = c(120, 280), 
     pch = 19, col = "mediumseagreen",
     ylab = "lgo10(N:P)", xlab = "Day of Year")
text(125, log10(200), "Phosphorus \nLimited", srt = 90)
text(125, log10(1), "Nitrogen \nLimited", srt = 90)
points(gvl19$doy, log10(gvl19$total_NP), 
       type = "o", lwd = 2, col = "gray30", pch = 19)
points(gvl19$doy, log10(gvl19$inorg_NP), 
       type = "o", lwd = 2, col = "dodgerblue2", pch = 19)
abline(log10(20),0, lty = 3)

legend("topright", legend = c("Total N:P", "Organic N:P", "Inorganic N:P"),
       pch = 15, pt.cex = 2, col = c("gray30", "mediumseagreen", "dodgerblue2"))

# Calculate fraction of organic N and P bound in zooplankton ===========================
zp_nut_sum = zp_stoic2 %>% # pull from zoop biomass data before converting to molar data 
  group_by(sampleid, doy) %>%
  summarise(
    mgN = sum(n_storage)/1000, 
    ugP = sum(p_storage)) %>% # ug/L of N and P in zooplankton 
  ungroup() %>%
  as_tibble() 
zp_nut_sum 

gv19_org = gvl19 %>% # Get organic fractions of N and P 
  select(doy, sampleDepth, orgP_ugL, orgN_mgL) %>%
  as_tibble()

zp_orgfrac_join = left_join(zp_nut_sum, gv19_org, by = 'doy')  %>% # Join ZP N and P storage to organic fractions 
  arrange(doy)
zp_orgfrac_join

zp_orgfrac = zp_orgfrac_join %>%
  mutate(zpN_frac = (mgN/orgN_mgL)*100, 
         zpP_frac = (ugP/orgP_ugL)*100) %>% # get percentage of N and P bound in zoops from organic fraction 
  as_tibble()
zp_orgfrac

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(zp_orgfrac$doy, zp_orgfrac$zpN_frac, type = "o", lwd = 2,
     ylim = c(0,5), xlim = c(120, 280), 
     pch = 19, col = "#004ebe",
     ylab = "Percent fraction of organic nutrients", xlab = "Day of Year, 2019")
points(zp_orgfrac$doy, zp_orgfrac$zpP_frac, 
       type = "o", lwd = 2, col = "#c24ad7", pch = 19)
legend("topright", legend = c("N bound in ZP", "P bound in ZP"),
       pch = 15, pt.cex = 2, col = c("#004ebe", "#c24ad7" ))

# Visualize GVL inorganic - organic - zooplankton stocks week to week (umol/L) ========================

rm(list=ls()) # clear working environment up to this point 
library(tidyverse)

# set working directory 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")

zp_cnp = read.csv('moody_zooplankton_cnp.csv')

zp_cnp = zp_cnp %>%
  rename(perc_c = X.C, 
         perc_n = X.N, 
         perc_p = X.P)

# read in zp biomass 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")

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

# Other GV Data 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")
gvl = read.csv("GVL_nutrients_w_ALM.csv")

#Subset to the study year, site, and surface depth 
gvl19 = gvl %>%
  filter(year==2019, siteID==4, depthID==1) %>%
  filter(project=="GVLmonitoring") %>%
  select(doy:SRP_ugL, TP_ugL, TN_mgL, NOx_mgL, microcystin_ugL) %>%
  filter(!(doy == 162))
gvl19

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

head(gvl19)

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
plot(gv_uML_stocks$doy, log(gv_uML_stocks$totalN_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', ylim = c(log(1), log(1000)), 
     ylab = 'ln(Nitrogen concentration (umol N/L))', xlab = 'Day of Year, 2019', yaxt = 'n', cex.lab = 1.5, cex.axis = 1.5)
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000'), las=2, cex.axis=1.5)
points(gv_uML_stocks$doy, log(gv_uML_stocks$inorgN_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#2a81ff')
points(gv_uML_stocks$doy, log(gv_uML_stocks$orgN_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#94c0ff')
points(gv_uML_stocks$doy, log(gv_uML_stocks$n_uML_zp+1), type = 'o', lwd=3,
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
plot(gv_uML_stocks$doy, log(gv_uML_stocks$totalP_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', ylim = c(log(1), log(20)), 
     ylab = 'ln(Phosphorus concentration (umol P/L))', xlab = 'Day of Year, 2019', yaxt = 'n', cex.lab = 1.5, cex.axis = 1.5)
axis(side=2,
     at=c(log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','20'), las=2, cex.axis=1.5)
points(gv_uML_stocks$doy, log(gv_uML_stocks$inorgP_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#d786e4')
points(gv_uML_stocks$doy, log(gv_uML_stocks$orgP_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#ebc3f2')
points(gv_uML_stocks$doy, log(gv_uML_stocks$p_uML_zp+1), type ='o', lwd=3, 
       pch = 19, col = 'gray60')

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Total P', 'Inorganic P', 'Organic P' ,'Zooplankton P'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7', '#d786e4', '#ebc3f2', 'gray60'))


# Elemental Imbalance re: Elser & Hassett 1994 =======================
# From Elser & Hassett 1994 - N:P(imbalance) = N:P(seston) - N:P(consumers) 
# Calculate N:P imbalance between seston (assuming here organic N:P) and consumers (zooplankton N:P)

# get molar ratio of for each stock 
head(gv_uML_stocks) # Convert back to mol to get N:P 
gv_org_mols = gv_uML_stocks %>%
  select(sampleid, doy, orgN_uML, orgP_uML, n_uML_zp, p_uML_zp) %>%
  mutate(org_NP = (orgN_uML/1000000)/(orgP_uML/1000000), 
         zp_NP = (n_uML_zp/1000000)/(p_uML_zp/1000000)) %>%
  select(sampleid, doy, org_NP, zp_NP) %>%
  as.data.frame()
gv_org_mols

gv_imbalance = gv_org_mols %>%
  filter(!(is.na(zp_NP) | is.na(org_NP))) %>%
  select(doy, org_NP, zp_NP) %>%
  mutate(imbalance = org_NP-zp_NP)
gv_imbalance

windows(height= 5, width = 6)
plot(gv_imbalance$doy, gv_imbalance$imbalance, type = "o", lwd = 3,
     ylim = c(-10, 120), xlim = c(110, 280), 
     pch = 19, col = "mediumseagreen",
     ylab = 'Elemental Imbalance (Seston-Consumer N:P)', xlab = "Day of Year, 2019")
text(142, 10, "P retained, N recycled", srt = 360, font=2)
text(142, -10, "N retained, P recycled", srt = 360, font=2)
abline(0,0, lty = 3, lwd=5, col='black')
