# Organic - Seston - Inorganic Stock Analysis #
rm(list = ls()) 

# set working directory 
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")

# load libraries 
library(tidyverse)

# Zooplankton N:P ==============================
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
    nstorage = sum(n_molar), 
    pstorage = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble() 
zp_stoic_sum

# Calculate community N:P 
zp_stoic_sum2 = zp_stoic_sum %>%
  mutate(zp_np = (nstorage)/(pstorage))
zp_stoic_sum2 = zp_stoic_sum2 %>% arrange(doy)
zp_stoic_sum2

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(zp_stoic_sum2$doy, zp_stoic_sum2$zp_np, type = 'o', pch=17, col='black', cex=3, cex.axis=2 , lwd=4, xlab='', ylab='') 
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Day of Year, 2019', side = 1, line =3, cex = 2)
legend('topright', legend=c('Zoop N:P'), col = c('black'), cex=1.5, pt.cex=3, pch=c(17))

# Calculate ug N/L per week 
zp_nut_sum = zp_stoic2 %>% 
  group_by(sampleid, doy) %>%
  summarise(
    ugN = sum(n_storage), 
    ugP = sum(p_storage)) %>%
  ungroup() %>%
  as_tibble() 
zp_nut_sum

# GVL inorganic - inorganic stocks #====================================
# Code originally written by Dr. Grace Wilkinson # 
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

#Plot Zoop-Seston-Inorganic Stocks per week ====================================
head(gvl19)
# Columns: doy, SRP_ugL, NOx_mgL orgP_ugL, orgN_ugL
gvl19_join = gvl19 %>%
  select(doy, SRP_ugL, NOx_mgL, orgP_ugL, orgN_mgL)
gvl19_join
zp_nut_sum
gv_pools = left_join(gvl19_join, zp_nut_sum, by='doy')
gv_stocks = gv_pools %>% 
  select(sampleid, doy, SRP_ugL, NOx_mgL, orgP_ugL, orgN_mgL, ugN, ugP) %>% # select useful columns
  rename(zpN_ugL = ugN, 
         zpP_ugL = ugP) %>% # denote that these columns refer to zoops 
  mutate(NOx_ugL = NOx_mgL*1000, 
         orgN_ugL = orgN_mgL*1000) %>% # convert nitrogen from mgL to ugL in order to compare 
  mutate(sesP_ugL = orgP_ugL-zpP_ugL, 
         sesN_ugL = orgN_ugL-zpN_ugL) %>% # Calculate seston by taking organic pool and subtracting zoops to get seston
  select(sampleid, doy, zpN_ugL, zpP_ugL, SRP_ugL, NOx_ugL, sesP_ugL, sesN_ugL) %>% 
  as_tibble() 
gv_stocks

gv_stocks_long = gv_stocks %>%
  pivot_longer(cols = c(zpN_ugL, zpP_ugL, SRP_ugL, NOx_ugL, sesP_ugL, sesN_ugL), values_to = 'concentration')
head(gv_stocks_long)

# Plot 
library(ggplot2)

#Nitrogen - need to figure out log scale 
# Nitrogen species(sesN_ugL = #004ebe; zpN_ugL = #2a81ff; NOx_ugL = #94c0ff)

# Plot in Base R 
gv_N = gv_stocks %>%
  select(doy, zpN_ugL, sesN_ugL, NOx_ugL) %>% # Select N info 
  as.data.frame() 
gv_N_long = gv_stocks_long %>% 
  filter(name == 'NOx_ugL' |name == 'zpN_ugL' | name == 'sesN_ugL') %>%
  filter(!(doy == 157))
gv_N_long =gv_N_long %>% 
  select(doy, name, concentration) %>%
  pivot_wider(names_from = doy, 
              values_from = concentration) %>%
  as.data.frame()
gv_N_long
gv_N_long <- gv_N_long %>% select(!(name))
gv_N_long
row.names(gv_N_long) <- c('NOx_ugL', 'zpN_ugL', 'sesN_ugL')
gv_N_m <- as.matrix(gv_N_long)
gv_N_m                                        # Print modified data

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(height = log(gv_N_m+1), beside = TRUE, yaxt ='n', col= c('#004ebe', '#2a81ff', '#94c0ff'), cex.names = 1.5, las=2)
box()
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000), log(2000), log(3000), log(4000), log(5000)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', '', '', '', '', '', '', '1000', '', '', '4000', ''), las=2, cex.axis=1.5)

mtext('N concentration (ug/L)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Zooplankton N', 'Inorganic N', 'Seston N'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#004ebe','#2a81ff', '#94c0ff'))



#Phosphorus - need to figure out log scale 
# Phosphorus species(sesP_ugL = #c24ad7; zpP_ugL = #d786e4; SRP_ugL = #ebc3f2)

# Plot in Base R 
gv_P = gv_stocks %>%
  select(doy, zpP_ugL, sesP_ugL, SRP_ugL) %>% # Select N info 
  as.data.frame() 
gv_P_long = gv_stocks_long %>% 
  filter(name == 'SRP_ugL' |name == 'zpP_ugL' | name == 'sesP_ugL') %>%
  filter(!(doy == 157))
gv_P_long =gv_P_long %>% 
  select(doy, name, concentration) %>%
  pivot_wider(names_from = doy, 
              values_from = concentration) %>%
  as.data.frame()
gv_P_long
gv_P_long <- gv_P_long %>% select(!(name))
gv_P_long
row.names(gv_P_long) <- c('SRP_ugL', 'zpP_ugL', 'sesP_ugL')
gv_P_m <- as.matrix(gv_P_long)
gv_P_m                                        # Print modified data

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(height = log(gv_P_m+1), beside = TRUE, yaxt ='n', col= c('#c24ad7', '#d786e4', '#ebc3f2'), cex.names = 1.5, las=2)
box()
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', ''), las=2, cex.axis=1.5)

mtext('P concentration (ug/L)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Zooplankton P', 'Inorganic P', 'Seston P'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7', '#d786e4', '#ebc3f2'))



#Plot Zoop-Seston_Inorganic Stacked Bar graph
# Formatted N and P information 
gv_P_m
gv_N_m
Pdat = log(gv_P_m+1)

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv_P_m, 
        col=c('#c24ad7', '#d786e4', '#ebc3f2'), 
        border='black', 
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('P concentration (ug/L)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv_N_m, 
        col=c('#004ebe','#2a81ff', '#94c0ff'), 
        border='black', 
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('N concentration (ug/L)', side = 2, line=3.5, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# in mg/L
windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot((gv_N_m/1000), 
        col=c('#004ebe','#2a81ff', '#94c0ff'), 
        border='black', 
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('N concentration (ug/L)', side = 2, line=3.5, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

#Plot Zoop-Seston-Inorganic N:P per week ===========================
head(gvl19)
# Columns: doy, SRP_ugL, NOx_mgL orgP_ugL, orgN_ugL
gvl19_join = gvl19 %>%
  select(doy, SRP_ugL, NOx_mgL, orgP_ugL, orgN_mgL)
gvl19_join
zp_nut_sum
gv_pools = left_join(gvl19_join, zp_nut_sum, by='doy')
gv_stocks = gv_pools %>% 
  select(sampleid, doy, SRP_ugL, NOx_mgL, orgP_ugL, orgN_mgL, ugN, ugP) %>% # select useful columns
  rename(zpN_ugL = ugN, 
         zpP_ugL = ugP) %>% # denote that these columns refer to zoops 
  mutate(NOx_ugL = NOx_mgL*1000, 
         orgN_ugL = orgN_mgL*1000) %>% # convert nitrogen from mgL to ugL in order to compare 
  mutate(sesP_ugL = orgP_ugL-zpP_ugL, 
         sesN_ugL = orgN_ugL-zpN_ugL) %>% # Calculate seston by taking organic pool and subtracting zoops to get seston
  select(sampleid, doy, zpN_ugL, zpP_ugL, SRP_ugL, NOx_ugL, sesP_ugL, sesN_ugL) %>% 
  as_tibble() 
gv_stocks

# get molar ratio of for each stock 
head(gv_stocks)

gv_molar_stocks = gv_stocks %>%
  mutate(zp_Pmol = zpP_ugL/(1000000*30.97), 
         zp_Nmol = zpN_ugL/(1000000*14.01), 
         inorg_Pmol = SRP_ugL/(1000000*30.97), 
         inorg_Nmol = NOx_ugL/(1000000*14.01),
         ses_Pmol = sesP_ugL/(1000000*30.97), 
         ses_Nmol = sesN_ugL/(1000000*14.01)) %>%
  mutate(zp_np = zp_Nmol/zp_Pmol, 
         inorg_np = inorg_Nmol/inorg_Pmol,
         ses_np = sesN_ugL/sesP_ugL) %>%
  select(sampleid, doy, zp_np, inorg_np, ses_np) %>%
  as.data.frame()
gv_molar_stocks <- do.call(data.frame, lapply(gv_molar_stocks, 
                                              function(x) replace(x, is.infinite(x), NA)))
gv_molar_stocks

# Plot in Base R 
gv_molar_long = gv_molar_stocks %>%
  filter(!(doy == 157)) %>% 
  pivot_longer(cols = c(zp_np, inorg_np, ses_np), values_to = 'np') %>%
  select(doy, name, np) %>% 
  pivot_wider(names_from = doy,
              values_from = np) %>%
  as.data.frame()
gv_molar_long
gv_molar_long <- gv_molar_long %>% select(!(name))
row.names(gv_molar_long) <- c('zp_np', 'inorg_np', 'ses_np')
gv_molar_m <- as.matrix(gv_molar_long)
gv_molar_m                                        # Print modified data

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(height = log(gv_molar_m+1), beside = TRUE, yaxt ='n', col= c('#550eaf', '#8a39ef', '#c49cf7'), cex.names = 1.5, las=2)
box()
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', '', '', '', '', '', '', '1000'), las=2, cex.axis=1.5)

mtext('Nitrogen: Phosphorus ratio', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Zooplankton N:P', 'Inorganic N:P', 'Seston N:P'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#550eaf', '#8a39ef', '#c49cf7'))

# non-log10
windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(height = gv_molar_m, beside = TRUE, yaxt ='n',ylim = c(0, 61), col= c('#550eaf', '#8a39ef', '#c49cf7'), cex.names = 1.5, las=2)
box()
axis(side=2,
     at=c(0,10,20,30,40,50,60), #Where the tick marks should be drawn
     las=2, cex.axis=1.5)

mtext('Nitrogen: Phosphorus ratio', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Zooplankton N:P', 'Inorganic N:P', 'Seston N:P'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#550eaf', '#8a39ef', '#c49cf7'))

# Zooplankton - Seston elemental imbalance # ======================================
# From Elser & Hassett 1994 - N:P(imbalance) = N:P(seston) - N:P(consumers) 
gv_molar_stocks = as_tibble(gv_molar_stocks)

gv_imbalance = gv_molar_stocks %>%
  filter(!(is.na(zp_np) | is.na(ses_np))) %>%
  select(doy, ses_np, zp_np) %>%
  mutate(imbalance = ses_np-zp_np)
gv_imbalance

windows(height= 5, width = 6)
plot(gv_imbalance$doy, gv_imbalance$imbalance, type = "o", lwd = 3,
     ylim = c(-20, 50), xlim = c(110, 280), 
     pch = 19, col = "mediumseagreen",
     ylab = 'Elemental Imbalance (Seston-Consumer N:P)', xlab = "Day of Year, 2019")
text(142, 10, "P retained, N recycled", srt = 360, font=2)
text(142, -10, "N retained, P recycled", srt = 360, font=2)
abline(0,0, lty = 3, lwd=5, col='black')

# Historical elemental imbalance 
library(magrittr)
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
hist_plankton_cnp <- read.csv('zp_np_historical.csv')
as_tibble(hist_plankton_cnp)
hist_plankton_cnp %<>% filter(!(genus=='Ostracod'))

# Multiply C, N, and P by biomass to get C, N, and P storage weighted by biomass by taxon
zp_stoic_molar <- hist_plankton_cnp %>% 
  mutate(p_storage = (perc_p/100)*biomass) %>%
  mutate(n_storage = (perc_n/100)*biomass) %>% 
  mutate(c_storage = (perc_c/100)*biomass) %>% 
  mutate(n_molar = n_storage/14010000) %>% # Get molar ratio 
  mutate(p_molar = p_storage/30970000) %>% # Get molar ratio 
  select(sampledate, doy, year, genus, division, biomass, n_molar, p_molar) %>%
  as_tibble
zp_stoic_molar

# Get zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum <- zp_stoic_molar %>% 
  group_by(doy, year, sampledate) %>%
  summarise(
    nstorage = sum(n_molar), 
    pstorage = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble()
zp_stoic_sum

# Get Zooplankton average biomass 
zp_avgbiomass <- hist_plankton_cnp %>%
  group_by(doy, year) %>%
  summarise( 
    avg_biom = sum(biomass)) %>%
  ungroup() %>%
  as_tibble()
zp_avgbiomass

# Calculate community N:P 
zp_stoic_hist <- zp_stoic_sum %>% mutate(zp_np = (nstorage)/(pstorage)) 
zp_stoic_hist
zp_stoic_hist = zp_stoic_hist %>% filter(!(doy == 157 & year == 2019))

# Yearly avg N:P 
zp_avg <- zp_stoic_hist %>%
  group_by(year) %>% 
  summarise(
    avg_np = mean(zp_np)) %>%
  as_tibble()
zp_avg


# Plot stoich  
windows(height=6, width=10)
par(mai=c(0.9,1,0.6,1))
zp_stoic_hist <- as.data.frame(zp_stoic_hist)

plot(zp_stoic_hist[zp_stoic_hist$year=='2011', "doy"], zp_stoic_hist[zp_stoic_hist$year == "2011", "zp_np"], 
     col = '#66c2a4', pch=17,type='o', cex=2, xlim=c(140,280), ylim=c(12,41), lwd=4, 
     cex.axis=2, xlab='', ylab='')
points(zp_stoic_hist[zp_stoic_hist$year=='2012', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2012', 'zp_np'],
       col = '#66c2a4', pch=15,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2013', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2013', 'zp_np'], 
       col = '#66c2a4', pch =19,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2014', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2014', 'zp_np'],
       col = '#238b45', pch=17,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2015', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2015', 'zp_np'], 
       col = '#238b45', pch=15,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2016', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2016', 'zp_np'],
       col = '#238b45', pch=19,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2017', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2017', 'zp_np'],
       col = '#00441b', pch=17,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2018', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2018', 'zp_np'],
       col = '#00441b', pch=15,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2019', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2019', 'zp_np'],
       col = '#00441b', pch=19,type='o', cex=2, lwd=4)
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Julian Day of Year', side = 1, line =3, cex = 2)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('2011', '2012', '2013','2014','2015','2016','2017','2018','2019'), 
       pch=c(17,15,19,17,15,19,17,15,19), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#66c2a4','#66c2a4','#66c2a4',
               '#238b45','#238b45','#238b45', 
               '#00441b','#00441b','#00441b'))
mtext("Year", at=0.2, cex=2)

# Plot ZP Biomass 
windows(height=6, width=10)
par(mai=c(0.9,1,0.6,1))
zp_avgbiomass <- zp_avgbiomass %>% filter(!(doy == 157 & year == 2019))
zp_avgbiomass <- as.data.frame(zp_avgbiomass)

plot(zp_avgbiomass[zp_avgbiomass$year=='2011', "doy"], zp_avgbiomass[zp_avgbiomass$year == "2011", "avg_biom"], 
     col = '#66c2a4', pch=17,type='o', cex=2, xlim=c(140,280), ylim=c(0, max(zp_avgbiomass$avg_biom)), lwd=4, 
     cex.axis=2, xlab='', ylab='')
points(zp_avgbiomass[zp_avgbiomass$year=='2012', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2012', 'avg_biom'],
       col = '#66c2a4', pch=15,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2013', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2013', 'avg_biom'], 
       col = '#66c2a4', pch =19,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2014', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2014', 'avg_biom'],
       col = '#238b45', pch=17,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2015', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2015', 'avg_biom'], 
       col = '#238b45', pch=15,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2016', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2016', 'avg_biom'],
       col = '#238b45', pch=19,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2017', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2017', 'avg_biom'],
       col = '#00441b', pch=17,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2018', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2018', 'avg_biom'],
       col = '#00441b', pch=15,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2019', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2019', 'avg_biom'],
       col = '#00441b', pch=19,type='o', cex=2, lwd=4)
mtext('Zooplankton Biomass', side = 2, line=3, cex=2)
mtext('Julian Day of Year', side = 1, line =3, cex = 2)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('2011', '2012', '2013','2014','2015','2016','2017','2018','2019'), 
       pch=c(17,15,19,17,15,19,17,15,19), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#66c2a4','#66c2a4','#66c2a4',
               '#238b45','#238b45','#238b45', 
               '#00441b','#00441b','#00441b'))
mtext("Year", at=0.2, cex=2)

gv2019 = zp_stoic_hist %>%
  filter(year == 2019)
gv2019
windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(gv2019$doy, gv2019$zp_np, type = 'o', pch=17, col='black', cex=3, cex.axis=2 , lwd=4, xlab='', ylab='') 
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Day of Year, 2019', side = 1, line =3, cex = 2)
legend('topright', legend=c('Zoop N:P'), col = c('black'), cex=1.5, pt.cex=3, pch=c(17))

# Historical Seston & Zoops
zp_stoic_hist
# Need to get historical seston 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
hist_alm = read_csv('Historical_ALM.csv')
hist_alm
# Select N and P variables + organic carbon
#Plot Zoop-Seston-Inorganic Stocks per week - Time Series (uM/L) ============================
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
         n_uML = n_storage*1000000/14010000, # Get uM/L 
         p_uML = p_storage*1000000/30970000) %>% 
  rename(group = group.x) %>% 
  select(sampleid, doy, taxon, group, biomass, n_uML, p_uML) %>%
  as_tibble()
zp_stoic2

# Get daily micromoles of N and P 
zp_uM_sum = zp_stoic2 %>% 
  group_by(sampleid, doy) %>%
  summarise(
    n_uML_zp = sum(n_uML), 
    p_uML_zp = sum(p_uML)) %>%
  ungroup() %>%
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
# Columns: doy, SRP_ugL, NOx_mgL orgP_ugL, orgN_ugL
gvl19_join = gvl19 %>%
  select(doy, SRP_ugL, NOx_mgL, orgP_ugL, orgN_mgL) %>%
  mutate(inorgP_uML= (SRP_ugL+0.01)*1000000/(1000000*30.97), 
         inorgN_uML= (NOx_mgL+0.01)*1000000/(1000*14.01), 
         orgP_uML= (orgP_ugL)*1000000/(1000000*30.97), 
         orgN_uML= (orgN_mgL)*1000000/(1000*14.01)) %>%
  select(doy, inorgP_uML, inorgN_uML, orgP_uML, orgN_uML) %>%
  as_tibble()
gvl19_join

gv_pools = left_join(gvl19_join, zp_uM_sum, by='doy')
gv_pools

gv_uML_stocks = gv_pools %>% 
  select(sampleid, doy, inorgN_uML, orgN_uML, n_uML_zp, inorgP_uML, orgP_uML, p_uML_zp) %>% # select useful columns
  mutate(sesP_uML = orgP_uML-p_uML_zp, 
         sesN_uML = orgN_uML-n_uML_zp) %>% # Calculate seston by taking organic pool and subtracting zoops to get seston
  select(sampleid, doy, inorgN_uML, sesN_uML, n_uML_zp, inorgP_uML, sesP_uML, p_uML_zp) %>% 
  as_tibble() 
gv_uML_stocks

# N uM/L 
# '#004ebe','#2a81ff', '#94c0ff'
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log(gv_uML_stocks$sesN_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#004ebe', ylim = c(log(0.5), log(420)), 
     ylab = 'Nitrogen concentration (umol N/L)', xlab = 'Day of Year, 2019', yaxt = 'n', cex.lab = 1.5, cex.axis = 1.5)
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', ''), las=2, cex.axis=1.5)
points(gv_uML_stocks$doy, log(gv_uML_stocks$inorgN_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#2a81ff')
points(gv_uML_stocks$doy, log(gv_uML_stocks$n_uML_zp+1), type = 'o', lwd=3, 
       pch = 19, col = '#94c0ff')

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Seston N', 'Inorganic N', 'Zooplankton N'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#004ebe','#2a81ff', '#94c0ff'))

# P uM/L 
# '#c24ad7', '#d786e4', '#ebc3f2'
windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
plot(gv_uML_stocks$doy, log(gv_uML_stocks$sesP_uML+1), type = 'o', lwd =3, 
     pch = 19, col = '#c24ad7', ylim = c(log(1), log(10)), 
     ylab = 'Phosphorus concentration (umol P/L)', xlab = 'Day of Year, 2019', yaxt = 'n', cex.lab = 1.5, cex.axis = 1.5)
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),
          log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100),
          log(200), log(300), log(400)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', ''), las=2, cex.axis=1.5)
points(gv_uML_stocks$doy, log(gv_uML_stocks$inorgP_uML+1), type = 'o', lwd=3, 
       pch = 19, col = '#d786e4')
points(gv_uML_stocks$doy, log(gv_uML_stocks$p_uML_zp+1), type = 'o', lwd=3, 
       pch = 19, col = '#ebc3f2')

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c( 'Seston P', 'Inorganic P', 'Zooplankton P'), 
       pch=19, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#c24ad7', '#d786e4', '#ebc3f2'))
