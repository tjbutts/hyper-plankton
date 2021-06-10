# GV fig 5 - Density Ridgeline Plots: GALD, RUEn, RUEp, Volume (TBD), zoop bodymass # 
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(ggridges)

# GALD ======================

# Want all GALD measurements - cumulative per DOY for Cyanophyta and non-Cyanophyta over the summer 
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")

gald = read_csv('precomm_gald.csv')
gald

# recode to Cyanophyte v. non Cyanophyte 
unique(gald$division)
gald$division <- gsub('Bacillariophyte', 'Non-Cyanophyte', gald$division)
gald$division <- gsub('Chlorophyte', 'Non-Cyanophyte', gald$division)
gald$division <- gsub('Cryptophyte', 'Non-Cyanophyte', gald$division)
gald$division <- gsub('Chrysophyte', 'Non-Cyanophyte', gald$division)
gald$division <- gsub('Euglenophyte', 'Non-Cyanophyte', gald$division)

unique(gald$division)

# Get doy, division, gald  
gald_plot = gald %>%
  select(doy, division, gald) %>%
  as_tibble()
gald_plot

doy = c(211, 211, 211, 234, 234,234, 273, 273,273)
division = c('na', 'na', 'na', 'na', 'na', 'na', 'na', 'na', 'na')
gald = c(0, 0, 0, 0, 0, 0, 0, 0, 0)
miss = data.frame(doy, division, gald)
str(miss) 
miss

gald_plot2 = gald_plot %>%
  rbind(miss) %>% 
  arrange(doy) %>%
  mutate(doy = as.factor(doy))
gald_plot2

gald_cyano = gald_plot %>%
  filter(division == 'Cyanophyte') %>% 
  mutate(doy = as.factor(doy))

ggplot(
  gald_cyano, 
  aes(x=gald, y = fct_rev(doy), height = ..density.. ,fill = stat(x))
) + 
  geom_density_ridges_gradient(stat = 'density', trim = T, scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "GALD", option = "C") +
  labs(title = 'Cyanophyta Greatest Axial Linear Distance')


gald_ncyano = gald_plot %>%
  filter(division == 'Non-Cyanophyte') %>% 
  mutate(doy = as.factor(doy))

ggplot(
  gald_ncyano, 
  aes(x=gald, y = fct_rev(doy), height = ..density.. ,fill = stat(x))
) + 
  geom_density_ridges_gradient(stat = 'density', trim = T, scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "GALD", option = "C") +
  labs(title = 'Cyanophyta Greatest Axial Linear Distance')

# All phyto GALDs together since non-cyanophyte is very sparse and pretty much between 1-25 um whenever they show up 


windows(height= 6, width = 8)
par(mai=c(0.9,1,0.6,1))

# average GALD
ggplot(
  gald_plot, 
  aes(x=log10(gald+1), y = fct_rev(doy), height = ..density.. ,fill = stat(x))
) + 
  geom_density_ridges_gradient(stat = 'density', trim = T, scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "GALD", option = "C") +
  labs(title = 'Phytoplankton Greatest Axial Linear Distance (GALD)') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size = 12, face = 'bold', colour = 'black')) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

# add in missing days 
windows(height= 8, width = 6)
par(mai=c(0.9,1,0.6,1))
ggplot(
  gald_plot2, 
  aes(x=log10(gald+1), y = fct_rev(doy), height = ..density.. ,fill = stat(x))
) + 
  geom_density_ridges_gradient(stat = 'density', trim = T, scale = 3, size = 0.3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "GALD", option = "C") +
  labs(title = 'Phytoplankton Greatest Axial Linear Distance (GALD)') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size = 12, face = 'bold', colour = 'black')) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

# Zooplankton 
setwd("C:/Users/Tyler/Box Sync/Active/Active GV Research/Trait compilation")

lengths = read_csv('zoop_length.csv')
lengths_171 = lengths %>% select(doy, length) %>% filter(doy == 171)
doy2 = 172
lengths_172 = lengths_171 %>% cbind(doy2) %>% select(length, doy2) %>% rename(doy = doy2)
length_range = lengths %>% select(doy, length) 

doy = c(157, 157, 157, 211, 211, 211) 
length = c(0,0,0, 0,0,0)
miss2 = data.frame(doy, length)
length_range2 = length_range %>% rbind(miss2) %>% arrange(doy) %>% mutate(doy = as.factor(doy))
length_range2

windows(height= 8, width = 6)
par(mai=c(0.9,1,0.6,1))
ggplot(
  length_range2, 
  aes(x=log10(length+1), y = fct_rev(doy), height = ..density.. ,fill = stat(x))
) + 
  geom_density_ridges_gradient(stat = 'density', trim = T, scale = 3, size = 0.3, rel_min_height = 0.15) +
  xlim(1.5, 3.5) +
  scale_fill_viridis_c(name = "Length", option = "C") +
  labs(title = 'Zooplankton Length') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size = 12, face = 'bold', colour = 'black')) + 
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank()) 

# RUE #=================================
# Phytoplankton ==============================

# read in datasets 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Cleaned Data")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Final Data/Cleaned Data")
met = read.csv("met_gvl_2019_MatchEXOdoy.csv") #meteorological data from Creston station 
hf = read.csv("high_frequency_EXO3_gvl_2019.csv") #EXO sonde data
buoy = read.csv("Green-Valley_meta-thermo-buoy.csv") #buoyancy frequency data

setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Plankton Data/phytoplankton")

prephy <- read.csv('pre_phyto_comm.csv') # pre-incubation phytoplankton community 
postphy <- read.csv('post_phyto_comm.csv') # post-incubation phytoplankton community 

setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton")
setwd("C:/Users/Tyler/Box Sync/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton")
gv19_phy <- read.csv('GVL_2019_Phyto_Summary.csv')
as_tibble(gv19_phy)

# Total biomass - filtered to pre 
gv_totalbiom <- gv19_phy %>%
  select(TAXON, BIOMASS.MG.L, Day, Treatment) %>%
  group_by(Day, Treatment) %>%
  summarise(
    totalbiom = sum(BIOMASS.MG.L)) %>%
  ungroup()
as_tibble(gv_totalbiom)

pre_totalbiom <- gv_totalbiom %>%
  filter(Treatment == "Pre")
pre_totalbiom 


# Read in inorganic nutrient data 
# Follow Filstrup et al. 2014 - Resources available to the trophic level TP and TN organic + inorganic 
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

# select inorganic N and P 
gvl19
gvl19$doy_alt = c(143, 150, 157, 164, 172, 178, 192, 199, 206, 211, 213, 220, 227, 234, 245, 251, 273)
gvl19 = gvl19 %>% select(!(doy)) %>% rename(doy = doy_alt)

rue_nutrients = gvl19 %>%
  select(doy, TP_ugL, TN_mgL, NOx_mgL, SRP_ugL)
rue_nutrients 

rue_phy = pre_totalbiom %>%
  select(Day, totalbiom) %>%
  rename(doy = Day)
rue_phy

rue_join = left_join(rue_phy, rue_nutrients, by = 'doy')
rue_join

rue = rue_join %>%
  mutate(rue_n = totalbiom/NOx_mgL, 
         rue_p = (totalbiom*1000)/SRP_ugL) %>% 
  mutate(rue_tn = totalbiom/TN_mgL, 
         rue_tp = (totalbiom*1000)/TP_ugL) %>%
  mutate(log_ruen = log10(rue_n+1), 
         log_ruep = log10(rue_p+1)) %>%
  mutate(log_ruetn = log10(rue_tn+1), 
         log_ruetp = log10(rue_tp+1))
rue

# Plot RUE N and P using 
rue_plot = rue %>% select(doy, log_ruetn, log_ruetp) 

windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1))

rue_plot
doy = c(211, 234, 273)
log_ruetn = c(0,0,0) 
log_ruetp = c(0,0,0)
miss = data.frame(doy, log_ruetn, log_ruetp)

rue_tot = rue_plot %>%
  rbind(miss) %>%
  arrange(doy) %>%
  as.data.frame()
rue_tot

ruep = rue_tot %>%
  select(doy, log_ruetp) %>%
  pivot_wider(names_from = doy, 
              values_from = log_ruetp) %>%
  as.data.frame()

ruep
row.names(ruep) <- 'p'
ruep.m = as.matrix(ruep)
ruep.m

windows(height= 6, width = 8)
par(mai=c(0.9,1,0.6,1))

barplot(ruep.m,
        col = 'orchid2', space = 0.04,
        border = 'black', font.axis=2, las=2, yaxt = 'n', ylim = c(log10(1), log10(2000)))
box()
mtext('Log10(RUE - Phosphorus)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3, cex=1.5)
axis(side=2,
     at=c(log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000), log(2000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000','2000'), las=2, font = 2)
abline(h = log10(100), lwd = 3)

windows(height= 6, width = 8)
par(mai=c(0.9,1,0.6,1))
ruen = rue_tot %>%
  select(doy, log_ruetn) %>%
  pivot_wider(names_from = doy, 
              values_from = log_ruetn) %>%
  as.data.frame()

ruen
row.names(ruen) <- 'n'
ruen.m = as.matrix(ruen)
ruen.m

barplot(ruen.m,
        col = 'dodgerblue2', space = 0.04,
        border = 'black', font.axis=2, las=2, yaxt = 'n', ylim = c(log10(1), log10(2000)))
box()
mtext('Log10(RUE - Nitrogen)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)
axis(side=2,
     at=c(log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000), log(2000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000','2000'), las=2, font = 2)
abline(h = log10(100), lwd = 3)
