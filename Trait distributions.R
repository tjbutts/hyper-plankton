# Plankton Trait Distributions # 
rm(list=ls())

#load packages


library(tidyverse)
library(magrittr)

# Phytoplankton ------------------------------------
# Below are trait distributions of Green Valley phytoplankton 

# GALD ------------------------
# Read in dataset # 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton/GALD data")
gald <- read.csv('precomm_gald.csv')
as_tibble(gald)

# Load the vioplot library 
library(vioplot)
# Trim data to doy and GALD 
as_tibble(gald)
gald_dist <- gald %>%
  select(doy, gald,division) %>% 
  as_tibble()

# Draw the violin plot 
windows(height=5, width=8)
par(mai=c(0.5,1,0.1,1), omi=c(0.1,0.1,0,0))
span <- unique(gald_dist$doy)
with(gald_dist, vioplot(
  gald[doy=='143'], gald[doy=='150'], gald[doy=='157'],
  gald[doy=='164'], gald[doy=='172'], gald[doy=='178'],
  gald[doy=='192'], gald[doy=='199'], gald[doy=='206'],
  gald[doy=='213'], gald[doy=='220'], gald[doy=='227'],
  gald[doy=='245'], gald[doy=='251'],
  col='dodgerblue3', names = span, cex.axis=1.5
))
mtext(side=2, line=3,  'Greatest Axial Linear Distance (um)',  cex=1.5 )
mtext(side=1, line=3, 'Day of Year, 2019', cex=1.5)

# Draw jittered gald plot 
plot(gald~jitter(doy, 1), pch=20, data=gald_dist, xaxt='n')
axis(side=1, at = span) 
# Color individual data points 
## Create a new column filled with default color 
gald_dist$color = 'black' 
# Set new column values to appropriate colors 
gald_dist$color[gald_dist$division=='Cyanophyte']='#810f7c'
gald_dist$color[gald_dist$division=='Chlorophyte']='#8856a7'
gald_dist$color[gald_dist$division=='Chrysophyte']='#66c2a4'
gald_dist$color[gald_dist$division=='Cryptophyte']='#2ca25f'
gald_dist$color[gald_dist$division=='Bacillariophyte']='#006d2c'

# replot - log scale
windows(height=8, width=12)
par(mai=c(0.6,.5,0.1,.1), omi=c(0.3,0.8,0,0))
plot(log(gald)~jitter(doy, 1), pch=20, data=gald_dist, xaxt='n', yaxt='n', ylab='', xlab='', col=gald_dist$color, cex=1.5)
axis(side=1, at = span, cex.axis=1)
axis(side=2, cex.axis=1.5,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), log(200), log(300)), #Where the tick marks should be drawn
                  labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', ''), las=2, cex.axis=1.5)
mtext(side=1, line=3, 'Day of Year, 2019', font=2, cex=2)
mtext(side=2, line=3, 'Greatest Axial Linear Distance (um)', font=2, cex=2)
windows(height=5, width=5)
plot(1,1, col="white", xaxt='T', yaxt="T")
legend('center', legend=c('Cyanophyte', 'Chlorophyte', 'Chrysophyte', 'Crypophyte', 'Bacillariophyte'), pch=20, 
       col=c('#810f7c','#8856a7', '#66c2a4', '#2ca25f','#006d2c'), pt.cex=2, cex=2)

# replot - natural 
windows(height=8, width=12)
par(mai=c(0.6,.5,0.1,.1), omi=c(0.3,0.8,0,0))
plot(gald~jitter(doy, 1), pch=20, data=gald_dist, xaxt='n', col=gald_dist$color, cex=1.5, ylab='', xlab='', cex.axis=1)
axis(side=1, at = span, cex.axis=1)
mtext(side=1, line=3, 'Day of Year, 2019', font=2, cex=2)
mtext(side=2, line=3, 'Greatest Axial Linear Distance (um)', font=2, cex=2)



# Phytoplankton Qualitative Community Traits -------------------------------------------
# set wd to trait data 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton/Community data")
phytrait <- read.csv('phyto_taxon_trait.csv')
as_tibble(phytrait)

# Community Weighted Mean - Quantitative 
# Calculating community weighted mean using dplyr and tidyr functions 
summarize.cwm.quant <- # New datafram where we can inspect the result 
  phytrait %>% # First step in the next string of statements
  group_by(doy) %>% # Groups the summary file by Plot number 
  summarize(  # Coding for how we want our CWMs summarized)
    avg_gald_cwm = weighted.mean(avg_gald, biomass), #Actual calcualtion of CWMs 
    med_gald_cwm = weighted.mean(med_gald)) %>%
  ungroup() %>%
  as_tibble()
summarize.cwm.quant

# Calculating community weighted mean of binary trait value # 
## In this instance - community weighted mean is the biomass proportion of species possessing trait value 1, hence the 
## CWM will be a value between 0 and 1 
as_tibble(phytrait)
phytrait %>% modify_if(is.integer, as.character) -> phytrait
as_tibble(phytrait)

# Get sums for biomass for trait presence/abcense for each binary trait
summarize.binary.traits <- 
  phytrait %>%
  group_by(doy) %>% 
  summarize( 
    diazotrophy_sumtrait = aggregate(biomass, by = list(diazotrophy), FUN = 'sum'),
    heterocys_sumtrait = aggregate(biomass, by = list(heterocystous), FUN = 'sum'), 
    toxin_sumtrait = aggregate(biomass, by = list(toxin_production), FUN = 'sum'),
    motil_sumtrait = aggregate(biomass, by = list(motility), FUN = 'sum'), 
    buoyancy_sumtrait=aggregate(biomass, by = list(buoyancy), FUN = 'sum'), 
    mixotrophy_sumtrait=aggregate(biomass, by = list(mixotrophy), FUN = 'sum')) %>%
  ungroup() %>%
  as_tibble()
summarize.binary.traits

# Reformat data to be usable in excel (too complicated for my R skill level at the moment)
# Read data back in for cleaning 
phytrait_qual <- read.csv('phy_taxon_qualtrait.csv')
as_tibble(phytrait_qual)
qualclean <- phytrait_qual %>%
  pivot_longer(cols = c(diaz, heterocys, toxin, motil, buoy, mixo), 
               names_to = 'qual.trait', 
               values_to = 'biomass.prop' ) %>% 
  pivot_wider(names_from = bin.trait, values_from = biomass.prop) %>%
  rename(no = '0', yes = '1') %>%
  as_tibble()
qualclean

summarize.cwm.qual <- qualclean %>%
  group_by(doy, qual.trait) %>%
  summarize(prop = ((yes/sum(yes,no))*100)) %>%
  ungroup() %>%
  as_tibble()
summarize.cwm.qual

# Calculate the biomass proportion within each morphology based functional group
as_tibble(phytrait)
phy_mbfg <- phytrait %>%
  select(doy, taxon, group, biomass, mbfg,graze) %>%
  as_tibble() 
phy_mbfg

summarize.cwm.mbfg <- phy_mbfg %>%
  group_by(doy, mbfg) %>% 
  summarize(mbfg_sum = sum(biomass)) %>% 
  mutate(prop = mbfg_sum/sum(mbfg_sum)) %>% 
  ungroup() %>% 
  as_tibble()
summarize.cwm.mbfg

summarize.cwm.graze <- phy_mbfg %>%
  group_by(doy, graze) %>%
  summarize(graze_sum = sum(biomass)) %>%
  ungroup() %>%
  as_tibble()
summarize.cwm.graze 


# Community weighted Mean Datasets 
summarize.cwm.quant # Quantitative 
summarize.cwm.qual # Qualitative 
summarize.cwm.mbfg # Qualitative 
summarize.cwm.graze # Qualitative

# Plot community Weighted Mean 
# GALD CWM
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
plot(summarize.cwm.quant$doy, summarize.cwm.quant$avg_gald_cwm, type = "o", pch = 20, col='mediumseagreen', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="GALD Community Weighted Mean", lwd=4, ylim = c(0,17))
points(summarize.cwm.quant$doy, summarize.cwm.quant$med_gald_cwm, type='o', pch = 20, col='dodgerblue3', cex=3, lwd =4 )
legend('topleft', legend = c('average GALD', 'median GALD'), col=c('mediumseagreen', 'dodgerblue3'), pch =20, cex = 1.5)

# Qaulitative Traits - CWM 
#Diazotrophy 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
diaz <- summarize.cwm.qual %>% 
  filter(qual.trait == 'diaz')
plot(diaz$doy, diaz$prop, col = 'dodgerblue3', type='o', lwd=4, xlab='Day of Year, 2019', ylab='Proportion of Biomass - Diazotrophy', cex.axis=1.5, cex.lab=1.5)

#Buoyancy - just cyanos pretty much 
#windows(height=5, width=8)
#par(mai=c(0.9,1,0.6,1))
#buoy <- summarize.cwm.qual %>% 
#  filter(qual.trait == 'buoy')
#plot(buoy$doy, buoy$prop, col = 'dodgerblue3', type='o', lwd=4, xlab='Day of Year, 2019', ylab='Proportion of Biomass - Buoyancy', cex.axis=1.5, cex.lab=1.5)

#Mixotrophy 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
mixo <- summarize.cwm.qual %>% 
  filter(qual.trait == 'mixo')
plot(mixo$doy, mixo$prop, col = 'dodgerblue3', type='o', lwd=4, xlab='Day of Year, 2019', ylab='Proportion of Biomass - Mixotrophy', cex.axis=1.5, cex.lab=1.5)

#Motility 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
motil <- summarize.cwm.qual %>% 
  filter(qual.trait == 'motil')
plot(motil$doy, motil$prop, col = 'dodgerblue3', type='o', lwd=4, xlab='Day of Year, 2019', ylab='Proportion of Biomass - Flagellated', cex.axis=1.5, cex.lab=1.5)

#Toxin Producing 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
toxin <- summarize.cwm.qual %>% 
  filter(qual.trait == 'toxin')
plot(toxin$doy, toxin$prop, col = 'dodgerblue3', type='o', lwd=4, xlab='Day of Year, 2019', ylab='Proportion of Biomass - Toxins', cex.axis=1.5, cex.lab=1.5)

# Combined Qual Traits 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
plot(diaz$doy, diaz$prop, col = '#006d2c', type='o', xlab='Day of Year, 2019', 
     ylab='Proportion of Phytoplankton Biomass', cex.axis=1.5, cex.lab=1.5, ylim=c(0,100), lwd=4) # Diazotrophy
points(toxin$doy, toxin$prop, col = '#2ca25f', type='o', lwd=4) # Toxin production
points(mixo$doy, mixo$prop, col = '#66c2a4', type='o', lwd=4) # Mixotrophy 
legend('topleft', legend=c('N-fixing', 'Toxin producing', 'Mixotrophy'), col=c('#006d2c', '#2ca25f', '#66c2a4'), pch=20, cex=1.8, pt.cex=2)

library(viridis)
library(hrbrthemes)

windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
qual.stack <- summarize.cwm.qual %>% filter(!(qual.trait == 'buoy' | qual.trait == 'heterocys'))
ggplot(qual.stack, aes(x=doy, y=prop, fill=qual.trait)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  xlab('Day of Year, 2019') +
  ylab('Proportion of Phytoplankton Biomass') +
  theme(
    axis.title.x = element_text(size=14, face='bold'), 
    axis.title.y = element_text(size=14, face='bold')
  )

# Morphology Proportions 

# Plot


summarize.cwm.mbfg %<>% mutate(mbfg = as.factor(mbfg)) %>% as_tibble
morph <- summarize.cwm.mbfg

# Proportion of biomass in each group
windows(height=8, width=12)
par(mai=c(0.9,1,0.6,1))

one <- morph %>% filter(mbfg == 1)
two <- morph %>% filter(mbfg == 2)
three <- morph %>% filter(mbfg == 3)
four <- morph %>% filter(mbfg == 4)
five <- morph %>% filter(mbfg == 5)
six <- morph %>% filter(mbfg == 6)
seven <- morph %>% filter(mbfg == 7)

plot(one$doy, log(one$mbfg_sum+1), col = '#810f7c', type='o', lwd=4, xlab='Day of Year, 2019', 
     ylab='Respective Phytoplankton Biomass', yaxt='n', cex.axis=1.5, cex.lab=1.5, ylim = c(log(1), log(400))) # Type I
axis(side=2, cex.axis=1.5,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), log(200), log(300), log(400)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '',''), las=2, cex.axis=1.5)
     
points(two$doy, log(two$mbfg_sum+1), col = '#8856a7', type='o', lwd=4) # Type II
points(three$doy, log(three$mbfg_sum+1), col = '#8c96c6', type='o', lwd=4) # Type III
points(four$doy, log(four$mbfg_sum+1), col = '#b2e2e2', type='o', lwd=4) # Type IV
points(five$doy, log(five$mbfg_sum+1), col = '#66c2a4', type='o', lwd=4) # Type V
points(six$doy, log(six$mbfg_sum+1), col = '#2ca25f', type='o', lwd=4) # Type VI
points(seven$doy, log(seven$mbfg_sum+1), col = '#006d2c', type='o', lwd=4) # Type VII
windows(height=5, width=5)
plot(3,3, col="white", xaxt='T', yaxt="T")
legend("topleft", legend=c('I', 'II', 'III', 'IV', 'V', 'VI', 'VII'), pch=20, 
       col=c('#810f7c','#8856a7','#8c96c6','#b2e2e2', '#66c2a4', '#2ca25f','#006d2c'), pt.cex=2, cex=2)

# Grazing susceptibility 
graze <- summarize.cwm.grze 
high <- graze %>% filter(graze == 'high')
low <- graze %>% filter(graze == 'low')
medium <- graze %>% filter(graze == 'medium')

windows(height=8, width=12)
par(mai=c(0.9,1,0.6,1))
plot(low$doy, low$graze_sum, col = '#006d2c', type='o', lwd=4, xlab='Day of Year, 2019', 
     ylab='Grazing Susceptability', cex.axis=1.5, cex.lab=1.5, ylim = c(0,350))
points(medium$doy, medium$graze_sum, col = '#2ca25f', type='o', lwd=4)
points(high$doy, high$graze_sum, col = '#66c2a4', type='o', lwd=4 )
legend('topleft', legend=c('low susceptability', 'medium susceptability', 'high susceptability'), col=c('#006d2c', '#2ca25f', '#66c2a4'), pch=20, cex=2)



# Zooplankton --------------------------------------
# Below are the trait distributions of Green Valley zooplankton 
rm(list=ls())
# Begin zoop trait exploration
# Stoichiometry ---------------------------------- 
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
zp_cnp <- read.csv('zooplankton_cnp.csv')
as_tibble(zp_cnp)

zp_cnp %<>% rename(perc_c = X.C) %>%
  rename(perc_n = X.N) %>%
  rename(perc_p = X.P) %>%
  as_tibble()
zp_cnp %<>% select(taxon, group, perc_c, perc_n, perc_p) %>% as_tibble()
zp_cnp

# read in zp biomass 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")

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
  mutate(biomass = replace_na(biomass, 0)) %>%
  filter(!(doy == 162 | doy == 157 | doy == 211))
as_tibble(gv19_zp)

# Combine biomass with cnp data 
zp_stoic <- left_join(gv19_zp, zp_cnp, by='taxon')
as_tibble(zp_stoic)

# Multiply C, N, and P by biomass to get C, N, and P storage weighted by biomass by taxon
zp_stoic %<>% select(sampleid, doy, taxon, group.x, biomass, perc_c, perc_n, perc_p) %>%
  #mutate(perc_c = perc_c/100) %>%
  #mutate(perc_n = perc_n/100) %>%
  #mutate(perc_p = perc_p/100) %>%
  mutate(p_storage = perc_p*biomass) %>%
  mutate(n_storage = perc_n*biomass) %>% 
  mutate(c_storage = perc_c*biomass) %>% 
  mutate(n_molar = n_storage/14010000) %>%
  mutate(p_molar = p_storage/32000000) %>%
  select(sampleid, doy, taxon, group.x, biomass, n_molar, p_molar) %>%
  rename(group = group.x) %>%
  as_tibble
zp_stoic

# Get Zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum <- zp_stoic %>% 
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

# read in GV N:P # 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Cleaned Data")
gv_nut <- read.csv('nutrients_gvl_2019.csv')
gv_nut %<>% select(sampleID, doy, siteID, depthID, tp, tn, srp, nitrate) %>%
  filter(siteID == 4) %>%
  as_tibble()
gv_nut
#gv_nut[gv_nut == 0] <- NA
gv_nprat <- gv_nut %>%
  mutate(tn_ug = tn*1000) %>%
  mutate(nox_ug = nitrate*1000) %>%
  mutate(total_np = tn_ug/tp) %>%
  mutate(dissolved_np =nox_ug/srp) %>%
  as_tibble()
gv_nprat
gv_nprat[gv_nprat == Inf] <- 0

# Plot GV N:P then on a separate y-axis plot Zooplankton N:P 
windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))

# Dissolved fraction of N:P 
depth1 <- gv_nprat %>% select(sampleID, doy, depthID, dissolved_np) %>% filter(depthID == 1)
depth2 <- gv_nprat %>% select(sampleID, doy, depthID, dissolved_np) %>% filter(depthID == 2)
depth3 <- gv_nprat %>% select(sampleID, doy, depthID, dissolved_np) %>% filter(depthID == 3)
depth4 <- gv_nprat %>% select(sampleID, doy, depthID, dissolved_np) %>% filter(depthID == 4)

plot(depth1$doy, log(depth1$dissolved_np+1), type = 'o', pch=20, col='#008837', cex=3, cex.axis=1.5, cex.lab=1.5, xlab = 'Day of Year', ylab = 'Green Valley N:P', lwd = 2, ylim = c(0, log(1100+1)), yaxt='n')
axis(side=2,
     at=c(log(0.1), log(0.2), log(0.3), log(0.4), log(0.5), log(0.6), log(0.7), log(0.8), log(0.9), log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000), log(2000)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', '1', '', '','','','','','','','10','','','','','','','','','100', '', '', '', '', '', '', '', '', '1000', ''), las=2, cex.axis=1.5)
points(depth2$doy, log(depth2$dissolved_np+1), type = 'o', pch=20, col='#a6dba0', cex=3, cex.axis=1.5, cex.lab=1.5)
points(depth3$doy, log(depth3$dissolved_np+1), type = 'o', pch=20, col='#c2a5cf', cex=3, cex.axis=1.5, cex.lab=1.5)
points(depth4$doy, log(depth4$dissolved_np+1), type = 'o', pch=20, col='#7b3294', cex=3, cex.axis=1.5, cex.lab=1.5)
par(new = T)
plot(zp_stoic_sum$doy, zp_stoic_sum$zp_np, type = 'o', pch=17, col='black', cex=3, lwd=4, axes=F, xlab='', ylab='') 
axis(side=4, cex=3, cex.axis=1.5, cex.lab=1.5, at = c(16, 16.5, 17, 17.5, 18, 18.5, 19))
mtext('Zooplankton N:P', side = 4, line=3, cex=1.5)
legend('topright', legend=c('depth1', 'depth2', 'depth3', 'depth4', 'Zoop N:P'), col = c('#008837','#a6dba0','#c2a5cf','#7b3294', 'black'), cex=1.5, pt.cex=3, pch=c(20,20,20,20,17))

# Ingestion & Clearance Rates ------------------------------
# Calculate Zooplankton Density (#/L)
#set wd 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
zp_rawden <- read.csv('2019_site4_gv_ZoopDensity_25Feb2021.csv')
as_tibble(zp_rawden)

zp_rawden <- zp_rawden %>% mutate(INDV.L = replace_na(INDV.L, 0)) %>% as_tibble() 
zp_rawden
zp_raw2 <- zp_rawden %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID) %>%
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(density = INDV.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

gv19_zp_dens = zp_raw2 %>% as_tibble()
# Remove ALM dates as well as faulty 157 date 
gv19_zp_dens %<>% filter(!(doy == 162 | doy == 211 | doy == 157)) %>% as_tibble()

gv_totaldens <- gv19_zp_dens %>%
  select(sampleid, density,group, doy) %>%
  group_by(sampleid, doy) %>%
  summarise(
    totaldens = sum(density)) %>%
  ungroup()
as_tibble(gv_totaldens)

# Zooplankton Community by Density
gv19_zpcomm_dens <- gv19_zp_dens %>% #sum the group biomasses 
  group_by(sampleid,doy, group) %>%
  summarise( 
    totaldens= sum(density)) %>%
  ungroup()
as_tibble(gv19_zpcomm_dens)

gv19_zpcomm_dens_wide <- gv19_zp_dens %>% # Widen the dataframe
  group_by(sampleid,doy, group) %>%
  summarise( 
    totaldens= sum(density)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(sampleid, doy),
              names_from = group, 
              values_from = totaldens) %>% 
  as_tibble()
as_tibble(gv19_zpcomm_dens_wide)

# Density tibbles # 
gv19_zpcomm_dens

# Add Ingestion and Clearance rates to relevant taxa - Colina 
zp_candi <- summarize.cwm.mbfg %>% 
  select(doy, mbfg, prop) %>%
  as_tibble()
zp_candi

# read in clearance and ingestion rate data by mbfg
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
clear.ing <- read_csv('zoop_candi.csv')
clear.ing

# Combine Clearance rate and Ingestion data to zooplankton density data to get (ml/d) 

# Multiply the Clearance and Ingestion rate per mbfg by the percent of phytoplankton biomass belonging to that 
## mbfg per day 

# Sum Clearance and Ingestion rate per mbfg to get estimated clearance and ingestion rate 

# Length Distribution 
# Lengths --------------------------------------
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
length_dist <- read_csv('zoop_length.csv')
length_dist# length in micrometers 

# Draw the violin plot 
windows(height=5, width=8)
par(mai=c(0.5,1,0.1,1), omi=c(0.1,0.1,0,0))
span <- unique(length_dist$doy)
length_dist$doy <- as.integer(length_dist$doy)
length_dist
with(length_dist, vioplot(
  length[doy=='143'], length[doy=='150'],
  length[doy=='164'], length[doy=='171'], length[doy=='178'],
  length[doy=='192'], length[doy=='199'], length[doy=='206'],
  length[doy=='213'], length[doy=='220'], length[doy=='227'],
  length[doy=='234'],
  length[doy=='245'], length[doy=='251'], length[doy=='273'],
  col='dodgerblue3', names = span, cex.axis=1.5
))
mtext(side=2, line=3,  'Zooplankton length (um)',  cex=1.5 )
mtext(side=1, line=3, 'Day of Year, 2019', cex=1.5) 

# Draw jittered length plot 
plot(length~jitter(doy, 1), pch=20, data=length_dist, xaxt='n')
axis(side=1, at = span) 
# Color individual data points 
## Create a new column filled with default color 
length_dist$color = 'black' 
# Set new column values to appropriate colors 
length_dist$color[length_dist$group=='SmCladocera']='#810f7c'
length_dist$color[length_dist$group=='LgCladocera']='#8856a7'
length_dist$color[length_dist$group=='Calanoida']='#8c96c6'
length_dist$color[length_dist$group=='Cyclopoida']='#b2e2e2'
length_dist$color[length_dist$group=='Nauplii'] = '#66c2a4'
length_dist$color[length_dist$group=='Rotifera']='#2ca25f'
length_dist$color[length_dist$group=='Ostracoda']='#006d2c'

# replot - log scale
windows(height=8, width=12)
par(mai=c(0.6,.5,0.1,.1), omi=c(0.3,1,0,0))
plot(log(length)~jitter(doy, 1), pch=20, data=length_dist, xaxt='n', yaxt='n', ylab='', xlab='', col=length_dist$color, cex=1.5)
axis(side=1, at = span, cex.axis=1)
axis(side=2, cex.axis=1.5,
     at=c(log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), log(200), log(300), log(400), log(500), log(600), log(700), log(800), log(900), log(1000), log(2000)), #Where the tick marks should be drawn
     labels = c('10','','','','','','','','','100', '', '', '', '','','','','','1000',''), las=2, cex.axis=1.5)
mtext(side=1, line=3, 'Day of Year, 2019', font=2, cex=2)
mtext(side=2, line=3, 'Zooplankton Length (um)', font=2, cex=2)
windows(height=5, width=5)
plot(1,1, col="white", xaxt='T', yaxt="T")
legend("topleft", legend=c('Small Cladocerans', 'Large Cladocerans', 'Calanoida', 'Cyclopoida', 'Nauplii', 'Rotifera', 'Ostracoda'), pch=20, 
       col=c('#810f7c','#8856a7','#8c96c6','#b2e2e2', '#66c2a4', '#2ca25f','#006d2c'), pt.cex=2, cex=2)

# replot - normal
windows(height=8, width=12)
par(mai=c(0.6,.5,0.1,.1), omi=c(0.3,1,0,0))
plot(length~jitter(doy, 1), pch=20, data=length_dist, xaxt='n', ylab='', xlab='', col=length_dist$color, cex=1.5, cex.axis=1)
axis(side=1, at = span, cex.axis=1)
mtext(side=1, line=3, 'Day of Year, 2019', font=2, cex=2)
mtext(side=2, line=3, 'Zooplankton Length (um)', font=2, cex=2)
windows(height=5, width=5)
plot(1,1, col="white", xaxt='T', yaxt="T")
legend("topleft", legend=c('Small Cladocerans', 'Large Cladocerans', 'Calanoida', 'Cyclopoida', 'Nauplii', 'Rotifera', 'Ostracoda'), pch=20, 
       col=c('#810f7c','#8856a7','#8c96c6','#b2e2e2', '#66c2a4', '#2ca25f','#006d2c'), pt.cex=2, cex=2)





