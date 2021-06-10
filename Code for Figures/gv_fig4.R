# GV fig 4 - db-RDA phytoplankton composition v. environmental variables (including zooplankton-relevant ones)
rm(list=ls())

# Libraries
library(tidyverse)
library(magrittr)
library(purrr)
library(forcats)
library(vegan)

# Community Data ====================

#species data 
setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Plankton Data/phytoplankton")

prephy <- read.csv('pre_phyto_comm.csv') # pre-incubation phytoplankton community 
prephy <- as_tibble(prephy)
prephy %<>% rename(doy = X)
prephy %<>% modify_if(is.character, as.factor)
prephy
colSums(prephy[,2:33])

min(prephy[prephy > 0]) # minimum value is 0.0034 mg/L 
prephy = prephy %>% 
  filter(!(doy == 157))
species = as.data.frame(prephy[,2:33])
row.names(species) = prephy$doy 
species

species.hell = decostand(species, method = 'hellinger') # Hellinger transform species data 
species.hell

# Environmental Data =========================
## Explanatory: Inorg N, Inorg P, Zoop excretion N, Zoop excretion P, Zoop N:P, temp, pH, total dissolved solids, buoyancy-frequency
# read in necessary data sets to start splicing 

## inorganic nutrients & Excretion Rates ===================
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets") #desktop
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")

gv_exc = read_csv('gv19_excretion.csv')
gv_stocks = read_csv('gv19_nutrientstocks.csv')
gv_exc %<>% filter(!c(doy == 211 | doy == 234 | doy == 273)) # Match missing in zoop and phyto data 
gv_stocks %<>% filter(!c(doy ==211 | doy == 234 | doy == 273 | doy == 157)) # Match missing in zoop and phyto data 

inorg_P = gv_stocks$SRP_ugL
inorg_N = gv_stocks$NOx_mgL

excrete_P = gv_exc$P_excrete_sum_d
excrete_N = gv_exc$N_excrete_sum_d

## Zooplankton N:P =====================
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

zp_stoic_sum %<>% filter(!c(doy ==234 | doy == 273)) # remove DOY 234 and 273 to match phyto data 
zoop_np = zp_stoic_sum$zp_np

## Environmental Variables from EXO (Need to Match with Sample dates) =================================
# Sample Dates  <- c(143, 150, 164, 172, 178, 192, 199, 206,213,220, 227, 245, 251) 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Cleaned Data")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Final Data/Cleaned Data")


hf = read_csv("high_frequency_EXO3_gvl_2019.csv") #EXO sonde data
buoy = read_csv("Green-Valley_meta-thermo-buoy.csv") #buoyancy frequency data


# need to filter for dates 
library(lubridate)
buoy$dttm = mdy_hm(buoy$date_time) # Convert column to date-time code 
buoy

buoy$yday = yday(buoy$dttm) # Get Juilian Day of Year
buoy

buoy_select = buoy %>% 
  select(yday, buoyancy_frequency) %>%
  filter(yday == 143 | 
           yday == 150 | 
           yday == 164 | 
           yday == 172 | 
           yday == 178 | 
           yday == 192 | 
           yday == 199 | 
           yday == 206 |
           yday == 213 | 
           yday == 220 | 
           yday == 227 | 
           yday == 245 | 
           yday == 251) %>% 
  group_by(yday) %>%
  summarise(avg_buoy = mean(buoyancy_frequency, na.rm =T)) 
buoy_select

buoy_var = buoy_select$avg_buoy

# hf 
hf_select = hf %>% 
  select(doy, temp, ph, tds) %>%
  filter(doy == 143 | 
           doy == 150 | 
           doy == 164 | 
           doy == 172 | 
           doy == 178 | 
           doy == 192 | 
           doy == 199 | 
           doy == 206 |
           doy == 213 | 
           doy == 220 | 
           doy == 227 | 
           doy == 245 | 
           doy == 251) %>%
  group_by(doy) %>%
  summarise(avg_temp = mean(temp, na.rm =T), 
            avg_ph = mean(ph, na.rm = T), 
            avg_tds = mean(tds, na.rm = T))
hf_select 
hf_var = c(hf_select$doy, hf_select$avg_temp, hf_select$avg_ph, hf_select$avg_tds)
temp = hf_select$avg_temp 
ph = hf_select$avg_ph
tds = hf_select$avg_tds

# Make data frame of explanatory variables ============================ 
# Z-score data to standardize around mean of zero and sd of 1 since data is dimensionally nonhomegenous 
doy = c(143, 150, 164, 172, 178, 192, 199, 206,213,220, 227, 245, 251)
inorg_P = gv_stocks$SRP_ugL
`inorganic P` = ((inorg_P - mean(inorg_P, na.rm = T))/sd(inorg_P, na.rm = T))
inorg_N = gv_stocks$NOx_mgL
`inorganic N` = ((inorg_N - mean(inorg_N, na.rm = T))/sd(inorg_N, na.rm = T))
excrete_P = gv_exc$P_excrete_sum_d
`P excretion` = ((excrete_P - mean(excrete_P, na.rm = T))/sd(excrete_P, na.rm = T))
excrete_N = gv_exc$N_excrete_sum_d
`N excretion` = ((excrete_N - mean(excrete_N, na.rm = T))/sd(excrete_N, na.rm = T))
zoop_np = zp_stoic_sum$zp_np
`zoop N:P` = ((zoop_np - mean(zoop_np, na.rm = T))/sd(zoop_np, na.rm = T))
temp = hf_select$avg_temp 
`temp` = ((temp - mean(temp, na.rm = T))/sd(temp, na.rm = T))
ph = hf_select$avg_ph
`pH` = ((ph - mean(ph, na.rm = T))/sd(ph, na.rm = T))
tds = hf_select$avg_tds
`TDS` = ((tds - mean(tds))/(sd(tds)))
buoy_var = buoy_select$avg_buoy
buoyz = (buoy_var - mean(buoy_var))/(sd(buoy_var))

env = data.frame(doy, `inorganic P`, `inorganic N`, `P excretion`, `N excretion`, `zoop N:P`, 
                 `temp`, `pH` , `TDS`)
env

# Run db-RDA of phytoplankton community composition =============================
# call species and explanatory variable datasets 
species.hell
str(species.hell)
env_exp = env[,2:9]
row.names(env_exp) = env$doy
str(env_exp)

species.hell # Hellinger-transformed phytoplankton biomass data (reduces significance of rare species)
env_exp # Z-scored environmental explanatory variables to maintain dimensional homogeneity (Legendre & Legendre 1998) 

# Choose a distance matrix - do this by ranking correlations between dissimilarity indices and gradient seperation 
## The higher the value the better 
rankindex(env_exp, species.hell, indices = c('euc', 'man', 'gow', 'bra', 'kul'), stepacross = F, method = 'spearman')

# Bray-Curtis is the greatest will use that (the higher the better)
# Run RDA taking an average of the last data before and after DOY 245 to see if NAs are screwing w/ df 
test_P = env_exp %>%
  select(inorganic.P)
test_N = env_exp %>%
  select(inorganic.N)

test_P
mean(1.12980565, 1.26280282)
test_P = test_P %>% mutate(inorganic.P = replace_na(inorganic.P, 1.129806))
test_P

test_N
mean(-0.5163036, -0.8260857)
test_N = test_N %>% mutate(inorganic.N = replace_na(inorganic.N, -0.5163036))
test_N

env_exp2 = env_exp %>%
  cbind(test_P) %>%
  cbind(test_N)
env_exp2 = env_exp2[,3:10]
env_exp2

rankindex(env_exp2, species.hell, indices = c('euc', 'man', 'gow', 'bra', 'kul'), stepacross = F, method = 'spearman')


# Run stepwise regression (backword and forward to attain the best model) 
library(MASS)
# fit the full model 
env_exp
full = capscale(species.hell~., data = env_exp, na.action = na.omit)

# Stepwise regression model to assess multicollinearity  
step.model <- stepAIC(full, direction = 'both', trace = FALSE)


# Is the model significant? 
anova.cca(step.model)
anova.cca(step.model, by = 'axis', perm.max=999)# test axes for significance 
anova.cca(step.model, by = 'terms', permu = 999)# test for significance of explanatory variables 

# summary 
summary(step.model)

# plot model results 
windows(height=6, width=9)
par(mai=c(1,1.1,.6,.6))
plot(step.model)

# Diagnostics 
ordiresids(step.model, kind = 'residuals')
ordiresids(step.model, kind = 'qqmath')

# Run db-RDA on phytoplankton functional traits (Need to figure out still) ===================================
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")

gv_phytos_traits = read_csv('phyto_taxon_trait.csv')
gv_phytos_traits
gv_phytos_traits %<>% arrange(doy)
gv_phytrait = gv_phytos_traits %>% filter(doy == 143) %>% # Get traits that will apply for whole set
  select(!c(doy, group, biomass, avg_gald,med_gald))
gv_phytrait.clean = gv_phytrait %>%
  mutate(taxon = factor(taxon, levels = c('Aphanizomenon',
                                          'Aphanocapsa',
                                          'Aphanothece',
                                          'Asterionella',
                                          'Chalmydomonas',
                                          'Coelastrum',
                                          'Cosmarium',
                                          'Cryptomonas',
                                          'Desmodesmus',
                                          'Dolichospermum',
                                          'Elakatothrix',
                                          'Eudorina',
                                          'Euglena',
                                          'Fragilaria',
                                          'Komma',
                                          'Mallomonas',
                                          'Merismopedia',
                                          'Microcystis',
                                          'Microcystis_Single',
                                          'Oocystis',
                                          'Pediastrum',
                                          'Planktolyngbya',
                                          'Pseudanabaena',
                                          'Schroederia',
                                          'Snowella',
                                          'Staurastrum',
                                          'Stephanodiscus',
                                          'Trachelomonas',
                                          'Unknown.centric.chlorophyte',
                                          'Unknown.centric.bacillariophyte',
                                          'Unknwon.pennate.bacillariophyte',
                                          'Woronichinia'))) %>%
  arrange(taxon)
gv_phytrait.clean
unique(gv_phytrait.clean$taxon)
gv_phytrait.m = as.matrix(gv_phytrait.clean[,2:11])
row.names(gv_phytrait.m) = gv_phytrait.clean$taxon
gv_phytrait.m

str(gv_phytrait.m)

trait = as.data.frame(gv_phytrait.m) 
trait # trait data
env_exp2 # Z-scored explanatory variables 

# For trait data - need the community weighted mean of each trait per DOY; 
## for binary get biomass of species within 1 of trait
# Will be CWM of each functional trait - then the 'species composition' is effectively 'trait composition'
library(data.table)

quant.trait = trait %>% 
  select(p_affin, max_growth, edibility)
  
quant.trait = setDT(quant.trait, keep.rownames = 'taxon')[]

bin.trait = trait %>%
  select(!c(p_affin, max_growth, edibility))

bin.trait = setDT(bin.trait, keep.rownames = 'taxon')[] # add GALD threshold >35 as a binary trait 

# attach on GALD trait to quant.trait (need to fix base data leave out for now) 
'setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")

gald = read_csv('precomm_gald.csv') 
gald
gald.trait = gald %>% 
  rename(taxon = genus) %>% 
  select(doy, taxon, gald) %>% 
  group_by(taxon, doy) %>% 
  summarise(gald = mean(gald)) %>%
  ungroup() %>% 
  arrange(taxon, doy) 
gald.trait'

# join those trait dataframes to a long form phytoplankton biomass data set with left_join() 
# for quant.trait calculate commmunity weighted mean (biomass-weighted mean trait value (or mean cell size))
# for bin.trait calculate the proportion of biomass within 1 (biomass proportion of species possessing the trait value 1)

phy_long = prephy %>%
  pivot_longer(cols = !(doy), names_to = 'taxon', values_to ='biomass')
phy_long

quant.trait.join = left_join(phy_long, quant.trait, by = 'taxon')
quant.trait.join

quant.trait.cwm = quant.trait.join %>%
  group_by(doy) %>% 
  summarise(
    p_affin.cwm = weighted.mean(p_affin, biomass), 
    max_growth.cwm = weighted.mean(max_growth, biomass), 
    edibility.cwm = weighted.mean(edibility, biomass)) %>%
  ungroup()
quant.trait.cwm # community weighted mean by phytoplankton biomass 

bin.trait.join = left_join(phy_long, bin.trait, by = 'taxon')
bin.trait.join




dbRDA_trait = capscale(trait ~ inorganic.P+inorganic.N+zoop.P.excretion+zoop.N.excretion+zoop.N.P+temp+pH+TDS, env_exp2, dist='bray')
# plot results 
windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
plot(dbRDA, xlim=c(-2,2))
# Is model significant? 
anova(dbRDA)

# What is signiicant? 
anova(dbRDA) # overall test of the significance of the analysis 
anova(dbRDA, by = 'axis', perm.max=999) # test axes for significance 
anova(dbRDA, by = 'terms', permu=999) # test for sign. environ. variables 

# Diagnostics 
ordiresids(dbRDA, kind = 'residuals')
ordiresids(dbRDA, kind = 'qqmath')

