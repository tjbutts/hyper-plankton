# Load in dry mass that takes an average dry biomass from a list of lengths # ========================
rm(list=ls())
library(tidyverse)
# Find average dry biomass per taxa 
# Calculate excretion rate (nmol of N or P per indv per hr) 
# Multiply by species density (# of indv/L to get nmol of N or P per L per hr) 
# Multiply by 24 to get daily rate  (nmol of N or P per L per d)
# Convert to umol of N or P per L per d
# ===Comparison to Lake Erie Values=== # 
# Get Areal rate by multiplying by Tow Depth 
# Convert umol of N or P to mg N or P 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")

# Turn raw R output into cleaned zoop data
zp_raw = read.csv('2019_site4_gv_Zoopdry_19Apr2021.csv')
zp_raw2 = zp_raw %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID,
         taxon = TAXON,
         lakeno = LAKE.NO,
         dry_biomass = BIOMASS.UG,
         group = GROUP,
         doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 

gv19_DM = zp_raw2 %>% 
  select(sampleid,doy, taxon, group, dry_biomass) %>% 
  mutate(dry_biomass = replace_na(dry_biomass, 0)) %>% # Replace NAs with 0s 
  filter(!(doy == 162 | doy == 157)) %>% # Remove the ALM sampling dates plus DOY 157 which we determined was wonky
  as_tibble()
gv19_DM

# Calculate excretion rates with the average dry biomass per taxa 
gv19_excretion_rate = gv19_DM %>% 
  mutate(dm_mg = dry_biomass/1000) %>% # Convert biomass from ug to mg to fit regression equation parameters 
  mutate(ln_N_NH4_excretion = 0.84*log(dm_mg)+2.50) %>% # nmol N per indv. per hour 
  mutate(ln_P_PO4_excretion = 0.70*log(dm_mg)+0.56) %>% # nmol P per indv. per hour 
  mutate(N_excrete = exp(ln_N_NH4_excretion)) %>%
  mutate(P_excrete = exp(ln_P_PO4_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  as_tibble()
gv19_excretion_rate

# check bounds of zooplankton 
#dry mass 
(max(gv19_excretion_rate$dm_mg)) # 0.0212
(min(gv19_excretion_rate[gv19_excretion_rate$dm_mg>0,'dm_mg'])) #-13.54
#excretion rate 
(max(gv19_excretion_rate$N_excrete)) #-0.7370516
(min(gv19_excretion_rate[gv19_excretion_rate$N_excrete>0, 'N_excrete'])) #-8.8766
(max(gv19_excretion_rate$P_excrete)) #-2.137534 
(min(gv19_excretion_rate[gv19_excretion_rate$P_excrete>0, 'P_excrete'])) #-8.92051

# check dry mass bound in ug of zooplankton 
(max(gv19_excretion_rate$dm_mg))*1000 # 21.203
(min(gv19_excretion_rate[gv19_excretion_rate$dm_mg>0,'dm_mg']))*1000 # 0.00131
# Wen and Peters 1994 dry mass bounds 
# 0.13534 - 2980.958 


#Dry Mass
# P Bound = -7 to 1 (Hebert et al. 2016)
# N Bound = -8 to 4 (Hebert et al. 2016)
#Excretion 
# P Bound = -6 to 2 (Hebert et al. 2016)
# N Bound = -4 to 4 (Hebert et al. 2016)


# Multiply excretion rates by volume corrected density to get nmol N or P per L per hour 
# set wd to density datasheet 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
gv19_dens = read.csv('2019_site4_gv_ZoopDensity_25Feb2021.csv') # Density data sheet 
gv19_dens = gv19_dens %>% filter(!(DOY == 162 | DOY == 157)) %>% rename(doy = DOY, 
                                                                        sampleid = SAMPLE.ID, 
                                                                        taxon = TAXON, 
                                                                        group = GROUP, 
                                                                        dens = INDV.L) %>%
  mutate(dens = replace_na(dens, 0)) %>%
  as_tibble()
gv19_dens

# Join excretion rate data to density data 
gv19_join2 = left_join(gv19_excretion_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
gv19_join2
gv19_excretion_rate_daily = gv19_join2 %>% 
  select(sampleid, doy, taxon, group.x, dm_mg, N_excrete, P_excrete, dens) %>% #Select relevant columns 
  rename(group = group.x) %>%
  mutate(Nexcretion_denscorrected = N_excrete*dens, 
         Pexcretion_denscorrected = P_excrete*dens) %>% # get nmol of N or P per L per hour 
  mutate(Nexcretion_daily_nmol = Nexcretion_denscorrected*24, 
         Pexcretion_daily_nmol = Pexcretion_denscorrected*24) %>% # get nmol of N or P per L per day
  mutate(Nexcretion_daily_umol = Nexcretion_daily_nmol/1000, 
         Pexcretion_daily_umol = Pexcretion_daily_nmol/1000) %>% # get umol of N or P per L per day 
  as_tibble()
gv19_excretion_rate_daily  

# Zoop community excretion 
gv19_zpcomm_excretionrate = gv19_excretion_rate_daily %>% 
  select(sampleid, doy, taxon, group, dm_mg, Nexcretion_daily_nmol, Pexcretion_daily_nmol, Nexcretion_daily_umol, Pexcretion_daily_umol) %>%
  group_by(sampleid, doy) %>% 
  summarise(N_comm_excretion_umol = sum(Nexcretion_daily_umol), 
            P_comm_excretion_umol = sum(Pexcretion_daily_umol),
            N_comm_excretion_nmol = sum(Nexcretion_daily_nmol),
            P_comm_excretion_nmol = sum(Pexcretion_daily_nmol)) %>%
  as_tibble()
gv19_zpcomm_excretionrate # nmol is a little better units 

# Areal daily excretion rates (mg per m^2 per day) 
gv19_arealexc_join = gv19_zpcomm_excretionrate %>% 
  select(sampleid, doy, N_comm_excretion_umol, P_comm_excretion_umol) %>%
  mutate(N_mol = N_comm_excretion_umol/1000000, # N mol per L per day 
         P_mol = P_comm_excretion_umol/1000000, 
         N_ug = N_mol*14010000, # N ug per L per day 
         P_ug = P_mol*30970000, 
         N_m3 = N_ug*1000, # N ug per m^3 per day 
         P_m3 = P_ug*1000, 
         N_m3_mg = N_m3/1000, # N mg per m^3 per day 
         P_m3_mg = P_m3/1000) %>%
  select(sampleid, doy, N_mol, P_mol, N_ug, P_ug, N_m3, P_m3, N_m3_mg, P_m3_mg) %>%
  as_tibble()
gv19_arealexc_join
# set wd to tow depth measurements 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton") 
zvol = read.csv('gv_zp_log_site4.csv')

tow = zvol %>% 
  rename(tow = TOW, 
         sampleid = SAMPLEID) %>%
  select(sampleid, doy, tow) %>%
  as_tibble()
tow  

gv19_exctow_join = left_join(gv19_arealexc_join, tow, by = 'doy')

gv19_arealexc = gv19_exctow_join %>%
  rename(sampleid =sampleid.x) %>%
  select(sampleid, doy, N_m3_mg, P_m3_mg, tow) %>%
  mutate(N_areal = N_m3_mg*tow, 
         P_areal = P_m3_mg*tow) %>%
  as_tibble()
gv19_arealexc # mg per square meter per day 

