# Wen and Peters 1994 - regression (more direct comparison to Lake Erie)
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

# Calculate excretion rates with the average dry biomass per taxa (Wen & Peters 1994)
gv19_excretion_rate = gv19_DM %>% 
  #mutate(dm_mg = dry_biomass/1000) %>% # No need to convert to mg, equation is body weight (ug)
  mutate(ln_P_excretion = (-1.65)+(0.54)*log(dry_biomass)) %>% # ln(P ug per day)
  mutate(ln_N_excretion = (-1.38)+(0.67)*log(dry_biomass))  %>% # ln(N ug per day)
  mutate(N_excrete = exp(ln_N_excretion)) %>% # N ug per day
  mutate(P_excrete = exp(ln_P_excretion)) %>% # P ug per day
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  as_tibble()
gv19_excretion_rate

# Wen and Peters 1994 dry mass bounds 
# 0.13534 - 2980.958 
max(gv19_DM$dry_biomass) # 21.20257
min(gv19_DM[gv19_DM$dry_biomass>0, 'dry_biomass']) # 0.001312

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
  select(sampleid, doy, taxon, group.x, dry_biomass, N_excrete, P_excrete, dens) %>% #Select relevant columns 
  mutate(N_comm_excrete = N_excrete*dens, 
         P_comm_excrete = P_excrete*dens) %>%
  group_by(sampleid, doy) %>%
  summarise(N_excrete_sum = sum(N_comm_excrete), # get total N excretion of the community 
            P_excrete_sum = sum(P_comm_excrete), # get total P excretion of the community 
            ) %>% 
  ungroup() %>%
  as_tibble() 
gv19_excretion_rate_daily # ug N or P per L per day 
gv19_excretion_rate_daily_WP_OG = gv19_excretion_rate_daily

# Areal daily excretion rates (mg per m^2 per day) 
gv19_arealexc_join = gv19_excretion_rate_daily %>% 
  select(sampleid, doy, Nexcretion_m3, Pexcretion_m3) %>%
  mutate(N_excrete_mg = Nexcretion_m3/1000, # N mg per m^3 per day 
         P_excrete_mg = Pexcretion_m3/1000) %>% # P mg per m^3 per day 
  select(sampleid, doy, N_excrete_mg, P_excrete_mg) %>%
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
  select(sampleid, doy, N_excrete_mg, P_excrete_mg, tow) %>%
  mutate(N_areal = N_excrete_mg*tow, 
         P_areal = P_excrete_mg*tow) %>%
  as_tibble()
gv19_arealexc # mg per square meter per day 

