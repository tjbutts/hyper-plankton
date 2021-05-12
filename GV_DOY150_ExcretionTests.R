# DOY 150 gv excretion rate # 
# Wen and Peters #=================================
# Using the Crustacean zooplankton regression equations in WP that incorporate temperature & experimental duration 
# I will set the experimental duration to 1 hour as I'm unclear why it needs a temporal component 
# After I run this through I'll make it 24 hours and see how much that effects it? The result seems to always be ug per day 
# so not clear 
rm(list=ls())
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

# Need to add on a temperature column to incorporate Temp variable 
doy = c(211, 143, 150, 164, 171, 178, 192, 199,206,213, 220, 227, 234, 245,251,273)
temp = c(26,18,24,24,22,22,26,27,26,26,26,25,26,23,23,20)
gv19_temp = data.frame(doy, temp)
gv19_temp = gv19_temp %>%
  mutate(temp = temp+273.15) # Data requires temp in K

gv19_DM_join = gv19_DM %>%
  left_join(gv19_temp, gv19_DM, by = 'doy') %>%
  as_tibble()
gv19_DM_join

# Calculate excretion rates with the average dry biomass per taxa 
# Wen & Peters requires body size (in ug) in order to estimate excretion 
# I will estimate N excretion (r2 72%, and then use that value to estimate P excretion r2 88%)
# Doing so might get a more accurate picture of N:P excretion than independently calculating either 
gv19_excretion_rate = gv19_DM_join %>% # Dry mass already in ug 
  mutate(ln_N_excretion = -9.36+(0.79*log(dry_biomass))+(0.02*temp)-(0.02)) %>%  
  mutate(ln_P_excretion = 2.29+(0.88*ln_N_excretion)-(0.00004*temp^2)-0.009) %>% 
  mutate(N_excrete = exp(ln_N_excretion)) %>%
  mutate(P_excrete = exp(ln_P_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  as_tibble()
gv19_excretion_rate # in ug per day 

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
gv19_excretion_conc = gv19_join2 %>% 
  select(sampleid, doy, taxon, group.x, dry_biomass, N_excrete, P_excrete, dens) %>% #Select relevant columns 
  rename(group = group.x) %>%
  mutate(Nexcretion_denscorrected = N_excrete*dens, 
         Pexcretion_denscorrected = P_excrete*dens) %>% # get ug N or P per day per L 
  as_tibble()
gv19_excretion_conc # ug N or P per day per L 

# ug N or P per day per L is just mg of N or P per day per m^3 
# Multiply by tow depth after summing by doy 
gv19_exc_sum = gv19_excretion_conc %>% 
  group_by(sampleid, doy) %>%
  summarise(dry_biomass_sum = sum(dry_biomass),
            dens_sum = sum(dens), 
            Nexc_sum = sum(Nexcretion_denscorrected), 
            Pexc_sum = sum(Pexcretion_denscorrected)) %>%
  ungroup() %>%
  as_tibble()
gv19_exc_sum # ug N or P per day per L 

# set wd to tow depth measurements 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton") 
zvol = read.csv('gv_zp_log_site4.csv')

tow = zvol %>% 
  rename(tow = TOW, 
         sampleid = SAMPLEID) %>%
  select(sampleid, doy, tow) %>%
  as_tibble()
tow  

gv19_exctow_join = left_join(gv19_exc_sum, tow, by = 'doy')
gv19_exctow_join

gv19_arealexc_WP = gv19_exctow_join %>%
  rename(sampleid =sampleid.x) %>%
  select(sampleid, doy, Nexc_sum, Pexc_sum, tow) %>%
  mutate(N_areal = Nexc_sum*tow, 
         P_areal = Pexc_sum*tow) %>% 
  arrange(doy) %>%
  as_tibble()
gv19_arealexc_WP # mg per square meter per day 

# Hebert equations #=================================
# Find average dry biomass per taxa 
# Calculate excretion rate (nmol of N or P per indv per hr) (Hebert)
# Multiply by species density (# of indv/L to get nmol of N or P per L per hr) 
# Multiply by 24 to get daily rate  (nmol of N or P per L per d)
# Convert to ug of N or P per L per d
# ===Comparison to Lake Erie Values=== # 
# Convert to mg of N or P per m^3 per d 
# Get Areal rate by multiplying by Tow Depth (mg per m^2 per day)
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

# Convert excretion rate from nmol to M (1000000000 nmol in 1 M)
gv19_excretion_rate_M = gv19_excretion_rate %>%
  mutate(N_excrete_M = N_excrete/1000000000, 
         P_excrete_M = P_excrete/1000000000) %>%
  as_tibble()

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
gv19_join2 = left_join(gv19_excretion_rate_M, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
gv19_join2
gv19_excretion_conc = gv19_join2 %>% 
  select(sampleid, doy, taxon, group.x, dm_mg, N_excrete_M, P_excrete_M, dens) %>% #Select relevant columns 
  rename(group = group.x) %>%
  mutate(Nexcretion_denscorrected = N_excrete_M*dens, 
         Pexcretion_denscorrected = P_excrete_M*dens) %>% # get ug N or P per day per L 
  as_tibble()
gv19_excretion_conc # ug N or P per day per L

# Zoop community excretion 
gv19_zpcomm_excretionrate = gv19_excretion_conc %>% 
  select(sampleid, doy, taxon, group, dm_mg, Nexcretion_denscorrected, Pexcretion_denscorrected) %>%
  mutate(Nexcretion_daily_M = Nexcretion_denscorrected*24,
         Pexcretion_daily_M = Pexcretion_denscorrected*24) %>%
  mutate(Nexcretion_daily_ug = Nexcretion_daily_M*14010000,
         Pexcretion_daily_ug = Pexcretion_daily_M*30970000) %>%
  group_by(sampleid, doy) %>%
  summarise(N_comm_excretion = sum(Nexcretion_daily_ug), 
            P_comm_excretion = sum(Pexcretion_daily_ug)) %>%
  as_tibble()
gv19_zpcomm_excretionrate # here in ug per L per day summed by the community 

# Areal daily excretion rates (mg per m^2 per day) 
gv19_arealexc_join = gv19_zpcomm_excretionrate %>% # ug per L per day is just mg per m^3 per day so no need to convert 
  select(sampleid, doy, N_comm_excretion, P_comm_excretion)
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
gv19_exctow_join

gv19_arealexc_Hebert = gv19_exctow_join %>%
  rename(sampleid =sampleid.x) %>%
  select(sampleid, doy, N_comm_excretion, P_comm_excretion, tow) %>%
  mutate(N_areal = N_comm_excretion*tow, 
         P_areal = P_comm_excretion*tow) %>%
  as_tibble()
gv19_arealexc_Hebert # mg per square meter per day 
gv19_doy150_exc = gv19_arealexc_Hebert %>%
  filter(doy == 150) %>%
  as_tibble()

gv19_doy150_exc # ug per L per day 
gv19_arealexc_WP %>% filter(doy == 150) # ug per L per day 
