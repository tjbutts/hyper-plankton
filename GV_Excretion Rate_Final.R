# GV Excretion Rates Final # 
# Crustacean zooplankton regression equation W&P =================================
# Using the Crustacean zooplankton regression equations in WP that incorporate temperature & experimental duration 
# I will set the experimental duration to 1 hour as I'm unclear why it needs a temporal component 
# After I run this through I'll make it 24 hours and see how much that effects it? The result seems to always be ug per day 
# so not clear 
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
  mutate(log_P_excretion = -5.28+(0.61*log10(dry_biomass))+(0.01*temp)+(0.02)) %>% 
  mutate(log_N_excretion = -3.47+(0.74*log10(dry_biomass))+(0.00002*temp^2)-(0.02)) %>%  
  mutate(N_excrete = (10^log_N_excretion)) %>%
  mutate(P_excrete = (10^log_P_excretion)) %>%
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

# Same process as above - however - going to sum the excretion rates for the day #==========================
# and then multiply that by total density. See if that boosts our rates at all? 
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
 
gv19_excretion_rate = gv19_DM_join %>% # Dry mass already in ug 
  mutate(log_P_excretion = -5.28+(0.61*log10(dry_biomass))+(0.01*temp)+(0.02)) %>% 
  mutate(log_N_excretion = -3.47+(0.74*log10(dry_biomass))+(0.00002*temp^2)-(0.02)) %>%  
  mutate(N_excrete = (10^log_N_excretion)) %>%
  mutate(P_excrete = (10^log_P_excretion)) %>%
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
gv19_join3 = left_join(gv19_excretion_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
gv19_join3
gv19_join3_clean = gv19_join3 %>% 
  select(sampleid, doy, taxon, group.x, dry_biomass, N_excrete, P_excrete, dens) %>% #Select relevant columns 
  rename(group = group.x) %>%
  #mutate(Nexcretion_denscorrected = N_excrete*dens, 
         #Pexcretion_denscorrected = P_excrete*dens) %>% # get ug N or P per day per L 
  arrange(doy) %>%
  as_tibble()
gv19_join3_clean # excretion and density joined together

# Do the summing step first before multiplying by density 
# ug N or P per day per L is just mg of N or P per day per m^3 
# Multiply by tow depth after summing by doy 
gv19_exc_sum2 = gv19_join3_clean %>% 
  group_by(sampleid, doy, group) %>% # include larger taxonomic groupings together to not inflate rotifers 
  summarise(dry_biomass_sum = sum(dry_biomass),
            dens_sum = sum(dens), 
            Nexc_sum = sum(N_excrete), 
            Pexc_sum = sum(P_excrete)) %>%
  ungroup() %>%
  mutate(N_excrete_d = Nexc_sum*dens_sum, 
         P_excrete_d = Pexc_sum*dens_sum) %>%
  group_by(sampleid, doy) %>%
  summarise(N_excrete_sum_d = sum(N_excrete_d),
            P_excrete_sum_d = sum(P_excrete_d)) %>% # get a daily sum 
  ungroup() %>%
  as_tibble()
gv19_exc_sum2 # ug N or P per day per L 


# set wd to tow depth measurements 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton") 
zvol = read.csv('gv_zp_log_site4.csv')

tow = zvol %>% 
  rename(tow = TOW, 
         sampleid = SAMPLEID) %>%
  select(sampleid, doy, tow) %>%
  as_tibble()
tow  

gv19_exctow_join2 = left_join(gv19_exc_sum2, tow, by = 'doy')
gv19_exctow_join2

gv19_arealexc_WP2 = gv19_exctow_join2 %>%
  rename(sampleid =sampleid.x) %>%
  select(sampleid, doy, N_excrete_sum_d, P_excrete_sum_d, tow) %>%
  mutate(N_areal = N_excrete_sum_d*tow, 
         P_areal = P_excrete_sum_d*tow) %>% 
  arrange(doy) %>%
  as_tibble()
gv19_arealexc_WP2 # mg per square meter per day 
