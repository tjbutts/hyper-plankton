## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#============================================================================================##
# STEP 7: SUPPLEMENTARY DATA - CALCULATIONS, FIGURES, AND TABLES 
#============================================================================================##

# Zooplankton Excretion #=====================================
Hebert_tot_exc_sum_e # ug N or P per day per L, estimate from Step4

# Convert to uM # 
Hebert_tot_exc_uM = Hebert_tot_exc_sum_e %>%
  rename(Nexc = H_ug_Nexcrete_sum_d, 
         Pexc = H_ug_Pexcrete_sum_d) %>% 
  mutate(Pexc_uM = (Pexc*1000000)/(1000000*30.97), # ug N or P per day per L to uM N or P per day  
         Nexc_uM = (Nexc*1000000)/(1000000*14.01))
Hebert_tot_exc_uM  

# Wen and Peters excretion equations 
zp_raw

# get the dry biomass (in micrograms) per taxa per day of year 
gv19_DM = zp_raw %>%
  select(sampleid, doy, taxon, group, drymass)
gv19_DM  

# get the density (number of individuals per liter) per taxa per day of year 
gv19_dens = zp_raw %>%
  select(sampleid, doy, taxon, group, density)
gv19_dens

# Need to add on a temperature column to incorporate Temp variable 
doy = c(211, 143, 150, 164, 171, 178, 192, 199,206,213, 220, 227, 234, 245,251,273)
temp = c(26,18,24,24,22,22,26,27,26,26,26,25,26,23,23,20)
gv19_temp = data.frame(doy, temp)
gv19_temp = gv19_temp %>%
  mutate(temp_K = temp+273.15, # Wen and Peters equations require temp in Kelvin
         temp_c = temp) 

gv19_DM_join = gv19_DM %>%
  left_join(gv19_temp, gv19_DM, by = 'doy') %>%
  as_tibble()
gv19_DM_join

WP_multi_excretion_rate = gv19_DM_join %>% # Dry mass already in ug 
  mutate(log_P_excretion = -5.28+(0.61*log10(drymass))+(0.01*temp_K)+(0.02*24)) %>% 
  mutate(log_N_excretion = -3.47+(0.74*log10(drymass))+(0.00002*temp_K^2)-(0.02*24)) %>%  
  mutate(N_excrete = (10^log_N_excretion)) %>%
  mutate(P_excrete = (10^log_P_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  rename(WP2_Nexcrete = N_excrete,
         WP2_Pexcrete = P_excrete) %>%
  select(sampleid, doy, taxon, group, WP2_Nexcrete, WP2_Pexcrete) %>%
  as_tibble()
WP_multi_excretion_rate # in ug per day 

# Transform to ug/L per day 
# Join excretion rate data to density data 
WP_multi_join = left_join(WP_multi_excretion_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
WP_multi_join
WP_multi_join_clean = WP_multi_join %>% 
  select(sampleid, doy, taxon, group.x, WP2_Nexcrete, WP2_Pexcrete, density) %>% #Select relevant columns 
  rename(group = group.x) %>%
  arrange(doy) %>%
  as_tibble()
WP_multi_join_clean # excretion and density joined together

# Do the summing step first before multiplying by density 
WP_multi_exc_sum = WP_multi_join_clean %>% 
  group_by(sampleid, doy, group) %>% # include larger taxonomic groupings together to not inflate rotifers 
  summarise(dens_sum = sum(density), 
            Nexc_sum = sum(WP2_Nexcrete), 
            Pexc_sum = sum(WP2_Pexcrete)) %>%
  ungroup() %>%
  mutate(Nexcrete_d = Nexc_sum*dens_sum, 
         Pexcrete_d = Pexc_sum*dens_sum) %>%
  group_by(sampleid, doy) %>%
  summarise(WP2_Nexcrete_sum_d = sum(Nexcrete_d),
            WP2_Pexcrete_sum_d = sum(Pexcrete_d)) %>% # get a daily sum 
  ungroup() %>% 
  arrange(doy) %>%
  as_tibble()
WP_multi_exc_sum # ug N or P per day per L

# Convert to uM 
WP_multi_uM = WP_multi_exc_sum %>% 
  rename(Nexc = WP2_Nexcrete_sum_d, 
         Pexc = WP2_Pexcrete_sum_d) %>%
  mutate(Nexc_uM = (Nexc*1000000)/(1000000*14.01), # ug N or P per day per L to uM N or P per day  
         Pexc_uM = (Pexc*1000000)/(1000000*30.97))
WP_multi_uM
  