# Purpose of this script is to calculate the excretion rate of N and P of zooplankton from Green Valley Lake, IA 
# First all code from GV_Dry Mass_Abundance_perSS.R must be run to get the data objects below: 
gv19_DM
gv19_COUNT
# Calculate community weighted mean ============================================
# STEP 6: COMMUNITY WEIGHTED MEAN - COUNT = ABUNDANCE, VALUE = BIOMASS (ug)
# Datasets to join 
gv19_COUNT
gv19_DM

#Join data and calculate community weighted mean per taxa 
gv19_cwm_join = left_join(gv19_DM, gv19_COUNT, by = c('sampleid', 'taxon'))
gv19_dm_cwm = gv19_cwm_join %>%
  select(sampleid, doy.x, taxon, group.x, count, dry_biomass) %>%
  rename(doy = doy.x, 
         group = group.x) %>%
  group_by(doy, group) %>%
  summarize( 
    DM_cwm = weighted.mean(dry_biomass, count)) %>%
  mutate(DM_cwm = replace_na(DM_cwm, 0)) %>%
  as_tibble() %>%
  ungroup()
gv19_dm_cwm

# Join data and calculate community weighted mean per DOY  
gv19_cwm_join_alt = left_join(gv19_DM, gv19_COUNT, by = c('sampleid', 'taxon'))
gv19_dm_cwm_alt = gv19_cwm_join_alt %>%
  select(sampleid, doy.x, taxon, group.x, count, dry_biomass) %>%
  rename(doy = doy.x, 
         group = group.x) %>%
  group_by(doy) %>%
  summarize(DM_cwm = weighted.mean(dry_biomass, count)) %>%
  as_tibble() 
gv19_dm_cwm_alt

#============================================
# STEP 7: EXCRETION RATE - do after get community weighted mean 
# Use excretion rate equations from Hebert to calculate nmol N or P per indv. per hour excretion rate based on dry mass
# Community weighted mean was grouped via taxa not DOY 
gv19_dm_cwm_mg = gv19_dm_cwm %>% 
  mutate(DM_cwm_mg = DM_cwm/1000) # Convert biomass from ug to mg to fit regression equation parameters 
gv19_excretion_rate = gv19_dm_cwm_mg %>%
  mutate(ln_N_NH4_excretion = 0.84*log(DM_cwm_mg)+2.50) %>% # nmol N per indv. per hour 
  mutate(ln_P_PO4_excretion = 0.70*log(DM_cwm_mg)+0.56) %>% # nmol P per indv. per hour 
  mutate(N_excrete = exp(ln_N_NH4_excretion)) %>%
  mutate(P_excrete = exp(ln_P_PO4_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  as_tibble()
gv19_excretion_rate

gv19_dailyexcretion = gv19_excretion_rate %>%
  mutate(dailyN_exc = N_excrete*24) %>% # convert hourly rate to daily rate 
  mutate(dailyP_exc = P_excrete*24) %>%
  mutate(dailyN_umol_d = dailyN_exc/1000) %>% # convert nmol to umol 
  mutate(dailyP_umol_d = dailyP_exc/1000) %>%
  as_tibble()
gv19_dailyexcretion

# same equations but with alt dry mass calculations per DOY 
gv19_dm_cwm_mg2 = gv19_dm_cwm_alt %>% 
  mutate(DM_cwm_mg = DM_cwm/1000) # Convert biomass from ug to mg to fit regression equation parameters 
gv19_excretion_rate2 = gv19_dm_cwm_mg2 %>%
  mutate(ln_N_NH4_excretion = 0.84*log(DM_cwm_mg)+2.50) %>% # nmol N per indv. per hour 
  mutate(ln_P_PO4_excretion = 0.70*log(DM_cwm_mg)+0.56) %>% # nmol P per indv. per hour 
  mutate(N_excrete = exp(ln_N_NH4_excretion)) %>%
  mutate(P_excrete = exp(ln_P_PO4_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  as_tibble()
gv19_excretion_rate2

gv19_dailyexcretion2 = gv19_excretion_rate2 %>%
  mutate(dailyN_exc = N_excrete*24) %>% # convert hourly rate to daily rate 
  mutate(dailyP_exc = P_excrete*24) %>%
  mutate(dailyN_umol_d = dailyN_exc/1000) %>% # convert nmol to umol 
  mutate(dailyP_umol_d = dailyP_exc/1000) %>%
  as_tibble()
gv19_dailyexcretion2

#============================================
# STEP 8: CORRECT FOR SUBSAMPLE VOLUME
# Create a sample log that has the following column headers -- COPIED EXACTLY:

# SAMPLEID	- the 12-character sample ID that is a perfect match to the file names
# SAMPLEVOLUME	- the volume of the total sample in milliliters
# VOLUMECOUNTED	- the volume of the subsample counted in milliliters
# TOW - the depth of the net tow for the zooplankton sample

# Note: there can be other columns, if desired, they will just be ignored in this script

# Each count file will be a row in this log, all of the columns must be filled in for each row
# Save the file as .csv in a DIFFERENT folder from the count files
# Do not include duplicate samples in the log unless they are being reported to DNR
# Each row must have a unique sample ID, so if duplicate samples were mis-named for the file, the file name will need to be changed to be unique
# In the parentheses below, put the full path to the sample log file
# A quick way to get this path is to right click on the file, go to "Properties", and copy the path -- NOTE THAT THE SLASHES IN THE PATH MUST BE "/"
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
zvol<-read.csv('gv_zp_log_site4.csv')
zvol = zvol %>% filter(!(doy == 162 | doy == 157)) # filter out DOYs that were removed in an earlier data cleaning step

names(zvol) #Check that your column headers are correct and match the headers listed above
zvol$SAMPLEID #Spits out a list of all of the sample IDs so you can double check

WI.NET.AREA<-0.0113 #m^2 ##this is the current diameter of the nets we use##
nsample<-length(zvol$SAMPLEID)
SSVOL<-c()
SVOL<-c()
TVOL<-c()
PROCDATE<-c()
PROCBY<-c()
for(i in c(1:nsample)){
  ssvol<-rep((zvol$VOLUMECOUNTED[i]/1000),46) #46 = number of taxa
  SSVOL<-append(SSVOL,ssvol)
  svol<-rep((zvol$SAMPLEVOLUME[i]/1000),46) #46 = number of taxa
  SVOL<-append(SVOL,svol)
  tvol<-rep(zvol$TOW[i], 46) #46 = number of taxa
  TVOL<-append(TVOL,tvol)
}
TVOLF<-(WI.NET.AREA*TVOL)*1000

#==========================================
# STEP 9: CALCULATE VOLUME CORRECTED DAILY EXCRETION RATES IN uM/L
gv19_dailyexcretion # Append onto this data frame the volume correction 

gv19_dailyexcretion_vcorrect = gv19_dailyexcretion %>%
  select(doy,group, DM_cwm_mg, dailyN_umol_d, dailyP_umol_d) %>%
  as_tibble()
gv19_join = left_join(gv19_dailyexcretion_vcorrect, zvol, by = 'doy')
gv19_join

gv19_excretion_rate_uML = gv19_join %>% 
  select(SAMPLEID, doy, group, DM_cwm_mg, dailyN_umol_d, dailyP_umol_d, SAMPLEVOLUME, VOLUMECOUNTED, TOW) %>%
  mutate(daily_N_umol_L=(dailyN_umol_d*(SAMPLEVOLUME/1000)/(VOLUMECOUNTED/1000)*((WI.NET.AREA*TOW)*1000))) %>%
  mutate(daily_P_umol_L=(dailyP_umol_d*(SAMPLEVOLUME/1000)/(VOLUMECOUNTED/1000)*((WI.NET.AREA*TOW)*1000))) %>%
  select(!c(SAMPLEVOLUME, VOLUMECOUNTED, TOW)) %>%
  as_tibble()
gv19_excretion_rate_uML  

gv19_excretion_rate_dailymean = gv19_excretion_rate_uML %>%
  group_by(doy) %>%
  summarize(
    meandaily_N_umol_L = mean(daily_N_umol_L),
    meandaily_P_umol_L = mean(daily_P_umol_L), 
    N_sd = sd(daily_N_umol_L), 
    P_sd = sd(daily_P_umol_L), 
    N_se = N_sd/sqrt(7), 
    P_se = P_sd/sqrt(7)) %>%
  ungroup() %>%
  as_tibble()
gv19_excretion_rate_dailymean

# Alternate dry mass calculation 
gv19_dailyexcretion2 # Append onto this data frame the volume correction 

gv19_dailyexcretion_vcorrect2 = gv19_dailyexcretion2 %>%
  select(doy, DM_cwm_mg, dailyN_umol_d, dailyP_umol_d) %>%
  as_tibble()
gv19_join2 = left_join(gv19_dailyexcretion_vcorrect2, zvol, by = 'doy')
gv19_join2

gv19_excretion_rate_uML2 = gv19_join2 %>%
  select(SAMPLEID, doy, DM_cwm_mg, dailyN_umol_d, dailyP_umol_d, SAMPLEVOLUME, VOLUMECOUNTED, TOW) %>%
  mutate(daily_N_umol_L=(dailyN_umol_d*(SAMPLEVOLUME/1000)/(VOLUMECOUNTED/1000)*((WI.NET.AREA*TOW)*1000))) %>%
  mutate(daily_P_umol_L=(dailyP_umol_d*(SAMPLEVOLUME/1000)/(VOLUMECOUNTED/1000)*((WI.NET.AREA*TOW)*1000))) %>%
  select(!c(SAMPLEVOLUME, VOLUMECOUNTED, TOW)) %>%
  as_tibble()
gv19_excretion_rate_uML2 

gv19_excretion_rate_dailymean2 = gv19_excretion_rate_uML2 %>%
  group_by(doy) %>%
  summarize(
    meandaily_N_umol_L = mean(daily_N_umol_L),
    meandaily_P_umol_L = mean(daily_P_umol_L)) %>%
  ungroup() %>%
  as_tibble()
gv19_excretion_rate_dailymean2
