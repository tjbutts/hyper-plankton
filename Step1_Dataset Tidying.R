## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts October 2021

#============================================#
# STEP 1: LOAD IN DATASETS 
#============================================#
rm(list=ls())
graphics.off()

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

# Set working directory to whichever file the datasets were saved to
here::here() # Set to R project

# Data sets
# Zooplankton data #===============

#Zooplankton Biomass (ug/L), Average Dry Mass (ug), and Density (#/L)
zp_dat = read_csv('2019_site4_gv_zoopdata.csv') # Need to tidy data
zp_raw = zp_dat %>%
  rename(sampleid = SAMPLE.ID) %>% # Rename columns for convenience 
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(biomass = BIOMASS.UG.L) %>%
  rename(drymass = BIOMASS.UG) %>%
  rename(density = INDV.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY) %>%
  mutate(biomass = replace_na(biomass,0), # Replace NAs with 0
         drymass = replace_na(drymass,0),
         density = replace_na(density,0)) %>%
  arrange(doy)
zp_raw$group = as.factor(zp_raw$group) # makes the group column a factor, easier for later analysis 
# Zooplankton Sampling Information
zp_log = read_csv('2019_site4_gv_zooplog.csv') 
# Zooplankton Stoichiometry Data
zp_stoich = read_csv('2019_zoop_cnpratios.csv')  


# Phytoplankton data # ======================
# Phytoplankton Biomass Data 
phy_biomass = read_csv('2019_site4_gv_PhytoBiomass_06Jun2021.csv') 
# Phytoplankton Grouping Information
phy_grouping = read_csv('2019_gv_phytoplanktongrouping.csv')  

# Green Valley Lake data #==================
# Surface water nutrient concentrations 
gv_nutrients = read_csv('2019_gv_nutrients.csv') 
# High frequency EXO data 
gv_exo_hf = read_csv('2019_highfrequency_gv_EXO3.csv') 
# Thermocline/buoyancy frequency information 
gv_met_buoy = read_csv('2019_gv_thermo-buoy.csv') 

# Zooplankton-phytoplankton size data #=====================
# Phytoplankton GALD v. Zooplankton length 
gv_gald_length = read_csv('2019_gv_gald-length.csv') 
# Phytoplankton GALD v. Zooplankton bodymass 
gv_gald_bodymass = read_csv('2019_gv_gald-bodymass.csv') 

# ========== Metadata ================== # 

