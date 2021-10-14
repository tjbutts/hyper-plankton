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
if (!require(here)) install.packages('here')
library(here)

# Set working directory 
here()

# Data sets
# Zooplankton data # 
zp_dat = read_csv('2019_site4_gv_zoopdata.csv') #Zooplankton Biomass (ug/L), Average Dry Mass (ug), and Density (#/L)
zp_log = read_csv('2019_site4_gv_zooplog.csv') # Zooplankton Sampling Information
zp_stoich = read_csv('2019_zoop_cnpratios.csv') # Zooplankton Stoichiometry Data 

# Phytoplankton data # 
phy_biomass = read_csv('2019_site4_gv_PhytoBiomass_06Jun2021.csv') # Phytoplankton Biomass Data 
phy_grouping = read_csv('2019_gv_phytoplanktongrouping.csv') # Phytoplankton Grouping Information 

# Green Valley Lake data # 
gv_nutrients = read_csv('2019_gv_nutrients.csv') # Surface water nutrient concentrations 
gv_exo_hf = read_csv('2019_highfrequency_gv_EXO3.csv') # High frequency EXO data 
gv_met_buoy = read_csv('2019_gv_thermo-buoy.csv') # Thermocline/buoyancy frequency information 

# Zooplankton-phytoplankton size data # 
gv_gald_length = read_csv('2019_gv_gald-length.csv') # Phytoplankton GALD v. Zooplankton length 
gv_gald_bodymass = read_csv('2019_gv_gald-bodymass.csv') # Phytoplankton GALD v. Zooplankton bodymass 

# ========== Metadata ================== # 

