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
if (!require(magrittr)) install.packages('magrittr')
library(magrittr)
if (!require(vegan)) install.packages('vegan')
library(vegan)
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges) 
if (!require(scales)) install.packages('scales')
library(scales) 
if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr) 
if (!require(lubridate)) install.packages('lubridate')
library(lubridate) 

# Set working directory to whichever file the datasets were saved to
# setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func")

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

# Zooplankton Stoichiometry Data
zp_stoich = read_csv('2019_zoop_cnpratios.csv')  

# Phytoplankton data # ======================
# Phytoplankton Biomass Data 
phy_biomass = read_csv('2019_site4_gv_phydata.csv') 
# Phytoplankton Grouping Information
phy_grouping = read_csv('2019_gv_phytoplanktongrouping.csv')  

# Green Valley Lake data #==================
# Surface water nutrient concentrations 
gv_nutrients = read_csv('2019_gv_nutrients.csv') 
# High frequency EXO data 
gv_exo_hf = read_csv('2019_highfrequency_gv_EXO3.csv') 

# Zooplankton Excretion Data from Hebert et al. 2016 #====================
n_dat_comb = read_csv('Hebert_extract_N.csv') # Freshwater and marine data 
n_dat_comb.fw = read_csv('Hebert_extract_N_FW.csv') # Just Freshwater data 
p_dat_comb = read_csv('Hebert_extract_P.csv') # Freshwater and marine data 
p_dat_comb.fw = read_csv('Hebert_extract_P_FW.csv') # Just Freshwater data 

# Zooplankton-phytoplankton size data #=====================
# Phytoplankton GALD v. Zooplankton length 
gv_gald_length = read_csv('2019_gv_gald-length.csv') 

# Supplemental #==========================
# Phytoplankton GALD v. Zooplankton bodymass 
gv_gald_bodymass = read_csv('2019_gv_gald-bodymass.csv')
# Historical GVL data from the Ambient Lakes Monitoring Program
alm_hist = read_csv('Historical_ALM.csv')
zoop_hist = read_csv('zp_historical.csv')
phyto_hist = read_csv('Historic Plankton Data.csv')

# ========== Metadata ================== #
# Document uploaded in GitHub # 
