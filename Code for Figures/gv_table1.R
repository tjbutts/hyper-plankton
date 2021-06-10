# GV Table 1 Info # 
library(tidyverse)

rm(list=ls())
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Tyler GVL")
gvl = read.csv("GVL_nutrients_w_ALM.csv")
gvl

#Subset to the study year, site, and surface depth 
gvl19 = gvl %>%
  filter(year==2019, siteID==4, depthID==1) %>%
  filter(project=="GVLmonitoring") %>%
  select(doy, SRP_ugL, TP_ugL, TN_mgL, NOx_mgL, microcystin_ugL) %>%
  filter(!(doy == 162))
gvl19

# Replace 0s in SRP and NOx estimate with detection limit/2 of AQ2 (3.9 and 0.03, respectively) 
gvl19$SRP_ugL<-replace(gvl19$SRP_ugL, gvl19$SRP_ugL<3.9, 3.9)
gvl19$SRP_ugL
gvl19$NOx_mgL<-replace(gvl19$NOx_mgL, gvl19$NOx_mgL<0.1, 0.1)
gvl19$NOx_mgL

#TN 
max(gvl19$TN_mgL, na.rm = T)
min(gvl19$TN_mgL, na.rm = T)
mean(gvl19$TN_mgL, na.rm = T)
sd(gvl19$TN_mgL, na.rm = T) 

#NOx 
max(gvl19$NOx_mgL, na.rm = T)
min(gvl19$NOx_mgL, na.rm = T)
mean(gvl19$NOx_mgL, na.rm = T)
sd(gvl19$NOx_mgL, na.rm = T) 

# NHX 
# set wd # 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Final Data/Historical Data")

# ALM Historical Data - Long # 
alm_long = read_csv('Historical_ALM.csv')
alm_long
alm_long = alm_long %>%
  filter(Year > 2018) %>%
  select(name, Year, DOYfrac, DOY, analyte, result, unit, detect, detectLimit)# Dissolved
alm_long
unique(alm_long$analyte)
max(alm_long[alm_long$analyte == 'Ammonia-nitrogen (as N)', 'result'])*1000
min(alm_long[alm_long$analyte == 'Ammonia-nitrogen (as N)', 'result'])*1000
nhx_vec <- alm_long[alm_long$analyte == 'Ammonia-nitrogen (as N)', 'result']
mean(nhx_vec$result)
sd(nhx_vec$result)

# AquIA database said detect lim is 0.0048 mg/L which is 4.8 ug/L

# TP 
max(gvl19$SRP_ugL, na.rm = T)
min(gvl19$SRP_ugL, na.rm = T)
mean(gvl19$SRP_ugL, na.rm = T)
sd(gvl19$SRP_ugL, na.rm = T) 

# Plankton Data 
# Set wd to raw data file 
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
  rename(doy = DOY) %>%
  filter(!(doy == 162 | doy == 157)) 
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

gv19_zp = zp_raw2 %>% select(sampleid,doy, taxon,group,biomass) %>% mutate(biomass = replace_na(biomass, 0))
as_tibble(gv19_zp)

# Total Biomass 
gv_totalbiom <- gv19_zp %>%
  select(sampleid, biomass,group, doy) %>%
  group_by(sampleid, doy) %>%
  summarise(
    totalbiom = sum(biomass)) %>%
  ungroup() %>%
  arrange(doy)
as_tibble(gv_totalbiom)

max(gv_totalbiom$totalbiom)
min(gv_totalbiom$totalbiom)
mean(gv_totalbiom$totalbiom)
sd(gv_totalbiom$totalbiom)

# phytos 

setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Plankton Data/phytoplankton")

prephy <- read.csv('pre_phyto_comm.csv') # pre-incubation phytoplankton community 
postphy <- read.csv('post_phyto_comm.csv') # post-incubation phytoplankton community 

setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton")
setwd("C:/Users/Tyler/Box Sync/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton")

gv19_phy <- read.csv('GVL_2019_Phyto_Summary.csv')
as_tibble(gv19_phy)

# Total biomass - filtered to pre 
gv_totalbiom <- gv19_phy %>%
  select(TAXON, BIOMASS.MG.L, Day, Treatment) %>%
  group_by(Day, Treatment) %>%
  summarise(
    totalbiom = sum(BIOMASS.MG.L)) %>%
  ungroup()
as_tibble(gv_totalbiom)

pre_totalbiom <- gv_totalbiom %>%
  filter(Treatment == "Pre")
pre_totalbiom 

max(pre_totalbiom$totalbiom)
min(pre_totalbiom$totalbiom)
mean(pre_totalbiom$totalbiom)

cya = gv19_phy %>% 
  filter(DIVISION == 'Cyanophyta') %>%
  filter(Treatment == 'Pre')

max(cya$BIOMASS.MG.L)
min(cya$BIOMASS.MG.L)
mean(cya$BIOMASS.MG.L)
