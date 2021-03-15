## GV - Zooplankton - Phytoplankton Dynamics # 
rm(list=ls())
library(tidyverse)
library(magrittr)
library(purrr)

# Phytoplankton
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton")
gv19_phy <- read.csv('GVL_2019_Phyto_Summary.csv')
as_tibble(gv19_phy)

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

# Zooplankton 

# Set wd to raw data file 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")

# Turn raw R output into a cleaned zoop data
zp_raw <- read.csv('2019_site4_gv_ZoopBiomass_09Feb2021.csv')

zp_raw2 <- zp_raw %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID) %>%
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(biomass = BIOMASS.UG.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

#total biomass
gv19_zp = zp_raw2 %>% select(sampleid,doy, taxon,group,biomass) %>% mutate(biomass = replace_na(biomass, 0))
as_tibble(gv19_zp)
gv_totalbiom <- gv19_zp %>%
  select(sampleid, biomass,group, doy) %>%
  group_by(sampleid, doy) %>%
  summarise(
    totalbiom = sum(biomass)) %>%
  ungroup()
as_tibble(gv_totalbiom)

# Conjoin the total biomass datasets gv_totalbiom (zp) & pre_totalbiom (phyto)
pre_totalbiom
gv_totalbiom

phy = 'phy'
zp = 'zp'

totphy <- pre_totalbiom %>% 
  rename(doy = Day) %>% 
  mutate(phy_biom_ug_l = totalbiom*1000) %>% 
  as_tibble()
totphy  

totzp <-   gv_totalbiom %>% 
  rename(zp_biom_ug_l = totalbiom) %>% 
  as_tibble()
totzp  

plankton <- left_join(totzp, totphy, by = 'doy')
tte <- plankton %>% 
  mutate(tte = zp_biom_ug_l/phy_biom_ug_l) %>%
  mutate(tte2 = zp_biom_ug_l/totalbiom) %>% 
  as_tibble()
tte

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(tte$doy, tte$tte2, type = "o", pch = 20, col='mediumseagreen', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="zp:phy ratio", lwd=4, xlim=c(143,255) )


