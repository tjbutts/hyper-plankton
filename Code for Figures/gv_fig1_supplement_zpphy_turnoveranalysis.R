# Temporal Turnover - Analysis # 

### Bray-Curtis Index - GV Data ### ============================================
rm(list=ls())
library(vegan)
library(tidyverse)

# Calculate standardized bray-curtis dissimilarity 
# Take (1-Bray-Curtis Dissimilarity)*100 to get the percent similarity 

### GV Zooplankton 

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

# Zooplankton Community Spread 
gv19_zpcomm <- gv19_zp %>% # Widen the dataframe
  pivot_wider(id_cols = c(sampleid, doy),
              names_from = taxon, 
              values_from = biomass) %>% 
  as_tibble() %>%
  arrange(doy)
as_tibble(gv19_zpcomm)

# Turn into matrix 
gvzp <- gv19_zpcomm
gvzp
gvzp.m <- as.matrix(gvzp[,3:48]) # remove sampleid & doy
gvzp.m
dimnames(gvzp.m)[[1]] <- gvzp$doy
gvzp.m

# Check which columns sum to zero 
colSums(gvzp.m)

# Remove columns that sum to zero (species never appear during the summer)
gvzp.m.clean = gvzp.m[, which(colSums(gvzp.m) !=0)]


# Transform biomass data using a Hellinger Transform 
# Hellinger:gives low weights to species with low counts (biomass in this case) and many zeros. 
## Divides each value by row sum, and takes sqrt of quotient
gvzp.m.hell <- decostand(gvzp.m.clean, method='hellinger')
gvzp.bcs <- vegdist(gvzp.m.hell, method='bray') # Calculate standardized Bray-Curtis Dissimilarity for ZP on hell-transformed biomass data  
gvzp.bcs # check to make it's copacetic 

library(reshape2)

gvzp.dissim <- melt(as.matrix(gvzp.bcs), varnames = c('row', 'col'))
gvzp.dissim

gvzp.dissim <- as_tibble(gvzp.dissim) # rename columns to work better 
gvzp.dissim <- gvzp.dissim %>% 
  rename(doy1 = row) %>%
  rename(doy2 = col) %>% 
  rename(dissimilarity = value)
gvzp_bcindex <- gvzp.dissim %>% mutate(index = (1-dissimilarity)*100)
gvzp_bcindex

plot_dissim <- gvzp_bcindex %>% 
  filter(doy1 == 143 & doy2 > 143)
plot_dissim

plot_dissim$lag = c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,18) 

plot(plot_dissim$lag, plot_dissim$index, type='o', pch=20, 
     ylim=c(30, 80), ylab = 'Similarity with DOY 143 (%)', 
     xlab = 'lag', cex=2, cex.lab=1.5, lwd=2)
plot_dissim

### GV Phytoplankton ####
setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/Phytoplankton")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Plankton Data/Phytoplankton")

gvphy <- read.csv('pre_phyto_comm.csv')

gvphy.m <- as.matrix(gvphy[,-1]) # remove doy
gvphy.m

dimnames(gvphy.m)[[1]] <- gvphy$X
gvphy.m

# Check which columns sum to zero 
colSums(gvphy.m)

# Remove columns that sum to zero (species never appear during the summer)
gvphy.m.clean = gvphy.m[, which(colSums(gvphy.m) !=0)]

gvphy.hell <-decostand(gvphy.m.clean, method = 'hellinger')
gvphy.bcs <- vegdist(gvphy.hell, method = 'bray') # Calculate standardized Bray-Curtis Dissimilarity for ZP 
gvphy.bcs # check to make it's copacetic 

library(reshape2)

gvphy.dissim <- melt(as.matrix(gvphy.bcs), varnames = c('row', 'col'))
gvphy.dissim

gvphy.dissim <- as_tibble(gvphy.dissim) # rename columns to work better 
gvphy.dissim <- gvphy.dissim %>% 
  rename(doy1 = row) %>%
  rename(doy2 = col) %>% 
  rename(dissimilarity = value)
gvphy_bcindex <- gvphy.dissim %>% mutate(index = (1-dissimilarity)*100)
gvphy_bcindex

plot_dissim <- gvphy_bcindex %>% 
  filter(doy1 == 143 & doy2 > 143)
plot_dissim

plot_dissim$lag = c(1,2,3,4,5,7,8,9,10,11,12,14,15) 

plot(plot_dissim$lag, plot_dissim$index, type='o', pch=20,
     ylab = 'Similarity with DOY 143 (%)', 
     xlab = 'DOY', cex=2, cex.lab=1.5, lwd=2)
