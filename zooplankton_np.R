# Zoop N:P # 
rm(list = ls())

# Set wd 
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")

# Load in libraries 
library(tidyverse)
library(magrittr)

# Load in dataset with n and p information 
zp_cnp <- read.csv('moody_zooplankton_cnp.csv')
as_tibble(zp_cnp)

zp_cnp %<>% rename(perc_c = X.C) %>%
  rename(perc_n = X.N) %>%
  rename(perc_p = X.P) %>%
  as_tibble()
zp_cnp %<>% select(taxon, group, perc_c, perc_n, perc_p) %>% as_tibble()
zp_cnp

# read in zp biomass 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")

# Turn raw R output into a cleaned zoop data
zp_raw <- read.csv('2019_site4_gv_ZoopBiomass_25Feb2021.csv')


zp_raw2 <- zp_raw %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID) %>%
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(biomass = BIOMASS.UG.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

gv19_zp = zp_raw2 %>% select(sampleid,doy, taxon,group,biomass) %>% 
  mutate(biomass = replace_na(biomass, 0)) %>% # Replace NAs with 0s 
  filter(!(doy == 162 | doy == 157 | doy == 211)) # Remove the ALM sampling dates plus DOY 157 which we determined was wonky
as_tibble(gv19_zp)

# Combine biomass with cnp data 
zp_stoic <- left_join(gv19_zp, zp_cnp, by='taxon') # Join np with biomass data 
as_tibble(zp_stoic)
zp_stoic %<>% select(sampleid, doy, taxon, group.y, biomass, perc_c, perc_n, perc_p) %>%
  rename(group = group.y) %>% as_tibble()
as_tibble(zp_stoic)

# Group taxon together in stoich groupings, then calculate ugX_L 
zp_stoic_ugL <- zp_stoic %>% 
  mutate(p_storage = (perc_p/100)*biomass) %>%
  mutate(n_storage = (perc_n/100)*biomass) %>%
  mutate(c_storage = (perc_c/100)*biomass) %>% 
  group_by(doy, group) %>%
  summarise(ugp_L = sum(p_storage),
            ugn_L = sum(n_storage), 
            ugc_L = sum(c_storage)) %>%
  as_tibble()
zp_stoic_ugL  

# Multiply C, N, and P by biomass to get C, N, and P storage weighted by biomass by taxon
zp_stoic_molar <- zp_stoic %>% select(sampleid, doy, taxon, group, biomass, perc_c, perc_n, perc_p) %>% # Select appropriate columns 
  mutate(p_storage = (perc_p/100)*biomass) %>%
  mutate(n_storage = (perc_n/100)*biomass) %>% 
  mutate(c_storage = (perc_c/100)*biomass) %>% 
  mutate(n_molar = n_storage/14010000) %>% # Get molar ratio 
  mutate(p_molar = p_storage/30970000) %>% # Get molar ratio 
  select(sampleid, doy, taxon, group, biomass, n_molar, p_molar) %>%
  as_tibble
zp_stoic_molar

# Get Zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum <- zp_stoic_molar %>% 
  group_by(sampleid, doy) %>%
  summarise(
    nstorage = sum(n_molar), 
    pstorage = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble()
zp_stoic_sum

# Calculate community N:P 
zp_stoic_sum %<>% mutate(zp_np = (nstorage)/(pstorage)) 
zp_stoic_sum

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(zp_stoic_sum$doy, zp_stoic_sum$zp_np, type = 'o', pch=17, col='black', cex=3, cex.axis=2 , lwd=4, xlab='', ylab='') 
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Day of Year, 2019', side = 1, line =3, cex = 2)
legend('topright', legend=c('Zoop N:P'), col = c('black'), cex=1.5, pt.cex=3, pch=c(17))

# Get Zooplankton community N:P by summing N and P storage per taxonomic grouping, doy, and sampleid
zp_stoic_sum_group <- zp_stoic_molar %>% 
  group_by(sampleid, doy, group) %>%
  summarise(
    nstorage = sum(n_molar), 
    pstorage = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble()
zp_stoic_sum_group
#calculate N:P 
zp_stoic_sum_group %<>% mutate(zp_np = (nstorage)/(pstorage)) 
zp_stoic_sum_group
dat <- as.data.frame(zp_stoic_sum_group)

# By group - N:P $ Something is wrong here....
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))
dat[is.nan(dat)] <- 0

plot(dat[dat$group=='Calanoid', "doy"], dat[dat$group == "Calanoid", "zp_np"], 
     col = '#4d004b', ylim = c(0, max(dat$zp_np+1)), pch=20,type='o', cex=2)
points(dat[dat$group=='Cyclopoid', 'doy'], dat[dat$group == 'Cyclopoid', 'zp_np'],
         col = '#810f7c', pch=20,type='o', cex=2)
points(dat[dat$group=='Nauplii', 'doy'], dat[dat$group == 'Nauplii', 'zp_np'], 
         col = '#88419d', pch = 20,type='o', cex=2)
points(dat[dat$group=='Daphnia', 'doy'], dat[dat$group == 'Daphnia', 'zp_np'],
         col = '#8c6bb1', pch=20,type='o', cex=2)
points(dat[dat$group=='Chydoridae', 'doy'], dat[dat$group == 'Chydoridae', 'zp_np'], 
         col = '#8c96c6', pch=20,type='o', cex=2)
points(dat[dat$group=='Rotifer', 'doy'], dat[dat$group == 'Rotifer', 'zp_np'],
         col = '#41ae76', pch=20,type='o', cex=2)
points(dat[dat$group=='Ostracod', 'doy'], dat[dat$group == 'Ostracod', 'zp_np'],
         col = '#238b45', pch=20,type='o', cex=2)
points(dat[dat$group=='Bosmina', 'doy'], dat[dat$group == 'Bosmina', 'zp_np'],
       col = '#238b45', pch=20,type='o', cex=2)
points(dat[dat$group=='Daphniid', 'doy'], dat[dat$group == 'Daphniid', 'zp_np'],
       col = '#238b45', pch=20,type='o', cex=2)
points(dat[dat$group=='Ceriodaphnia', 'doy'], dat[dat$group == 'Ceriodaphnia', 'zp_np'],
       col = '#238b45', pch=20,type='o', cex=2)


# Calculate ug_L per nutrient by taxonomic groupings 
as_tibble(zp_stoic_ugL)

library(viridis)
library(hrbrthemes)

# turn zP_stoic_ugL wide to see which taxa can be removed if they don't appear 
zp_stoic_wide <- zp_stoic_ugL %>%
  pivot_wider(names_from = group, values_from = c(ugp_L, ugn_L, ugc_L)) %>%
  as_tibble()
zp_stoic_wide # Can remove Diaphanosoma and Leptodora 

zp_stoic_ugL %<>% filter(!(group == 'Diaphanosoma' | group == 'Leptodora'))

# By ug/L - N 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
ggplot(zp_stoic_ugL, aes(x=doy, y=ugn_L, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# by ug/L - P 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
ggplot(zp_stoic_ugL, aes(x=doy, y=ugp_L, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# by ug/L - C 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
ggplot(zp_stoic_ugL, aes(x=doy, y=ugc_L, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

