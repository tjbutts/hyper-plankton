#################################################
#### Green Valley Zooplankton Community Data ####
#################################################
rm(list=ls())
# Read in raw data from zoop output 

library(tidyverse)
library(magrittr)

# Set wd to raw data file 
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

gv19_zp = zp_raw2 %>% select(sampleid,doy, taxon,group,biomass) %>% mutate(biomass = replace_na(biomass, 0))
as_tibble(gv19_zp)

# Total Biomass ####
# Set working directory to folder containing datasets derived from raw data 
setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/zooplankton")
gv_totalbiom <- gv19_zp %>%
  select(sampleid, biomass,group, doy) %>%
  group_by(sampleid, doy) %>%
  summarise(
    totalbiom = sum(biomass)) %>%
  ungroup()
as_tibble(gv_totalbiom)


# Zooplankton Community Spread ####
gv19_zpcomm <- gv19_zp %>% #sum the group biomasses 
  group_by(sampleid,doy, group) %>%
  summarise( 
    totalbiom= sum(biomass)) %>%
  ungroup()
as_tibble(gv19_zpcomm)

gv19_zpcomm <- gv19_zp %>% # Widen the dataframe
  group_by(sampleid,doy, group) %>%
  summarise( 
    totalbiom= sum(biomass)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(sampleid, doy),
              names_from = group, 
              values_from = totalbiom) %>% 
  as_tibble()
as_tibble(gv19_zpcomm)



# Stacked Area Graph #### 

forcats::fct_explicit_na

gv19_zp %<>% group_by(doy, group) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
gv19_zp

gv19_zp$group <- factor(gv19_zp$group, levels=c('Calanoid', 'Cyclopoid', 'Nauplii', 'Rotifer', 'Ostracod', 'LgCladocera', 'SmCladocera'))

# Plot
library(viridis)
library(hrbrthemes)

windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(gv19_zp, aes(x=doy, y=percentage, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  xlim(140, 255) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

gv19_zp2 <- gv19_zp %>%
  filter(!(doy == 157 | doy == 162))
# By ug/L 
windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
ggplot(gv19_zp2, aes(x=doy, y=totbiom, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Remove the ALM samples & DOy 157 
gv19_zp3 <- gv19_zp %>%
  filter(!(doy == 273 | doy == 157 | doy == 162 | doy == 211))
# By ug/L 
ggplot(gv19_zp3, aes(x=doy, y=totbiom, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(gv19_zp3, aes(x=doy, y=percentage, fill=group)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  xlim(140, 255) +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Stacked graph in base R to control colors 
gv19_zp2

gv19_zp2_long = gv19_zp2 %>%
  select(!(percentage)) %>%
  pivot_wider(names_from = doy, 
              values_from = totbiom) %>% 
  arrange(factor(group, levels = c('LgCladocera', 'SmCladocera', 'Rotifer', 'Ostracod', 'Calanoid', 'Cyclopoid', 'Nauplii'))) %>%
  as_tibble()
gv19_zp2_long
gv19_zp2_long %<>% select(!(group)) %>%
  as.data.frame()
row.names(gv19_zp2_long) <- c('LgCladocera', 'SmCladocera','Rotifer', 'Ostracod', 'Calanoid', 'Cyclopoid', 'Nauplii')
gv19_zp_stack <- as.matrix(gv19_zp2_long)
gv19_zp_stack

#Plot
# Cool Colors: '#00FEFF', '#59d8e6', '#00C8FF', '#009BDF', '#005BFF', '#0024FF', '#00305a'
windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv19_zp_stack, 
        col=c('#00FEFF', '#59d8e6', '#00C8FF', '#009BDF', '#799AE0', '#0024FF', '#00305a'), 
        border='black',
        ylim = c(0,300),
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('Zooplankton Biomass (ug/L)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Nauplii', 'Cyclopoid','Calanoid', 'Ostracod' ,'Rotifer',  'SmCladocera', 'LgCladocera'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#00305a', '#0024FF', '#799AE0', '#009BDF', '#00C8FF','#59d8e6', '#00FEFF' ))

# Density -------------------
# Turn raw R output into a cleaned zoop data
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
zp_rawden <- read.csv('2019_site4_gv_ZoopDensity_25Feb2021.csv')
as_tibble(zp_rawden)

zp_rawden <- zp_rawden %>% mutate(INDV.L = replace_na(INDV.L, 0)) %>% as_tibble() 
zp_rawden
zp_raw2 <- zp_rawden %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID) %>%
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(density = INDV.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

gv19_zp_dens = zp_raw2 %>% as_tibble()
gv19_zp_dens %<>% filter(!(doy == 162 | doy == 211 | doy == 157)) %>% as_tibble()

gv_totaldens <- gv19_zp_dens %>%
  select(sampleid, density,group, doy) %>%
  group_by(sampleid, doy) %>%
  summarise(
    totaldens = sum(density)) %>%
  ungroup()
as_tibble(gv_totaldens)
write.csv(gv_totaldens, 'gv19_totaldens.csv')

# Zooplankton Community by Density

