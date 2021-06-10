# GV Figure 1 - Zoop-Phyto Succession Visualization # 
rm(list=ls())

# Zooplankton Data # ==========================
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
  rename(doy = DOY) %>%
  filter(!(doy == 162 | doy == 157)) 
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 
as_tibble(zp_raw2)

gv19_zp = zp_raw2 %>% select(sampleid,doy, taxon,group,biomass) %>% mutate(biomass = replace_na(biomass, 0))
as_tibble(gv19_zp)

# Total Biomass 
# Set working directory to folder containing datasets derived from raw data 
setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/zooplankton")
gv_totalbiom <- gv19_zp %>%
  select(sampleid, biomass,group, doy) %>%
  group_by(sampleid, doy) %>%
  summarise(
    totalbiom = sum(biomass)) %>%
  ungroup() %>%
  arrange(doy)
as_tibble(gv_totalbiom)

# Zooplankton Community Spread #
gv19_zpcomm <- gv19_zp %>% #sum the group biomasses 
  group_by(sampleid,doy, group) %>%
  summarise( 
    totalbiom= sum(biomass)) %>%
  ungroup() %>%
  arrange(doy)
as_tibble(gv19_zpcomm)

gv19_zpcomm <- gv19_zp %>% # Widen the dataframe
  group_by(sampleid,doy, group) %>%
  summarise( 
    totalbiom= sum(biomass)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(sampleid, doy),
              names_from = group, 
              values_from = totalbiom) %>% 
  as_tibble() %>%
  arrange(doy)
as_tibble(gv19_zpcomm)

# Stacked graph in base R to control colors (biomass)======================


#biomass 
gv_totalbiom
sampleid = 'G19041157410'
doy = 157
totalbiom = 0
miss = data.frame(sampleid, doy, totalbiom)
miss
zp_biomassbar = gv_totalbiom %>%
  rbind(miss) %>%
  arrange(doy) %>%
  select(doy, totalbiom) %>%
  pivot_wider(names_from = doy, 
              values_from = totalbiom) %>%
  as.data.frame()

zp_biomassbar
row.names(zp_biomassbar) <- 'biomass'
zp_biomassbar.m = as.matrix(zp_biomassbar)
zp_biomassbar.m

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
barplot(zp_biomassbar.m, ylim = c(0,300),
        col = '#6E016B', space = 0.04,
        border = 'black', font.axis=2, las=2)
box()
mtext(text = expression('Zooplankton Biomass     g/L)'), side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('LgCladocera', 'SmCladocera','Rotifer', 'Ostracod', 'Calanoid', 'Cyclopoid', 'Nauplii'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c("#6E016B", "#88419D", "#8C6BB1", "#8C96C6" ,"#9EBCDA", "#BFD3E6", "#EDF8FB"))

# Stacked graph in base R to control colors (percentage) =========================
gv19_zp %<>% group_by(doy, group) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
gv19_zp

gv19_zp$group <- factor(gv19_zp$group, levels=c('Calanoid', 'Cyclopoid', 'Nauplii', 'Rotifer', 'Ostracod', 'LgCladocera', 'SmCladocera'))

gv19_zp
gv19_zp_long_p = gv19_zp %>%
  select(!(totbiom)) %>%
  pivot_wider(names_from = doy, 
              values_from = percentage) %>% 
  arrange(factor(group, levels = c('LgCladocera', 'SmCladocera', 'Rotifer', 'Ostracod', 'Calanoid', 'Cyclopoid', 'Nauplii'))) %>%
  as_tibble()
gv19_zp_long_p
gv19_zp_long_p %<>% select(!(group)) %>%
  as.data.frame()
row.names(gv19_zp_long_p) <- c('LgCladocera', 'SmCladocera','Rotifer', 'Ostracod', 'Calanoid', 'Cyclopoid', 'Nauplii')
gv19_zp_stack_p <- as.matrix(gv19_zp_long_p)
gv19_zp_stack_p

# Add in discongrous DOYs with Phytoplankton. Comment out write.csv file once created 
setwd("C:/Users/Owner/Box/Active/Active GV Research/Figures/Final")
#write.csv(gv19_zp_stack_p, 'gv19_zp_stack_perc.csv')
gv19_zp_stack = read.csv('gv19_zp_stack_perc.csv')
gv19_zp_stack %<>% select(!(X))
gv19_zp_stack.m = as.matrix(gv19_zp_stack)
gv19_zp_stack.m
gv19_zp_stack.m <- unname(gv19_zp_stack.m)
gv19_zp_stack.m
row.names(gv19_zp_stack.m) <- c('LgCladocera', 'SmCladocera',
                                   'Rotifer', 'Ostracod', 'Calanoid', 
                                   'Cyclopoid', 'Nauplii', 'Missing')

gv19_zp_stack.m
colnames(gv19_zp_stack.m) <- c(143, 150, 157, 164, 172, 178, 192, 199, 206,211,213,
                                  220, 227, 234, 245, 251, 273)
gv19_zp_stack.m

#Plot
display.brewer.all()
display.brewer.pal(7, 'BuPu')
rev(brewer.pal(7, 'BuPu'))
windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv19_zp_stack.m, 
        col=c("#6E016B", "#88419D", "#8C6BB1", "#8C96C6" ,"#9EBCDA", "#BFD3E6", "#EDF8FB", 'gray30'), 
        border='black',
        ylim = c(0,1),
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('Zooplankton Biomass (%)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
taxa = rev(c('LgCladocera', 'SmCladocera','Rotifer', 'Ostracod', 'Calanoid', 'Cyclopoid', 'Nauplii', 'NA'))
col= rev(c("#6E016B", "#88419D", "#8C6BB1", "#8C96C6" ,"#9EBCDA", "#BFD3E6", "#EDF8FB", 'gray30'))
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend = taxa, 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = col)



# Phytoplankton Data # =================================
#Packages required for data manipulation
library(tidyverse)
library(magrittr)
library(purrr)

# read in datasets 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Cleaned Data")
met = read.csv("met_gvl_2019_MatchEXOdoy.csv") #meteorological data from Creston station 
hf = read.csv("high_frequency_EXO3_gvl_2019.csv") #EXO sonde data
buoy = read.csv("Green-Valley_meta-thermo-buoy.csv") #buoyancy frequency data

setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton")
prephy <- read.csv('pre_phyto_comm.csv') # pre-incubation phytoplankton community 
postphy <- read.csv('post_phyto_comm.csv') # post-incubation phytoplankton community 

setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton")
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

# Community Data
library(tidyverse)
library(magrittr)
library(purrr)
library(forcats)

setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton")
prephy <- read.csv('pre_phyto_comm.csv') # pre-incubation phytoplankton community 
prephy <- as_tibble(prephy)
prephy %<>% rename(doy = X)
prephy$time <- c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7', 'week8', 'week9', 'week10', 'week11', 'week12', 'week13', 'week14')
prephy %<>% modify_if(is.character, as.factor)
prephy
prephy2 <- gather(prephy, key='species', value='biovolume', -c(doy, time))
prephy2 %<>% rename(taxon = species) # Need to add a division identifier 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Phytoplankton/2019 Green Valley Phytoplankton/Community data")
phydiv <- read.csv('phyto_taxon_trait_v2.csv')
prephy2
prephy3 <- left_join(prephy2, phydiv, by=c('taxon', 'doy'))
as_tibble(prephy3)

unique(prephy3$group) # Combine Chrysophyta, Cryptophyta, and Euglenophyta into other; Cyanophyta into other Cyanos 
prephy3$group <- gsub("Cryptophyta", "Other", prephy3$group)
prephy3$group <- gsub("Chrysophyta", "Other", prephy3$group)
prephy3$group <- gsub("Euglenophyta", "Other", prephy3$group)
prephy3$group <- gsub("Cyanophyta", "Misc_Cyanos", prephy3$group)
prephy3$group <- gsub('Cyanobacteria', "Misc_Cyanos", prephy3$group)
prephy3

phybiomass <- prephy3 %>% group_by(doy, group) %>%
  summarise(totbiom = sum(biovolume)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
phybiomass
unique(phybiomass$group)


phybiomass$division <- factor(phybiomass$group, levels=c('Bacillariophyta','Chlorophyta', 'Other', 
                                                         'Aphanizomenon', 'Aphanothece','Dolichospermum', 
                                                         'Microcystis', 'Misc_Cyanos'))
phybiomass = phybiomass %>% select(!(group))

# Stacked graph in base R to control colors (total)
pre_totalbiom
Treatment = c('Pre', 'Pre', 'Pre')
Day = c(211, 234, 273)
totalbiom = c(0,0,0)
miss = data.frame(Day, Treatment, totalbiom)
miss
phy_biomassbar = pre_totalbiom %>%
  rbind(miss) %>%
  arrange(Day) %>%
  select(Day, totalbiom) %>%
  pivot_wider(names_from = Day, 
              values_from = totalbiom) %>%
  as.data.frame()

phy_biomassbar
row.names(phy_biomassbar) <- 'biomass'
phy_biomassbar.m = as.matrix(phy_biomassbar)
phy_biomassbar.m

windows(height= 5, width = 8)
par(mai=c(0.9,1,0.6,1))
barplot(phy_biomassbar.m, ylim = c(0,400),
        col = '#238443', space = 0.04,
        border = 'black', font.axis=2, las=2)
box()
mtext(text = expression('Phytoplankton Biomass (mg/L)'), side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Write .csv and add in the discongruous DOYs that are in ZP but not PHY, create a 'missing' taxa
# Comment out following lines once csv file and dates are manually added 
setwd("C:/Users/Owner/Box/Active/Active GV Research/Figures/Final")
#write.csv(gv19_phyto_stack, 'gv19_phyto_stack_add.csv')
gv19_phyto_stack = read.csv('gv19_phyto_stack_add.csv')
gv19_phyto_stack %<>% select(!(X))
gv19_phyto_stack.m = as.matrix(gv19_phyto_stack)
gv19_phyto_stack.m
gv19_phyto_stack.m <- unname(gv19_phyto_stack.m)
gv19_phyto_stack.m
row.names(gv19_phyto_stack.m) <- c('Bacillariophyta','Chlorophyta', 'Other', 
                                'Aphanizomenon', 'Aphanothece','Dolichospermum', 
                                'Microcystis', 'Misc_Cyanos', 'Missing')

gv19_phyto_stack.m
colnames(gv19_phyto_stack.m) <- c(143, 150, 157, 164, 172, 178, 192, 199, 206,211,213,
                                  220, 227, 234, 245, 251, 273)
gv19_phyto_stack.m
#Plot
library(RColorBrewer)
display.brewer.pal(8, 'YlGn')
brewer.pal(8, 'YlGn')

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv19_phyto_stack.m, 
        col=c("#FFFFE5" ,"#F7FCB9", "#D9F0A3", "#ADDD8E" ,"#78C679" ,"#41AB5D" ,"#238443", "#005A32", 'gray90'), 
        border='black',
        ylim = c(0,400),
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('Phytoplankton Biomass (mg/L)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend =c('Bacillariophyta','Chlorophyta', 'Non-Cyanophytes', 
                           'Aphanizomenon', 'Aphanothece','Dolichospermum', 
                           'Microcystis', 'Other Cyanophytes', 'NA'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c("#FFFFE5" ,"#F7FCB9", "#D9F0A3", "#ADDD8E" ,"#78C679" ,"#41AB5D" ,"#238443", "#005A32", 'gray30'))

# Stacked graph in base R to control colors (percentage)
phybiomass

phypercent_long = phybiomass %>%
  select(!(totbiom)) %>%
  pivot_wider(names_from = doy, 
              values_from = percentage) %>% 
  arrange(factor(division, levels = c('Bacillariophyta','Chlorophyta', 'Other', 
                                      'Aphanizomenon', 'Aphanothece','Dolichospermum', 
                                      'Microcystis', 'Misc_Cyanos'))) %>%
  as_tibble()
phypercent_long
phypercent_long %<>% select(!c(division)) %>%
  as.data.frame()
row.names(phypercent_long) <- c('Bacillariophyta','Chlorophyta', 'Other', 
                                'Aphanizomenon', 'Aphanothece','Dolichospermum', 
                                'Microcystis', 'Misc_Cyanos')
gv19_phyto_stack_perc <- as.matrix(phypercent_long)
gv19_phyto_stack_perc

setwd("C:/Users/Owner/Box/Active/Active GV Research/Figures/Final")
#write.csv(gv19_phyto_stack, 'gv19_phyto_stack_add.csv')
gv19_phyto_stack = read.csv('gv19_phyto_stack_perc.csv')
gv19_phyto_stack %<>% select(!(X))
gv19_phyto_stack.m = as.matrix(gv19_phyto_stack)
gv19_phyto_stack.m
gv19_phyto_stack.m <- unname(gv19_phyto_stack.m)
gv19_phyto_stack.m
row.names(gv19_phyto_stack.m) <- c('Bacillariophyta','Chlorophyta', 'Other', 
                                   'Aphanizomenon', 'Aphanothece','Dolichospermum', 
                                   'Microcystis', 'Misc_Cyanos', 'Missing')

gv19_phyto_stack.m
colnames(gv19_phyto_stack.m) <- c(143, 150, 157, 164, 172, 178, 192, 199, 206,211,213,
                                  220, 227, 234, 245, 251, 273)
gv19_phyto_stack.m

#Plot
library(RColorBrewer)
display.brewer.pal(8, 'YlGn')
brewer.pal(8, 'YlGn')

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv19_phyto_stack.m, 
        col=c("#FFFFE5" ,"#F7FCB9", "#D9F0A3", "#ADDD8E" ,"#78C679" ,"#41AB5D" ,"#238443", "#005A32", 'gray30'), 
        border='black',
        ylim = c(0,1),
        space=0.04, 
        font.axis=2, 
        las=2)
box()
mtext('Phytoplankton Biomass (%)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Legend
div = rev(c('Bacillariophyta','Chlorophyta', 'Non-Cyanophytes', 
            'Aphanizomenon', 'Aphanothece','Dolichospermum', 
            'Microcystis', 'Other Cyanophytes', 'NA'))
col = rev(c("#FFFFE5" ,"#F7FCB9", "#D9F0A3", "#ADDD8E" ,"#78C679" ,"#41AB5D" ,"#238443", "#005A32", 'gray30'))
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend = div, 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = col)
