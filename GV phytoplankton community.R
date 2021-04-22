#### GV phytoplankton community #####
rm(list=ls())
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

# Phytoplankton biomass from EXO Sonde #### 
hfDaily = hf %>%
  group_by(doy) %>%
  summarize(chl = mean(chl, na.rm = T),
            spcond = mean(spcond, na.rm = T),
            do_sat = mean(odo_sat, na.rm = T),
            do_conc = mean(odo, na.rm = T),
            pc = mean(pc, na.rm = T),
            ph = mean(ph, na.rm = T),
            temp = mean(temp, na.rm = T),
            algae = mean(chl + pc, na.rm = T)) %>% 
  ungroup() 
hfDaily = as.data.frame(hfDaily)
as_tibble(hfDaily)

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(hfDaily$doy, hfDaily$algae, type = "l", pch = 20, col='#008837', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Total Algae (EXO Sonde)", lwd=4 )

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(hfDaily$doy, hfDaily$temp, type = "l", pch = 20, col='dodgerblue3', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Surface Temperature", lwd=4 )

# plot 
windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
incub_temp <- c(18,19,24,24,22,22,26,27,26,26,26,26,23,23)
plot(pre_totalbiom$Day, incub_temp, type = "o", pch = 1 ,col='dodgerblue3', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Incubation Water Temp", lwd=4 )


# find Y coordinates for those dates 
exophy <- hfDaily %>% 
  select(doy, algae) %>% 
  filter(doy ==143| 
           doy ==150|
           doy ==157|
           doy ==164| 
           doy ==172| 
           doy ==178|
           doy ==192|
           doy ==199|
           doy ==206|
           doy ==213|
           doy ==220|
           doy ==227|
           doy ==245|
           doy ==251)
as_tibble(exophy)
points(143,28.4, pch=20, lwd=10,col='black')
points(150,30.4, pch=20, lwd=10,col='black')
points(157, 21.8, pch=20, lwd=10,col='black')
points(164,18.4, pch=20, lwd=10,col='black')
points(171,19.5, pch=20, lwd=10,col='black')
points(178,17.0, pch=20, lwd=10,col='black')
points(192,29.5, pch=20, lwd=10,col='black')
points(199,23.8, pch=20, lwd=10,col='black')
points(206,24.7, pch=20, lwd=10,col='black')
points(213,24.2, pch=20, lwd=10,col='black')
points(220,24.0, pch=20, lwd=10,col='black')
points(227,18.6, pch=20, lwd=10,col='black')
points(245,18.0, pch=20, lwd=10,col='black')
points(251,20.9, pch=20, lwd=10,col='black')

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(hfDaily$doy, hfDaily$algae, type = "l", pch = 20, col='#008837', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Daily Total Algae (EXO Sonde)", lwd=4 )
points(exophy$doy, exophy$algae, type = 'o', pch=20,cex=3,col='black', lwd=4)
legend("topright", legend=c(expression('Chlorophyll-' *italic(a)* ' from EXO Sonde'), 'Study Sampling Dates'), col=c("#008837", "black"), cex=1.5, pt.cex=3, pch=20, bg="white")

# Phytoplankton Total Biomass pre ####
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

# plot 
windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(pre_totalbiom$Day, pre_totalbiom$totalbiom, type = "o", pch = 1 ,col='#008837', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Algae biomass (mg/L) - Incubation", lwd=4 )

# EXO v. incubation 
windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(hfDaily$doy, hfDaily$algae, type = "p", pch = 20, col='mediumseagreen', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Total Algae (EXO Sonde)", lwd=4 )
par(new = T)
plot(pre_totalbiom$Day, pre_totalbiom$totalbiom, type = 'p', pch=17, col='black', cex=3, axes=F, xlab='', ylab='', xlim=c(134,273))
axis(side=4, at = c(0,50,100,150,200,250,300), cex=3, cex.axis=1.5, cex.lab=1.5)
mtext('Phytoplankton biomass (mg/L)', side=4, line=3, cex=1.5)
legend("topright", legend=c('EXO Sonde Total Algae', 'Incubation phyto biomass'), col=c("mediumseagreen",'black'), cex=1, pt.cex=2, pch=c(20,17), bg="white")


# Species Rate of Change - might not be as useful as Diamond Turnover #### 
setwd("C:/Users/Owner/Box/Active/Butts_GV Manuscript")
rateochange <- read.csv('rateochange_phy.csv')
as_tibble(rateochange) # need to make column titles more r friendly/informative 

rateochange %<>% rename(c(sum_rate = Summed.rate.of.change, time_diff = t1.t2, rate_change = Rate.of.Change)) %>%
  select(-Notes) %>% 
  as_tibble()
rateochange$doy <- c(150, 157, 164, 172, 178, 192, 199, 206, 213, 220, 227, 245, 251) # add an x-variable 

windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(rateochange$doy, rateochange$rate_change, type='o', pch = 20, col='black', cex=3, xlim = c(140, 255), cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Succession Rate (per day)", lwd=4 )

# Turnover + Rank Shift + Rate Change ####
library(codyn)

# turnover() calculates total turnover as well as the proportion of species 
## that either appear or disappear between time points.

# rank_shift() quantifies relative changes in species rank abundances by taking the sum difference 
## of species ranks in consecutive time points. This metric goes hand-in-hand with "rank clocks," 
## a useful visualization tool of shifts in species ranks.

# rate_change() analyzes differences in species composition between samples at increasing time lags. 
## It reflects the rate of directional change in community composition. The associated function rate_change_interval() 
## returns the full set of community distance values and associated time lag intervals to use in visualization.

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
prephy2 # Need to add a division identifier 
phydiv <- read.csv('phy_taxon_division.csv')
prephy3 <- left_join(prephy2, phydiv, by='species')
as_tibble(prephy3)

# Turnover # 
gv19_appearance <- turnover(df = prephy3, 
                          time.var = 'doy', 
                          species.var = 'species', 
                          abundance.var = 'biovolume', 
                          metric = 'appearance')
as_tibble(gv19_appearance)

gv19_appearance %<>% mutate(p = appearance*100) %>% as_tibble()
gv19_appearance


windows(height=8.5, width=12)
plot(gv19_appearance$doy, gv19_appearance$p, type='o', pch = 20, col='black', cex=3, cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Species that appear between time points (%)", lwd=4 )


gv19_disappearance <- turnover(df = prephy3, 
                               time.var = 'doy', 
                               species.var = 'species', 
                               abundance.var = 'biovolume', 
                               metric = 'disappearance')
as_tibble(gv19_disappearance)
gv19_disappearance %<>% mutate(p = disappearance*100) %>% as_tibble()
gv19_disappearance

windows(height=8.5, width=12)
plot(gv19_disappearance$doy, gv19_disappearance$p, type='o', pch = 20, col='black', cex=3, cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Species that disappear between time points (%)", lwd=4 )



# Rank Shifts 

# create dataset by group # 
prephy4 <- prephy3 %>%  group_by(doy, division) %>%
  summarise(totbiom = sum(biovolume)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
prephy4

prephy3$division <- factor(prephy3$division, levels=c('Bacillariophyta','Chlorophyta', 'Cryptophyta', 'Cyanophyta', 'Euglenaphyta'))


# traditional visualization can be confusing 
ggplot(prephy4, aes(doy, totbiom, color = division)) + 
  geom_line(size = 2) + theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# rank clock highlights differences in the stability of species 
ggplot(prephy4, aes(doy, totbiom, color = division)) + 
  geom_line(size = 2) + coord_polar() + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  
# Large reordering in relative abundance of phytoplankton division around DOY 200 

# Mean rank shift # 
# rank_shift describes the relative changes in species rank abundances, which indicates the degree of species reordering 
## between two time points - requires a column for species, time, and abundance 

gv19_rankshift <- rank_shift(df=prephy3,
                            time.var = "doy",
                            species.var = "species",
                            abundance.var = "biovolume")
gv19_rankshift
#Select the final time point from the returned time.var_pair
gv19_rankshift$doy <- as.numeric(substr(gv19_rankshift$year_pair, 5,7))
as_tibble(gv19_rankshift)

# Plot it
ggplot(gv19_rankshift, aes(doy, MRS)) + 
  geom_line(size= 2, color = 'mediumseagreen') + theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Calculating mean rank shifts highlights that the stability of communities diverged at Konza around 1992, with fewer rank 
## shifts between species in the burned relative to the unburned are

# Phytoplankton Stacked Area Graph ####
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
phydiv <- read.csv('phyto_taxon_trait.csv')
prephy2
prephy3 <- left_join(prephy2, phydiv, by=c('taxon', 'doy'))
as_tibble(prephy3)

prephy3 %<>%  group_by(doy, group) %>%
  summarise(totbiom = sum(biovolume)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
prephy3

prephy3$division <- factor(prephy3$group, levels=c('Bacillariophyta','Chlorophyta', 'Cryptophyta', 'Chrysophyta', 'Cyanophyta', 'Euglenophyta'))

# Plot
library(viridis)
library(hrbrthemes)

windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(prephy3, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
# By mg/L 
ggplot(prephy3, aes(x=doy, y=totbiom, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Stacked graph in base R to control colors 
prephy3

prephy3_long = prephy3 %>%
  select(!(percentage)) %>%
  pivot_wider(names_from = doy, 
              values_from = totbiom) %>% 
  arrange(factor(group, levels = c('Bacillariophyta', 'Chlorophyta', 'Cryptophyta', 'Chrysophyta', 'Cyanophyta', 'Euglenophyta'))) %>%
  as_tibble()
prephy3_long
prephy3_long %<>% select(!c(group, division)) %>%
  as.data.frame()
row.names(prephy3_long) <- c('Bacillariophyta', 'Chlorophyta', 'Cryptophyta', 'Chrysophyta', 'Cyanophyta', 'Euglenophyta')
gv19_phyto_stack <- as.matrix(prephy3_long)
gv19_phyto_stack

#Plot
# Green Pallette: '#00261C', '#044D29', '#168039', '#A8C545', '#45BF55', '#96ED89'

windows(height=5, width=8)
par(mai=c(1,1.1,.6,.6))
barplot(gv19_phyto_stack, 
        col=c('#00261C', '#044D29', '#168039', '#A8C545', '#45BF55', '#96ED89'), 
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
legend("center", legend =c('Euglenophyta', 'Cyanophyta','Chrysophyta', 'Cryptophyta', 'Chlorophyta','Bacillariophyta'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#96ED89', '#45BF55', '#A8C545', '#168039', '#044D29','#00261C'))

# Cyanophyte Diversity 
setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton")
prephy <- read.csv('pre_phyto_comm.csv') # pre-incubation phytoplankton community 
prephy <- as_tibble(prephy)
prephy %<>% rename(doy = X)
prephy$time <- c('week1', 'week2', 'week3', 'week4', 'week5', 'week6', 'week7', 'week8', 'week9', 'week10', 'week11', 'week12', 'week13', 'week14')
prephy %<>% modify_if(is.character, as.factor)
prephy
prephy2 <- gather(prephy, key='species', value='biovolume', -c(doy, time))
prephy2 # Need to add a division identifier 
phydiv <- read.csv('phy_taxon_division.csv')
prephy3 <- left_join(prephy2, phydiv, by='species')
as_tibble(prephy3)

# Drop non-cyanophyte rows 
prephy4 <- prephy3[!(prephy3$division=='Bacillariophyta' | 
                       prephy3$division=='Chlorophyta' | 
                       prephy3$division=='Chrysophyta' |
                       prephy3$division=='Cryptophyta' | 
                       prephy3$division=='Euglenophyta'), ]

prephy4 %<>% group_by(doy, division_complex) %>%
  summarise(totbiom = sum(biovolume)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
prephy4

prephy4$division_complex <- factor(prephy4$division_complex, levels=c('Microcystis_sing',
                                                              'Microcystis',
                                                              'Cyanophyta_fil', 
                                                              'Cyanophyta_col', 
                                                              'Aphanothece'))



# Plot
library(viridis)
library(hrbrthemes)

doy <- pre_totalbiom$Day
doy <- as.data.frame(doy)

windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(prephy4, aes(x=doy, y=percentage, fill=division_complex)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))
# By mg/L 
ggplot(prephy4, aes(x=doy, y=totbiom, fill=division_complex)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# Cyanofluor Measurement #### 

# Cyanofluor # 
setwd("C:/Users/Owner/Box/Green Valley Project/Plankton Data/phytoplankton") 
gv19_cyanofl <- read.csv('gv_incubation_cyanofluor.csv')
as_tibble(gv19_cyanofl)

pre_cyanofl <- gv19_cyanofl %>%
  filter(incubation == "pre") %>% as_tibble()
pre_cyanofl

# plot 
windows(height=8.5, width=12)
par(mar=c(5,5,5,5)+0.3)
plot(pre_cyanofl$doy, pre_cyanofl$chl_cyano, type = "o", pch = 1 ,col='mediumseagreen', cex=3,cex.axis=1.5,cex.lab=1.5, xlab="Day of Year", ylab="Cyanofluor RFU", lwd=4, ylim=c(1000, 20000), xlim=c(143,273))
points(pre_cyanofl$doy, pre_cyanofl$pc_cyano, type = "o", pch = 1 ,col='dodgerblue3', cex=3,cex.axis=1.5,cex.lab=1.5, lwd=4)
par(new = TRUE)
plot(pre_cyanofl$doy, pre_cyanofl$mg.L, type = "o", pch = 1 ,col='black', axes=F, xlab='',ylab='' ,lwd=4)
axis(side=4, at = c(0,50,100,150,200,250,300), cex=3, cex.axis=1.5, cex.lab=1.5)
mtext('Phytoplankton biomass (mg/L)', side=4, line=3, cex=1.5)
legend("topright", legend=c(expression('Chlorophyll-' *italic(a)* ' RFUs'), 'Phycocyanin RFUs', 'Pyto biomass (mg/L)'), col=c("mediumseagreen",'dodgerblue3' , "black"), cex=1.5, pt.cex=3, pch=20, bg="white")

