#### Historical Plankton Data #### 
rm(list=ls())

library(tidyverse)
library(magrittr)

# buffer # 

# Historical sampling date spread ####
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data") 
hist_alm <- read.csv('Historical_ALM.csv')

hist_alm <- hist_alm %>% 
  filter(Year > 2008) %>% 
  filter(analyte == 'Dissolved oxygen (DO)') %>%
  select(sampleDate,Year, DOY, DOYfrac)
as_tibble(hist_alm)
library(lubridate)
hist_alm <- hist_alm %>% 
  mutate(dttm = mdy_hm(sampleDate)) %>%
  as_tibble()
hist_alm

sampledate <- hist_alm %>%
  select(dttm, Year, DOY, DOYfrac) %>%
  mutate(yvalue = 1) %>%
  as_tibble()
sampledate

# Scatterplot of sampling date spread 
sampledate
spread <- sampledate %>% mutate(year = year(dttm), 
                      month = month(dttm),
                      day = day(dttm))
spread$monthday <- paste( month.abb[spread$month], spread$day, sep='-')
as_tibble(spread)
spread %<>% mutate(yvalue = rep(c(1.1,1.1,1.1,1.2,1.2,1.2,1.3,1.3,1.3,1.4,1.4,1.4,1.5,1.5,1.5,1.6,1.6,1.6,1.7,1.7,1.7,1.8,1.8,1.8,1.9,1.9,1.9), each=1))
as_tibble(spread)

windows(height=5, width=8)
par(mai=c(0.9,1,0.6,1))

plot(spread$DOY, spread$yvalue, pch=20, cex=2, xlim=c(121,273), ylab = 'arbitrary value', xlab = '2019 - Day of Year')
abline(v=c(60,91,121, 151, 181, 212, 243, 273, 304), lty=2)
text(135, 1.2, 'May', font=2)
text(195, 1.2, 'July', font=2)
text(230, 1.2, 'August', font=2)

firstsampdate <- spread %>% 
  filter(DOY < 200) %>% 
  summarise(avg = mean(DOY), 
            sd = sd(DOY)) %>% 
  as_tibble()
firstsampdate # 162 +/- 4 (closest 19 date = 164)

secondsampdate <- spread %>%
  filter(DOY > 170 & DOY < 220) %>%
  summarise(avg = mean(DOY), 
            sd = sd(DOY)) %>% 
  as_tibble()
secondsampdate # 212 +/- 4 (closest 19 date = 213)

thirdsampdate <- spread %>%
  filter(DOY >220) %>%
  summarise(avg = mean(DOY), 
            sd= sd(DOY)) %>%
  as_tibble()
thirdsampdate # 252 +/- 6 (closest 19 date = 251)

# Stacked Bar of Plankton at (DOY < 200; DOY > 170 < 220; DOY > 220) + Lake data #### 

setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
hist_plankton <- read.csv('Historic Plankton Data.csv')
as_tibble(hist_plankton)

hist_plankton <- hist_plankton %>% 
  filter(Year >2008) %>%
  as_tibble()
hist_plankton

setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data") 
hist_alm <- read.csv('Historical_ALM.csv')
as_tibble(hist_alm)
hist_alm <- hist_alm %>% 
  filter(Year > 2008) %>% 
  filter(analyte == 'Dissolved oxygen (DO)') %>%
  select(sampleDate,Year, DOY, DOYfrac)
as_tibble(hist_alm)
library(lubridate)


doy_record <- hist_alm %>% 
  mutate(dttm = mdy_hm(sampleDate)) %>%
  as_tibble() %>%
  select(Year, DOY)
list1 <- 1:27
list2 <- rep(c(1,2,3), length(list1))
doy_record <- cbind(doy_record, list2)
doy_record %<>% rename(samp = list2) %>% as_tibble(doy_record)
doy_record <- doy_record[1:27, ]
doy_record 

# phytoplankton #===============================================

setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
hist_plankton <- read.csv('Historic Plankton Data.csv')
as_tibble(hist_plankton)

hist_plankton <- hist_plankton %>% 
  filter(Year >2008) %>%
  as_tibble()
hist_plankton

setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data") 
hist_alm <- read.csv('Historical_ALM.csv')

hist_alm <- hist_alm %>% 
  filter(Year > 2008) %>% 
  filter(analyte == 'Dissolved oxygen (DO)') %>%
  select(sampleDate,Year, DOY, DOYfrac)
as_tibble(hist_alm)
library(lubridate)
hist_alm <- hist_alm %>% 
  mutate(dttm = mdy_hm(sampleDate)) %>%
  as_tibble()
hist_alm

doy_record <- hist_alm %>% 
  mutate(dttm = mdy_hm(sampleDate)) %>%
  as_tibble() %>%
  select(Year, DOY)
list1 <- 1:27
list2 <- rep(c(1,2,3), length(list1))
doy_record <- cbind(doy_record, list2)
doy_record %<>% rename(samp = list2) %>% as_tibble(doy_record)
doy_record <- doy_record[1:27, ]
doy_record 

hist_phyto <- hist_plankton %>%
  select(Year, DOYFrac, Bacillariophyta, Chlorophyta, Cryptophyta, Chrysophyta, Cyanophyta, Dinophyta, Euglenophyta, Haptophyta) %>% 
  as_tibble()
hist_phyto
hist_phyto2 <- hist_phyto %>% cbind(doy_record$samp) %>% cbind(doy_record$DOY) %>% as_tibble()
str(hist_phyto2)
hist_phyto2 %<>% rename(c(samp = 'doy_record$samp', doy = 'doy_record$DOY'))
hist_phyto2 

# Make dataset for analysis 
hist_phyto_dat <- hist_phyto2 %>%
  select(Year, doy, samp, Bacillariophyta, Chlorophyta, Cryptophyta, Chrysophyta, Cyanophyta, Dinophyta, Euglenophyta, Haptophyta) %>%
  as_tibble()
hist_phyto_dat

phyto_long <- hist_phyto_dat %>%
  pivot_longer(!c(Year, doy, samp), names_to = 'phyla', values_to = 'biomass') %>%
  as_tibble()
phyto_long

# Get total phyto biomass 
phyto_tot <- phyto_long %>% 
  group_by(Year, doy) %>%
  summarise(biomass = sum(biomass)) %>%
  as_tibble()
phyto_tot  

# Plot stoich  
windows(height=6, width=10)
par(mai=c(0.9,1,0.6,1))
phyto_tot <- as.data.frame(phyto_tot)

# plot tot phyto biomass 
plot(phyto_tot[phyto_tot$Year=='2011', "doy"], phyto_tot[phyto_tot$Year == "2011", "biomass"], 
     col = '#66c2a4', pch=17,type='o', cex=2, xlim=c(150,260), ylim=c(0, 500), lwd=4, 
     cex.axis=2, xlab='', ylab='')
points(phyto_tot[phyto_tot$Year=='2012', 'doy'], phyto_tot[phyto_tot$Year == '2012', 'biomass'],
       col = '#66c2a4', pch=15,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2013', 'doy'], phyto_tot[phyto_tot$Year == '2013', 'biomass'], 
       col = '#66c2a4', pch =19,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2014', 'doy'], phyto_tot[phyto_tot$Year == '2014', 'biomass'],
       col = '#238b45', pch=17,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2015', 'doy'], phyto_tot[phyto_tot$Year == '2015', 'biomass'], 
       col = '#238b45', pch=15,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2016', 'doy'], phyto_tot[phyto_tot$Year == '2016', 'biomass'],
       col = '#238b45', pch=19,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2017', 'doy'], phyto_tot[phyto_tot$Year == '2017', 'biomass'],
       col = '#00441b', pch=17,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2018', 'doy'], phyto_tot[phyto_tot$Year == '2018', 'biomass'],
       col = '#00441b', pch=15,type='o', cex=2, lwd=4)
points(phyto_tot[phyto_tot$Year=='2019', 'doy'], phyto_tot[phyto_tot$Year == '2019', 'biomass'],
       col = '#00441b', pch=19,type='o', cex=2, lwd=4)
mtext('Phytoplankton Biomass', side = 2, line=3, cex=2)
mtext('Julian Day of Year', side = 1, line =3, cex = 2)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topright", legend =c('2011', '2012', '2013','2014','2015','2016','2017','2018','2019'), 
       pch=c(17,15,19,17,15,19,17,15,19), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#66c2a4','#66c2a4','#66c2a4',
               '#238b45','#238b45','#238b45', 
               '#00441b','#00441b','#00441b'))
mtext("Year", at=0.2, cex=2)


# zooplankton ======================================
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
hist_plankton <- read.csv('Historic Plankton Data.csv')
as_tibble(hist_plankton)

hist_zoop <- hist_plankton %>%
  select(Year, DOYFrac, RotiferBiomass, Bosmina, Calanoida, Ceriodaphnia, Chydorus, Cyclopoida, Daphnia, Nauplii) %>% 
  as_tibble()
hist_zoop
hist_zoop2 <- hist_zoop %>% cbind(doy_record$samp) %>% cbind(doy_record$DOY) %>% as_tibble()
str(hist_zoop2)
hist_zoop2 %<>% rename(c(samp = 'doy_record$samp', doy = 'doy_record$DOY'))
hist_zoop2 # Need to combine SmCladocerans, LgCladocerans, Copepods; replace Daphnia NAs with 0s 
hist_zoop2[is.na(hist_zoop2)] <- 0 # replace the NAs in LgCladocerans with 0

hist_zoop2 <- hist_zoop2 %>% 
  mutate(SmCladocerans = rowSums(hist_zoop2[, c(4,7)])) %>%
  mutate(LgCladocerans = rowSums(hist_zoop2[, c(6,9)])) %>% 
  mutate(Copepods = rowSums(hist_zoop2[, c(5,8)])) %>% 
  as_tibble()
hist_zoop2 

hist_zoop2 %<>% select(Year, DOYFrac, doy, samp, LgCladocerans, SmCladocerans, Copepods, Nauplii, RotiferBiomass)


# First Sampling date series 
samp1_zp <- hist_zoop2 %>% 
  filter(samp == 1) %>% 
  as_tibble()
samp1_zp # 9 unique DOYs; 
samp1_zp <- gather(samp1_zp, key = 'division', value = 'biomass', -c(Year, DOYFrac, doy, samp))
samp1_zp
samp1_zp2 <- samp1_zp %>% drop_na() %>% as_tibble()
samp1_zp2

library(viridis)
library(hrbrthemes)
library(forcats)

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

ggplot(samp1_zp2, aes(x=doy, y=biomass, fill=division)) + # biomass
  scale_fill_viridis(discrete = T) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

samp1_zp_p <- samp1_zp2 %>% group_by(doy, division) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
samp1_zp_p # removes year 
samp1_zp_p$Year <- samp1_zp$Year[match(samp1_zp_p$doy, samp1_zp$doy)]
as_tibble(samp1_zp_p)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(samp1_zp_p, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Second Sampling date series 
samp2_zp <- hist_zoop2 %>% 
  filter(samp == 2) %>% 
  as_tibble()
samp2_zp
samp2_zp <- gather(samp2_zp, key = 'division', value = 'biomass', -c(Year, DOYFrac, doy, samp))
samp2_zp
samp2_zp2 <- samp2_zp %>% drop_na() %>% as_tibble()
samp2_zp2

library(viridis)
library(hrbrthemes)
library(forcats)

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

ggplot(samp2_zp2, aes(x=doy, y=biomass, fill=division)) + # biomass
  scale_fill_viridis(discrete = T) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

samp2_zp_p <- samp2_zp2 %>% group_by(doy, division) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
samp2_zp_p # removes year 
samp2_zp_p$Year <- samp2_zp$Year[match(samp2_zp_p$doy, samp2_zp$doy)]
as_tibble(samp2_zp_p)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(samp2_zp_p, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Third Sampling date series 
samp3_zp <- hist_zoop2 %>% 
  filter(samp == 3) %>% 
  as_tibble()
samp3_zp
samp3_zp <- gather(samp3_zp, key = 'division', value = 'biomass', -c(Year, DOYFrac, doy, samp))
samp3_zp
samp3_zp2 <- samp3_zp %>% drop_na() %>% as_tibble()
samp3_zp2

library(viridis)
library(hrbrthemes)
library(forcats)

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

ggplot(samp3_zp2, aes(x=doy, y=biomass, fill=division)) + # biomass
  scale_fill_viridis(discrete = T) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

samp3_zp_p <- samp3_zp2 %>% group_by(doy, division) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
samp3_zp_p # removes year 
samp3_zp_p$Year <- samp3_zp$Year[match(samp3_zp_p$doy, samp3_zp$doy)]
as_tibble(samp3_zp_p)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(samp3_zp_p, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# zooplankton cnp_Historical #=================================================
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
hist_plankton_cnp <- read.csv('zp_np_historical.csv')
as_tibble(hist_plankton_cnp)
hist_plankton_cnp %<>% filter(!(genus=='Ostracod'))

# Multiply C, N, and P by biomass to get C, N, and P storage weighted by biomass by taxon
zp_stoic_molar <- hist_plankton_cnp %>% 
  mutate(p_storage = (perc_p/100)*biomass) %>%
  mutate(n_storage = (perc_n/100)*biomass) %>% 
  mutate(c_storage = (perc_c/100)*biomass) %>% 
  mutate(n_molar = n_storage/14010000) %>% # Get molar ratio 
  mutate(p_molar = p_storage/30970000) %>% # Get molar ratio 
  select(sampledate, doy, year, genus, division, biomass, n_molar, p_molar) %>%
  as_tibble
zp_stoic_molar

# Get zooplankton community N:P by summing N and P storage for the community for a DOY 
zp_stoic_sum <- zp_stoic_molar %>% 
  group_by(doy, year, sampledate) %>%
  summarise(
    nstorage = sum(n_molar), 
    pstorage = sum(p_molar)) %>%
  ungroup() %>%
  as_tibble()
zp_stoic_sum

# Get Zooplankton average biomass 
zp_avgbiomass <- hist_plankton_cnp %>%
  group_by(doy, year) %>%
  summarise( 
    avg_biom = sum(biomass)) %>%
  ungroup() %>%
  as_tibble()
zp_avgbiomass

# Calculate community N:P 
zp_stoic_hist <- zp_stoic_sum %>% mutate(zp_np = (nstorage)/(pstorage)) 
zp_stoic_hist
zp_stoic_hist = zp_stoic_hist %>% filter(!(doy == 157 & year == 2019))

# Yearly avg N:P 
zp_avg <- zp_stoic_hist %>%
  group_by(year) %>% 
  summarise(
    avg_np = mean(zp_np)) %>%
  as_tibble()
zp_avg


# Plot stoich  
windows(height=6, width=10)
par(mai=c(0.9,1,0.6,1))
zp_stoic_hist <- as.data.frame(zp_stoic_hist)

plot(zp_stoic_hist[zp_stoic_hist$year=='2011', "doy"], zp_stoic_hist[zp_stoic_hist$year == "2011", "zp_np"], 
     col = '#66c2a4', pch=17,type='o', cex=2, xlim=c(140,280), ylim=c(12,41), lwd=4, 
     cex.axis=2, xlab='', ylab='')
points(zp_stoic_hist[zp_stoic_hist$year=='2012', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2012', 'zp_np'],
       col = '#66c2a4', pch=15,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2013', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2013', 'zp_np'], 
       col = '#66c2a4', pch =19,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2014', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2014', 'zp_np'],
       col = '#238b45', pch=17,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2015', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2015', 'zp_np'], 
       col = '#238b45', pch=15,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2016', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2016', 'zp_np'],
       col = '#238b45', pch=19,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2017', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2017', 'zp_np'],
       col = '#00441b', pch=17,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2018', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2018', 'zp_np'],
       col = '#00441b', pch=15,type='o', cex=2, lwd=4)
points(zp_stoic_hist[zp_stoic_hist$year=='2019', 'doy'], zp_stoic_hist[zp_stoic_hist$year == '2019', 'zp_np'],
       col = '#00441b', pch=19,type='o', cex=2, lwd=4)
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Julian Day of Year', side = 1, line =3, cex = 2)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('2011', '2012', '2013','2014','2015','2016','2017','2018','2019'), 
       pch=c(17,15,19,17,15,19,17,15,19), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#66c2a4','#66c2a4','#66c2a4',
               '#238b45','#238b45','#238b45', 
               '#00441b','#00441b','#00441b'))
mtext("Year", at=0.2, cex=2)

# Plot ZP Biomass 
windows(height=6, width=10)
par(mai=c(0.9,1,0.6,1))
zp_avgbiomass <- zp_avgbiomass %>% filter(!(doy == 157 & year == 2019))
zp_avgbiomass <- as.data.frame(zp_avgbiomass)

plot(zp_avgbiomass[zp_avgbiomass$year=='2011', "doy"], zp_avgbiomass[zp_avgbiomass$year == "2011", "avg_biom"], 
     col = '#66c2a4', pch=17,type='o', cex=2, xlim=c(140,280), ylim=c(0, max(zp_avgbiomass$avg_biom)), lwd=4, 
     cex.axis=2, xlab='', ylab='')
points(zp_avgbiomass[zp_avgbiomass$year=='2012', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2012', 'avg_biom'],
       col = '#66c2a4', pch=15,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2013', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2013', 'avg_biom'], 
       col = '#66c2a4', pch =19,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2014', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2014', 'avg_biom'],
       col = '#238b45', pch=17,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2015', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2015', 'avg_biom'], 
       col = '#238b45', pch=15,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2016', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2016', 'avg_biom'],
       col = '#238b45', pch=19,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2017', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2017', 'avg_biom'],
       col = '#00441b', pch=17,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2018', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2018', 'avg_biom'],
       col = '#00441b', pch=15,type='o', cex=2, lwd=4)
points(zp_avgbiomass[zp_avgbiomass$year=='2019', 'doy'], zp_avgbiomass[zp_avgbiomass$year == '2019', 'avg_biom'],
       col = '#00441b', pch=19,type='o', cex=2, lwd=4)
mtext('Zooplankton Biomass', side = 2, line=3, cex=2)
mtext('Julian Day of Year', side = 1, line =3, cex = 2)

# Legend
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("topleft", legend =c('2011', '2012', '2013','2014','2015','2016','2017','2018','2019'), 
       pch=c(17,15,19,17,15,19,17,15,19), 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#66c2a4','#66c2a4','#66c2a4',
               '#238b45','#238b45','#238b45', 
               '#00441b','#00441b','#00441b'))
mtext("Year", at=0.2, cex=2)

gv2019 = zp_stoic_hist %>%
  filter(year == 2019)
gv2019
windows(height=8.5, width=12)
par(mai=c(0.9,1,0.6,1))
plot(gv2019$doy, gv2019$zp_np, type = 'o', pch=17, col='black', cex=3, cex.axis=2 , lwd=4, xlab='', ylab='') 
mtext('Zooplankton N:P', side = 2, line=3, cex=2)
mtext('Day of Year, 2019', side = 1, line =3, cex = 2)
legend('topright', legend=c('Zoop N:P'), col = c('black'), cex=1.5, pt.cex=3, pch=c(17))
