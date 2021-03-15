#### Historical Plankton Data #### 
rm(list=ls())

library(tidyverse)
library(magrittr)


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

hist_phyto <- hist_plankton %>%
  select(Year, DOYFrac, Bacillariophyta, Chlorophyta, Cryptophyta, Chrysophyta, Cyanophyta, Dinophyta, Euglenophyta, Haptophyta) %>% 
  as_tibble()
hist_phyto
hist_phyto2 <- hist_phyto %>% cbind(doy_record$samp) %>% cbind(doy_record$DOY) %>% as_tibble()
str(hist_phyto2)
hist_phyto2 %<>% rename(c(samp = 'doy_record$samp', doy = 'doy_record$DOY'))
hist_phyto2 

# First Sampling date series 
samp1_phy <- hist_phyto2 %>% 
  filter(samp == 1) %>% 
  as_tibble()
samp1_phy
samp1_phy <- gather(samp1_phy, key = 'division', value = 'biomass', -c(Year, DOYFrac, doy, samp))
samp1_phy
samp1_phy2 <- samp1_phy %>% drop_na() %>% as_tibble()
samp1_phy2

library(viridis)
library(hrbrthemes)
library(forcats)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

ggplot(samp1_phy2, aes(x=doy, y=biomass, fill=division)) + # biomass
  scale_fill_viridis(discrete = T) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

samp1_phy_p <- samp1_phy2 %>% group_by(doy, division) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
samp1_phy_p # removes year 
samp1_phy_p$Year <- samp1_phy$Year[match(samp1_phy_p$doy, samp1_phy$doy)]
as_tibble(samp1_phy_p)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(samp1_phy_p, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Second Sampling date series 
samp2_phy <- hist_phyto2 %>% 
  filter(samp == 2) %>% 
  as_tibble()
samp2_phy
samp2_phy <- gather(samp2_phy, key = 'division', value = 'biomass', -c(Year, DOYFrac, doy, samp))
samp2_phy
samp2_phy2 <- samp2_phy %>% drop_na() %>% as_tibble()
samp2_phy2

library(viridis)
library(hrbrthemes)
library(forcats)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

ggplot(samp2_phy2, aes(x=doy, y=biomass, fill=division)) + # biomass
  scale_fill_viridis(discrete = T) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

samp2_phy_p <- samp2_phy2 %>% group_by(doy, division) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
samp2_phy_p # removes year 
samp2_phy_p$Year <- samp2_phy$Year[match(samp2_phy_p$doy, samp2_phy$doy)]
as_tibble(samp2_phy_p)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(samp2_phy_p, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Third Sampling date series 
samp3_phy <- hist_phyto2 %>% 
  filter(samp == 3) %>% 
  as_tibble()
samp3_phy
samp3_phy <- gather(samp3_phy, key = 'division', value = 'biomass', -c(Year, DOYFrac, doy, samp))
samp3_phy
samp3_phy2 <- samp3_phy %>% drop_na() %>% as_tibble()
samp3_phy2

library(viridis)
library(hrbrthemes)
library(forcats)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

ggplot(samp3_phy2, aes(x=doy, y=biomass, fill=division)) + # biomass
  scale_fill_viridis(discrete = T) +
  geom_bar(stat = 'identity') + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

samp3_phy_p <- samp3_phy2 %>% group_by(doy, division) %>%
  summarise(totbiom = sum(biomass)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
samp3_phy_p # removes year 
samp3_phy_p$Year <- samp3_phy$Year[match(samp3_phy_p$doy, samp3_phy$doy)]
as_tibble(samp3_phy_p)

# Plot
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))

# % Composition 
ggplot(samp3_phy_p, aes(x=doy, y=percentage, fill=division)) + 
  scale_fill_viridis(discrete = T) +
  geom_area(alpha=0.6 , size=1, colour="black") + 
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# zooplankton ======================================
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
