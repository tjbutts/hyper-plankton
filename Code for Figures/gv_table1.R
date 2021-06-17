# GV Table 1 Info # 
library(tidyverse)

rm(list=ls())
setwd("C:/Users/Owner/Box/Green Valley Project/Tyler GVL")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Tyler GVL")
gvl = read.csv("GVL_nutrients_w_ALM.csv")
gvl

# Table ========================================
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
  filter(Year > 2010) %>%
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


# Figure =============================

setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Final Data/Historical Data")

alm=read.csv("Historical_ALM.csv") # Total GV Data 2000 - 2019 
alm = alm %>% filter(Year >2010)

# replace 0s and below detect lim with 1/2 LRL
unique(alm$analyte)

gvl19$SRP_ugL<-replace(gvl19$SRP_ugL, gvl19$SRP_ugL<3.9, 3.9)
gvl19$SRP_ugL
gvl19$NOx_mgL<-replace(gvl19$NOx_mgL, gvl19$NOx_mgL<0.1, 0.1)
gvl19$NOx_mgL
gvl19$NHx <- replace(gvl19$NHX, gvl19$NHX<2.4, 2.4)


windows(height=8, width=10)
par(mfrow=c(3,3), omi=c(0.1,0.1,0.1,0.1), mai=c(0.5,1,0.1,0.1))

#Create Transparent Color Palette (use col2rgb() to transform fave color to RGB values)
ncol <- rgb(30,144,255, max = 255, alpha = 70, names = "blue_transp")
pcol <- rgb(147, 112, 219, max = 255, alpha = 70, names = "purple_transp")
fcol <- rgb(139, 117, 0, max = 255, alpha = 70, names = "tan_transp")
ccol <- rgb(60,179,113, max = 255, alpha = 70, names = "green_transp")

## Nitrogen ##
#Total Nitrogen
plot(alm[alm$analyte=="Total Nitrogen", "DOYFrac"], alm[alm$analyte=="Total Nitrogen", "result"], pch=19, col=ncol, cex=2.5, xlab="", cex.axis=1.5, xlim=c(2010,2020), ylab=expression(Total~N~"("*mg~L^-1*")"), cex.lab=2)
lines(c(2011,2019), c(2.11, 2.11), lwd=4, col="dodgerblue4")

#Nitrate
plot(alm[alm$analyte=="Nitrate", "DOYFrac"], alm[alm$analyte=="Nitrate", "result"], pch=19, col=ncol, cex=2.5, xlab="", cex.axis=1.5, xlim=c(2010,2020), ylab=expression(Nitrate~"("*mg~L^-1*")"), cex.lab=2)
lines(c(2011,2019), c(0.46, 0.46), lwd=4, col="dodgerblue4")

#Amonnium
plot(alm[alm$analyte=="Ammonia-nitrogen (as N)", "DOYFrac"], alm[alm$analyte=="Ammonia-nitrogen (as N)", "result"], pch=19, col=ncol, cex=2.5, xlab="", cex.axis=1.5, xlim=c(2010,2020), ylab=expression(Ammonia~"("*mg~L^-1*")"), cex.lab=2)
lines(c(2011,2019), c(0.107, 0.107), lwd=4, col="dodgerblue4")

## Phosphorus ##
#Total PHosphorus
plot(alm[alm$analyte=="Total Phosphorus", "DOYFrac"], alm[alm$analyte=="Total Phosphorus", "result"]*1000, pch=19, col=pcol, cex=2.5, xlab="", cex.axis=1.5, xlim=c(2010,2020),ylab=expression(Total~P~"("*mu*g~L^-1*")"), cex.lab=2)
lines(c(2011,2019), c(0.239*1000, 0.239*1000), lwd=4, col="mediumpurple4")

#Soluble Reactive P
plot(alm[alm$analyte=="Orthophosphate (as P)", "DOYFrac"], alm[alm$analyte=="Orthophosphate (as P)", "result"]*1000, pch=19, col=pcol, cex=2.5, xlab="", cex.axis=1.5, xlim=c(2010,2020),ylab=expression(Soluble~P~"("*mu*g~L^-1*")"), cex.lab=2)
lines(c(2011,2019), c(0.0963*1000, 0.0963*1000), lwd=4, col="mediumpurple4")

#Fixed Suspended Solids
plot(alm[alm$analyte=="Fixed suspended solids", "DOYFrac"], alm[alm$analyte=="Fixed suspended solids", "result"], pch=19, col=pcol, cex=2.5, xlab="", cex.axis=1.5, xlim=c(2010,2020),ylab=expression(Inorg~Part~"("*mg~L^-1*")"), cex.lab=2)
lines(c(2011,2019), c(7.22, 7.22), lwd=4, col="mediumpurple4")

## Plankton ## 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
plank = read.csv("Historic Plankton Data.csv")
plank = plank %>% filter(Year > 2010)

# Zooplankton
plot(plank$DOYFrac, log10(plank$ZoopBiomass), col=fcol, pch=19, cex=2.5, xlab="", yaxt ='n', cex.axis=1.5, xlim=c(2010,2020),ylab=expression(Zooplankton~"("*mu*g~L^-1*")"), cex.lab=2, ylim=c(log10(1), log10(1000)))
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),
                  log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90),
                  log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000)), 
     labels=c("1","","","","","","","","","10","","","","","","","","","100","","","","","","","","","1000"), las=2)
lines(c(2011,2019), c(log10(135.74), log10(135.74)), lwd=4, col="gold4")

# Phytoplankton 
plot(plank$DOYFrac, log10(plank$PhytoBiomass), col=ccol, pch=19, cex=2.5, yaxt="n", xlab="", cex.axis=1.5, xlim=c(2010,2020),ylab=expression(Phytoplankton~"("*mg~L^-1*")"), cex.lab=2, ylim=c(log10(1), log10(2000)))
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90), log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000)), 
     labels=c("1","","","","","","","","","10","","","","","","","","","100","","","","","","","","","1000"), las=2)
lines(c(2011,2019), c(log10(266.07), log10(266.07)), lwd=4, col="seagreen4")

# Cyanophyta
plot(plank$DOYFrac, log10(plank$Cyanophyta), col=ccol, pch=19, cex=2.5, yaxt="n", xlab="", cex.axis=1.5, xlim=c(2010,2020),ylab=expression(Cyanophyta~"("*mg~L^-1*")"), cex.lab=2, ylim=c(log10(1), log10(2000)))
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90), log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000)), 
     labels=c("1","","","","","","","","","10","","","","","","","","","100","","","","","","","","","1000"), las=2)
lines(c(2011,2019), c(log10(218), log10(218)), lwd=4, col="seagreen4")

