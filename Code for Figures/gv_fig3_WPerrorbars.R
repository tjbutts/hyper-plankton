# GV Excretion flux & percentage of nutrient stocks # 
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets") #desktop
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets") #desktop


library(tidyverse)
library(magrittr)

# load in data 
gv_exc = read_csv('gv19_excretion_alt.csv')
gv_stocks = read_csv('gv19_nutrientstocks.csv')

# Excretion rates compared to inorganic N and P (A); Excretion rates compared to inorganic N:P molar ratio (up or down?)
gv_exc
gv_stocks


gv_stocks %<>% select(doy, SRP_ugL, NOx_mgL, microcystin_ugL, orgP_ugL, orgN_mgL) # select variables we want to compare
gv_exc %<>% 
  mutate(Nexc_mid = Nexc_mid/1000, 
         Nexc_lo = Nexc_lo/1000, 
         Nexc_hi = Nexc_hi/1000) %>% #convert ug to mg
  as_tibble()
gv_frac = left_join(gv_exc, gv_stocks, by = 'doy')
gv_frac

gv_frac2 = gv_frac %>%
  mutate(Nexc_frac_mid = (Nexc_mid/NOx_mgL)*100, 
         Pexc_frac_mid = (Pexc_mid/SRP_ugL)*100, 
         Nexc_frac_lo = (Nexc_lo/NOx_mgL)*100, 
         Pexc_frac_lo = (Pexc_lo/SRP_ugL)*100,
         Nexc_frac_hi = (Nexc_hi/NOx_mgL)*100, 
         Pexc_frac_hi = (Pexc_hi/SRP_ugL)*100,) %>%
  select(doy, Nexc_frac_mid, Pexc_frac_mid, Nexc_frac_lo,
         Pexc_frac_lo, Nexc_frac_hi, Pexc_frac_hi,SRP_ugL, NOx_mgL) %>%
  as_tibble()
gv_frac2

plot(Nexc_frac_mid~doy, data = gv_frac2) #no frills plot 
# Nexc, in this case, is NH4 but I'm comparing it to inorganic N which is just NOx...I feel I can't do that but will for now...
plot(Pexc_frac_mid~doy, data = gv_frac2) #no frills plot 

# 2x1 plot; A = Nitrogen, B = Phosphorus
windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1), mfrow(2,1))

plot(gv_frac2$doy, gv_frac2$Nexc_frac_mid, type = "o", lwd = 2, 
     pch = 19, col = "dodgerblue3", ylim = c(0, 15), 
     ylab = "Excretion fraction of inorganic N (%)", xlab = "Day of Year, 2019", cex = 1.5, cex.lab = 1.5)
arrows(x0=gv_frac2$doy, y0=gv_frac2$Nexc_frac_hi, x1=gv_frac2$doy, y1=gv_frac2$Nexc_frac_lo, code=3, 
       angle=90, length=0.1, col='black', lwd=2)

plot(gv_frac2$doy, gv_frac2$Pexc_frac_mid, type = 'o', lwd = 2, 
     pch = 19, col = 'orchid2', ylim = c(0,150),
     ylab = 'Excretion fraction of inorganic P (%)', xlab = "Day of Year, 2019", cex = 1.5, cex.lab = 1.5)
arrows(x0=gv_frac2$doy, y0=gv_frac2$Pexc_frac_hi, x1=gv_frac2$doy, y1=gv_frac2$Pexc_frac_lo, code=3, 
       angle=90, length=0.1, col='black', lwd=2)

