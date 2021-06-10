# GV Figure 3 - Excretion flux & percentage of nutrient stocks # 
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets") #desktop
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets") #desktop


library(tidyverse)
library(magrittr)

# load in data 
gv_exc = read_csv('gv19_excretion.csv')
gv_stocks = read_csv('gv19_nutrientstocks.csv')

# Excretion rates compared to inorganic N and P (A); Excretion rates compared to inorganic N:P molar ratio (up or down?)
gv_exc
gv_stocks


gv_stocks %<>% select(doy, SRP_ugL, NOx_mgL, microcystin_ugL, orgP_ugL, orgN_mgL) # select variables we want to compare
gv_exc %<>% select(doy, N_excrete_sum_d, P_excrete_sum_d) %>%
  rename(Nexc = N_excrete_sum_d, 
         Pexc = P_excrete_sum_d) %>% 
  mutate(Nexc = Nexc/1000) %>% #convert ug to mg
  as_tibble()
gv_frac = left_join(gv_exc, gv_stocks, by = 'doy')
gv_frac

gv_frac2 = gv_frac %>%
  mutate(Nexc_frac = (Nexc/NOx_mgL)*100, 
         Pexc_frac = (Pexc/SRP_ugL)*100) %>%
  select(doy, Nexc_frac, Pexc_frac, SRP_ugL, NOx_mgL) %>%
  as_tibble()
gv_frac2
# Make a dataframe reflecting the NA for DOY 157 and copy doy 171 to make a DOY 172 to be consistent with fig. 2
doy = c(157, 172)
Nexc_frac = c(NA, 0.156)
Pexc_frac = c(NA, 9.94)
SRP_ugL = c(NA, 3.9)
NOx_mgL = c(NA, 1.02)
replace = data.frame(doy, Nexc_frac, Pexc_frac, SRP_ugL, NOx_mgL)
replace

gv_frac2 = rbind(gv_frac2, replace)
gv_frac2
gv_frac2 %<>% arrange (doy) %>% filter(!(doy == 171))
gv_frac2

plot(Nexc_frac~doy, data = gv_frac2) #no frills plot 
# Nexc, in this case, is NH4 but I'm comparing it to inorganic N which is just NOx...I feel I can't do that but will for now...
plot(Pexc_frac~doy, data = gv_frac2) #no frills plot 

# 2x1 plot; A = Nitrogen, B = Phosphorus
windows(height= 12, width = 10)
par(mai=c(0.9,1,0.6,1), mfrow = c(2,1))

# Need better y-axis name and a barplot makes more sense here 
plot(gv_frac2$doy, gv_frac2$Nexc_frac, type = "o", lwd = 2, 
     pch = 19, col = "dodgerblue3",
     ylab = "Excretion fraction of inorganic N (%)", xlab = "Day of Year, 2019", cex = 1.5, cex.lab = 1.5)
plot(gv_frac2$doy, gv_frac2$Pexc_frac, type = 'o', lwd = 2, 
     pch = 19, col = 'orchid2', 
     ylab = 'Excretion fraction of inorganic P (%)', xlab = "Day of Year, 2019", cex = 1.5, cex.lab = 1.5)

# Barplot 
gv_frac2

windows(height= 12, width = 10)
par(mai=c(0.9,1,0.6,1), mfrow = c(2,1))

#N
Nexc_plot = gv_frac2 %>%
  select(doy, Nexc_frac) %>%
  pivot_wider(names_from = doy, 
              values_from = Nexc_frac) %>%
  as.data.frame()

Nexc_plot
row.names(Nexc_plot) <- 'Nexcretion_precent'
Nexc_plot.m = as.matrix(Nexc_plot)
Nexc_plot.m

barplot(Nexc_plot.m, ylim = c(0,10),
        col = 'dodgerblue2', space = 0.04,
        border = 'black', font.axis=2, las=2)
box()
mtext('Fraction of inorganic N (%)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

#P
Pexc_plot = gv_frac2 %>%
  select(doy, Pexc_frac) %>%
  pivot_wider(names_from = doy, 
              values_from = Pexc_frac) %>%
  as.data.frame()

Pexc_plot
row.names(Pexc_plot) <- 'Pexcretion_precent'
Pexc_plot.m = as.matrix(Pexc_plot)
Pexc_plot.m

barplot(Pexc_plot.m, ylim = c(0,50),
        col = 'orchid2', space = 0.04,
        border = 'black', font.axis=2, las=2)
box()
mtext('Fraction of inorganic P (%)', side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)

# Side plot of inorganic N & P dynamics over the course of the summer 
gv_stocks

windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1))

inorg = gv_stocks %>% 
  select(doy, SRP_ugL, NOx_mgL) %>%
  mutate(N = NOx_mgL*1000) %>%
  rename(P = SRP_ugL) %>%
  select(doy, N, P) %>%
  as_tibble()
inorg

# Time Series # 
windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1))

plot(inorg$doy, log10(inorg$N), type = "o", lwd = 2, yaxt='n', xaxt='n', xlim = c(143, 273), 
     pch = 19, col = "dodgerblue3", ylim = c(log10(1), log10(2000)),
     ylab = "", xlab = "Day of Year, 2019", cex = 1.5, cex.lab = 1.5)
mtext('Inorganic Nutrient Concentration (ug/L)', side = 2, line = 3.5, cex = 1.5)
points(inorg$doy, log10(inorg$P), 
       type = "o", lwd = 2, col = "orchid2", pch = 19, cex = 1.5)
legend("bottomright", legend = c("Inorganic N", "Inorganic P"),
       pch = 19, pt.cex = 3, col = c('dodgerblue2', 'orchid2'), cex =2)
axis(side=2,
     at=c(log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000), log10(2000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000','2000'), las=2, font = 2)
axis(side=1, 
     at=c(143, 150, 157, 164, 172, 178, 192, 199, 206,211,213,
            220, 227, 234, 245, 251, 273), 
     labels = c('143', '150', '157', '164', '172', '178', '192', '199', '206','211','213',
                '220', '227', '234', '245', '251', '273'), las=2, font.axis=2)

# Barplot # 
inorg

inorg_long = inorg %>%
  pivot_longer(!doy, names_to = "nutrient", values_to = "conc") %>%
  as.data.frame()
inorg_long

windows(height= 6, width = 10)
par(mai=c(0.9,1,0.6,1))
barplot(log10(inorg_long$conc), ylim = c(log10(1),log10(2000)),
        col = c('dodgerblue2', 'orchid2'), space = 0.04,
        border = 'black', font.axis=2, las=2, yaxt= 'n', beside = T)
box()
mtext(text = expression('Inorganic Nutrient Concentration'~'('~mu~'g/L)'), side = 2, line=3, cex=1.5)
mtext('Day of Year, 2019', side =1, line = 3.5, cex=1.5)
axis(side=2,
     at=c(log10(1),
          log10(2),log10(3),log10(4),log10(5),log10(6),log10(7),log10(8),log10(9),log10(10),
          log10(20),log10(30),log10(40),log10(50),log10(60),log10(70),log10(80),log10(90),log10(100),
          log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900), log10(1000), log10(2000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','','','','','','','','','100', '', '', '','','','','','','1000','2000'), las=2)
axis(side=1, 
     at=c(143, 150, 157, 164, 172, 178, 192, 199, 206,211,213,
          220, 227, 234, 245, 251, 273), 
     labels = c('143', '150', '157', '164', '172', '178', '192', '199', '206','211','213',
                '220', '227', '234', '245', '251', '273'), las=2, font.axis=2)
