# Turnover of P by zooplankton # 
setwd("C:/Users/Tyler/Box Sync/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets") #desktop
setwd("C:/Users/Owner/Box/Butts_Scripts/GV Grazing/chapter1-plankton-func/Derived Datasets")


library(tidyverse)
library(magrittr)

# load in data 
gv_exc = read_csv('gv19_excretion.csv')
gv_stocks = read_csv('gv19_nutrientstocks.csv')

gv_exc
gv_stocks

# Turnover 
## Assuming excretion of soluble nutrients by zooplankton was contributing soluble phosphate to the water column 
## and transforming particulate (total) phosphorus into soluble forms, can calculate turnover times of these 
## nutrient pools by dividing the average water column concentration by the excretion rates of zooplankton (water column concentration/excretion rate)

# Select correct stocks 
# Only doing P since can't directly compare NH4 excretion as did not take weekly NH4 
gv_watercolumn = gv_stocks %>% 
  select(doy, SRP_ugL, TP_ugL)
gv_exc_turn = gv_exc %>%
  select(doy, P_excrete_sum_d)

gv_turnover = left_join(gv_watercolumn, gv_exc_turn, by = 'doy')
gv_turnover

gv_turnover =gv_turnover %>%
  mutate(SRPturn = SRP_ugL/P_excrete_sum_d,
         TPturn = TP_ugL/P_excrete_sum_d) 
gv_turnover

# Interpretation: Crustacean zooplankton could 
## turnover water column total phosphorus in X days, and soluble reactive phosphrus in z days

plot(SRPturn~doy, data=gv_turnover, type='l', ylim=c(0,365))
plot(TPturn~doy, data =gv_turnover, type = 'l', ylim=c(0,365))

# Try areal see if you get similar results, may have to do mean or 
## mean over certain periods (Inorganic N/P supply shift)
gv_watercolumn
gv_wc_areal = gv_watercolumn %>%
  mutate(SRP_areal = SRP_ugL*8.1, 
         TP_areal = TP_ugL*8.1)
gv_wc_areal

gv_exc_areal = gv_exc %>%
  select(doy, N_areal, P_areal)

gv_arealturn = left_join(gv_wc_areal, gv_exc_areal, by = 'doy')
gv_arealturn = gv_arealturn %>%
  mutate(SRPturn = SRP_areal/P_areal, 
         TPturn = TP_areal/P_areal)
gv_arealturn

plot(SRPturn~doy, data=gv_arealturn, ylim = c(0,365))
plot(TPturn~doy, data=gv_arealturn, ylim = c(0,365))

summ = gv_arealturn %>%
  summarise(SRPareal_avg = mean(SRP_areal, na.rm = T), 
            TPareal_avg = mean(TP_areal, na.rm = T), 
            Pareal_avg = mean(P_areal, na.rm = T)) %>%
  mutate(SRPturn = SRPareal_avg/Pareal_avg, 
         TPturn = TPareal_avg/Pareal_avg)
summ
