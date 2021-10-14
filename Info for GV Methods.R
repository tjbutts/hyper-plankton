# GV Methods Information # 

setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Final Data/Historical Data")

alm=read.csv("Historical_ALM.csv") # Total GV Data 2000 - 2019 
alm = alm %>% filter(Year >2010) %>% arrange(DOY) %>% as_tibble()
alm # split between early and late by < DOY 190 & > DOY 190. Add an identifier column 
alm = alm %>% 
  mutate(season = if_else(DOY < 170, "early", "late")) 
alm = alm %>% select(!c(siteID, name, sampleDate, Treatment, detect, detectLimit))
alm

# replace 0s and below detect lim with 1/2 LRL
unique(alm$analyte)
alm = alm %>% filter(analyte == 'Ammonia-nitrogen (as N)' | analyte == 'Total Nitrogen' | 
                       analyte == 'Nitrate' | analyte == 'Total Phosphorus' | analyte == 'Orthophosphate (as P)' |
                       analyte == 'Fixed suspended solids' | analyte == 'Chlorophyll a')
alm

# Average chl-a # 
chl = alm %>% filter(analyte == 'Chlorophyll a')
chl
mean(chl$result, na.rm=T)
sd = sd(chl$result, na.rm=T)
count = 55
se = sd/sqrt(count)

# Average Thermocline # 
cline = read_csv('thermocline_schmidt_gvl_2019.csv')
cline
library(lubridate)
cline = cline %>% mutate(datetime = mdy_hm(datetime)) %>% mutate(doy = yday(datetime))
cline

avg_depth = cline %>% filter(doy >142) %>% filter(doy<274) %>% drop_na()
mean(avg_depth$thermocline_depth, na.rm=T)
sd = sd(avg_depth$thermocline_depth, na.rm=T)
count = 4345
se = sd/sqrt(count)

# ISS in terms of percent TSS # 
setwd("C:/Users/Owner/Box/Green Valley Project/Final Data/Historical Data")
setwd("C:/Users/Tyler/Box Sync/Green Valley Project/Final Data/Historical Data")
alm=read_csv("Historical_ALM.csv") # Total GV Data 2000 - 2019 
alm
alm_tot = alm %>% filter(analyte == 'Total suspended solids')
alm_tot = alm_tot %>% select(sampleDate, Year, DOY, analyte, result, unit, detectLimit) %>% pivot_wider(names_from = analyte, 
                                                                                                        values_from = result) %>%
  mutate(sampleDate = mdy_hm(sampleDate)) %>% rename(tot = 'Total suspended solids')
alm_tot

alm_fix = alm %>% filter(analyte == 'Fixed suspended solids')
alm_fix = alm_fix %>% select(sampleDate, Year, DOY, analyte, result, unit, detectLimit) %>% pivot_wider(names_from = analyte, 
                                                                                                    values_from = result) %>%
  mutate(sampleDate = mdy_hm(sampleDate)) %>% rename(fix = 'Fixed suspended solids')
alm_fix

alm_solids = left_join(alm_tot, alm_fix, by = 'sampleDate')
alm_solids

alm_solids = alm_solids %>% mutate(perc = (fix/tot)*100) %>% filter(Year.x > 2010)
alm_solids
mean(alm_solids$perc, na.rm = T)

windows(height=6, width=8)
plot(alm_solids$sampleDate, alm_solids$perc, ylab= '(Fixed/Total Suspended Solids)*100', xlab = 'Sampling Date', pch = 19, cex=1.5)
