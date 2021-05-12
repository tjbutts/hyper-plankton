# Historical Key Zoop Groups 
# Historical Daphnia, Cyclopoid, Calanoid, Rotifer, Bosmina, Chydorus Distributions # 
# Purpose of this script is to compare how 2019 fits within the history of Green Valley Lake 2011-2018 # 

library(tidyverse)
library(magrittr)
setwd("C:/Users/Owner/Box/Active/Active GV Research/Trait compilation")
hist_plankton_cnp <- read.csv('zp_np_historical.csv')
as_tibble(hist_plankton_cnp)
hist_plankton_cnp %<>% filter(!(genus=='Ostracod'))
hist_zoop = hist_plankton_cnp %>%
  select(sampledate, doy, year, genus, division, biomass) %>% # don't need stoich for this 
  group_by(year, doy, division) %>%
  summarise(biomass = sum(biomass)) %>%
  ungroup() %>%
  as_tibble()
hist_zoop

# Daphnia 
daphnia = hist_zoop %>% filter(division == 'Daphnia')
daphnia1 = daphnia %>%
  filter( doy < 170) %>%
  filter(!(doy == 157 & year == 2019)) %>%
  filter(!(doy == 162 & year == 2019)) %>%
  filter(!(doy == 143 | doy == 150))
daphnia1
hist(daphnia1$biomass)
plot(biomass~year, data=daphnia1)


daphnia2 = daphnia %>%
  filter(doy > 170 & doy < 240) %>%
  filter(!(year == 2019)) 
daphnia2
hist(daphnia2$biomass)
plot(biomass~year, data=daphnia2, xlim = c(2011, 2019))
daphnia %>% filter(year == 2019 & doy == 213)
points(2019, 3.72)

daphnia3 = daphnia %>% 
  filter(doy > 240) %>%
  filter(!(year == 2019))
daphnia3
plot(biomass~year, data=daphnia3, xlim = c(2011, 2019), ylim = c(0,140))
daphnia %>% filter(year == 2019 & doy > 240)
points(2019, 135)

# Cyclopoid 
cyclopoid = hist_zoop %>%
  filter(division == 'Cyclopoid' | division == 'Cyclopoida') %>%
  filter(doy < 170) %>%
  filter(!(doy == 157 & year == 2019)) %>%
  filter(!(doy == 162 & year == 2019)) %>%
  filter(!(doy == 143 | doy == 150))
cyclopoid
hist(cyclopoid$biomass)
plot(biomass~year, data=cyclopoid)

# Bosmina 
bosmina = hist_zoop %>%
  filter(division == 'Bosmina') %>%
  filter(doy < 170) %>%
  filter(!(doy == 157 & year == 2019)) %>%
  filter(!(doy == 162 & year == 2019)) %>%
  filter(!(doy == 143 | doy == 150))
bosmina
hist(bosmina$biomass)
plot(biomass~year, data=bosmina)

# 