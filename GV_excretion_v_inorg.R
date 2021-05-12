library(tidyverse)

# GV Chapter Fig. 1 - Zoop excretion + water column concentration #
gv19_arealexc_WP

# Need water column concentration of inorgP - from gv_stocks
gvl_nutr = gvl19 %>%
  select(doy, SRP_ugL, NOx_mgL) %>%
  rename(inorgP_ugL = SRP_ugL, 
         inorgN_mgL = NOx_mgL) %>%
  as_tibble()

gv_turn_join = left_join(gv19_arealexc_WP, gvl_nutr, by = 'doy')  
gv_turn_join

# Calculate turnover - avg water concentration divided by excretion rate 
## Assumes excretion of soluble nutrients by zoops contribute N and P to the water column
## Don't have weekly data on NH4 so can't really do N turnover - hypothesis is about P however so going to 
## focus on that? 

gv_turn = gv_turn_join %>%
  mutate(exc_proport_P = (Pexc_sum/inorgP_ugL)*100) %>%
  mutate(Nexc_sum_mg = Nexc_sum/1000) %>%
  mutate(exc_proport_N = (Nexc_sum_mg/inorgN_mgL)*100) %>%
  select(sampleid, doy, Nexc_sum, Pexc_sum, inorgP_ugL, inorgN_mgL, exc_proport_P, exc_proport_N) %>%
  as_tibble()
gv_turn
