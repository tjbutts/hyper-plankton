## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#================================================================##
# STEP 4: QUANTIFY ZOOPLANKTON EXCRETION BASED ON BODY SIZE 
#================================================================##
## NOTE: Be sure to run Step1_Dataset Tidying first ## 

# Excretion equations come from Hebert et al. 2016 # 
 # Hébert, M. P. et al. (2016). A meta-analysis of zooplankton functional traits influencing ecosystem function. Ecology, 97, 1069–1080.

# First load in data extracted from Hebert et al. 2016, Ecology. Data were used for Ordinary Least
# Squares Regression to determine the relationship between zooplankton body size and excretion rate # 

# Determine error coefficients for allometric equations #====================
n_dat_comb = read_csv('Hebert_extract_N.csv') # Freshwater and marine data 
n_dat_comb.fw = read_csv('Hebert_extract_N_FW.csv') # Just Freshwater data 
p_dat_comb = read_csv('Hebert_extract_P.csv') # Freshwater and marine data 
p_dat_comb.fw = read_csv('Hebert_extract_P_FW.csv') # Just Freshwater data 

# Extracted N Data 
plot(n_dat_comb$X,n_dat_comb$Y, ylab = 'ln(N excretion (nmol per ind. per hour))', xlab = 'ln(Dry Mass (mg))')
mod.n = lm(Y~X, data=n_dat_comb)
summary(mod.n)
abline(mod.n)

# Freshwater N Data # 
plot(n_dat_comb.fw$X,n_dat_comb.fw$Y, ylab = 'ln(N excretion (nmol per ind. per hour))', xlab = 'ln(Dry Mass (mg))')
mod.n.fw = lm(Y~X, data=n_dat_comb.fw)
summary(mod.n.fw)
abline(mod.n.fw)
abline(mod.n)

# Extracted P Data # 
plot(p_dat_comb$X,p_dat_comb$Y, ylab = 'ln(p excretion (nmol per ind. per hour))', xlab = 'ln(Dry Mass (mg))')
mod.p = lm(Y~X, data=p_dat_comb)
summary(mod.p)
abline(mod.p)

# Freshwater P Data # 
plot(p_dat_comb.fw$X,p_dat_comb.fw$Y, ylab = 'ln(p excretion (nmol per ind. per hour))', xlab = 'ln(Dry Mass (mg))')
mod.p.fw = lm(Y~X, data=p_dat_comb.fw)
summary(mod.p.fw)
abline(mod.p.fw)
abline(mod.p)

# Freshwater data alone are not significant and the slope is similar to the slope of the freshwater + marine model # 
# Going forward will just use the full freshwater+marine Hebert excretion model # 

# For output of other allometric equations (Wen & Peters 1994) or just the freshwater model, see 
# Step 7_Supplemental Info 

# Estimated zooplankton N and P data #

# Error propagation for y = mx + b # ================
# find uncertainty of b (intercept), find uncertainty of m (slope)# 
# Uncertainty of y = square root(uncertainty of m + uncertainty of b) # 

# Hebert N uncertainty 
N_interr = sqrt(0.17422^2) # error of the intercept 
N_slopeerr = 0.06002*sqrt((0.06002/0.84)^2) # error of the slope 
N_uncert = sqrt(N_slopeerr + N_interr)

# Hebert P Equation uncertainty 
P_interr = sqrt(0.19657^2) # error of the intercept 
P_slopeerr = 0.06304*sqrt((0.06304/0.69380)^2) # error of the slope 
P_uncert = sqrt(P_slopeerr + P_interr)

# get the dry biomass (in micrograms) per taxa per day of year 
gv19_DM = zp_raw %>%
  select(sampleid, doy, taxon, group, drymass)
gv19_DM  

# get the density (number of individuals per liter) per taxa per day of year 
gv19_dens = zp_raw %>%
  select(sampleid, doy, taxon, group, density)
gv19_dens

# Calculate the concentration of nutrients zooplankton would excrete in a day based on their size # 
# Calculate the estimate of zooplankton excretion along with the upper and lower ranges per day of year
Hebert_tot_excretion_rate = gv19_DM %>% # Dry mass in ug
  mutate(dry_biomass_mg = (drymass/1000)) %>% # Allometric equation calls for mass in milligrams
  mutate(ln_P_excretion = 0.56 + (0.70*log(dry_biomass_mg))) %>% 
  mutate(ln_N_excretion = 2.50 + (0.84*log(dry_biomass_mg))) %>%
  mutate(ln_P_excretion_upperr = ln_P_excretion + P_uncert, 
         ln_P_excretion_lowerr = ln_P_excretion - P_uncert) %>%
  mutate(ln_N_excretion_upperr = ln_N_excretion + N_uncert, 
         ln_N_excretion_lowerr = ln_N_excretion - N_uncert)
Hebert_tot_excretion_rate

# Convert value from nmol of N or P per hour to micrograms of N or P per day per L # ==================

### Estimate ### 
Hebert_tot_excretion_rate_e = Hebert_tot_excretion_rate %>%
  select(!c(ln_P_excretion_upperr, ln_P_excretion_lowerr, ln_N_excretion_upperr, ln_N_excretion_lowerr)) %>%
  mutate(N_excrete = exp(ln_N_excretion)) %>%
  mutate(P_excrete = exp(ln_P_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  rename(H4_Nexcrete = N_excrete,
         H4_Pexcrete = P_excrete) %>% 
  select(sampleid, doy, taxon, group, H4_Nexcrete, H4_Pexcrete) %>%
  as_tibble()
Hebert_tot_excretion_rate_e # in nmol per individual per hour 

# Transform nmol per individual per hour to ug/L per day
# First, transform to ug per individual per day 
Hebert_tot_ug_rate = Hebert_tot_excretion_rate_e %>% 
  mutate(N_nmol_indv_d = H4_Nexcrete*24, # change to a daily rate 
         P_nmol_indv_d = H4_Pexcrete*24) %>% 
  mutate(H4_ug_Nexcrete = (N_nmol_indv_d*14.01)/(1000000000*0.000001), #nmol -> mol -> micrograms 
         H4_ug_Pexcrete = (P_nmol_indv_d*30.97)/(1000000000*0.000001)) %>% #nmol -> mol -> micrograms 
  arrange(doy) %>%
  select(sampleid, doy, taxon, group, H4_ug_Nexcrete, H4_ug_Pexcrete)
Hebert_tot_ug_rate # in ug per indvidual per day 

# Join excretion rate data to density data 
Hebert_tot_join = left_join(Hebert_tot_ug_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
Hebert_tot_join
Hebert_tot_join_clean = Hebert_tot_join %>% 
  select(sampleid, doy, taxon, group.x, H4_ug_Nexcrete, H4_ug_Pexcrete, density) %>% #Select relevant columns 
  rename(group = group.x) %>%
  arrange(doy) %>%
  as_tibble()
Hebert_tot_join_clean # excretion and density joined together

# Do the summing step first before multiplying by density 
Hebert_tot_exc_sum_e = Hebert_tot_join_clean %>% 
  group_by(sampleid, doy, group) %>% # include larger taxonomic groupings together to not inflate rotifers 
  summarise(dens_sum = sum(density), 
            Nexc_sum = sum(H4_ug_Nexcrete), 
            Pexc_sum = sum(H4_ug_Pexcrete)) %>%
  ungroup() %>%
  mutate(Nexcrete_d = Nexc_sum*dens_sum, 
         Pexcrete_d = Pexc_sum*dens_sum) %>%
  group_by(sampleid, doy) %>%
  summarise(H_ug_Nexcrete_sum_d = sum(Nexcrete_d),
            H_ug_Pexcrete_sum_d = sum(Pexcrete_d)) %>% # get a daily sum 
  ungroup() %>% 
  arrange(doy) %>%
  as_tibble()
Hebert_tot_exc_sum_e # ug N or P per day per L, mean 

### Upper ### 
Hebert_tot_excretion_rate_u = Hebert_tot_excretion_rate %>%
  select(!c(ln_P_excretion, ln_P_excretion_lowerr, ln_N_excretion, ln_N_excretion_lowerr)) %>%
  mutate(N_excrete = exp(ln_N_excretion_upperr)) %>%
  mutate(P_excrete = exp(ln_P_excretion_upperr)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  rename(H4_Nexcrete = N_excrete,
         H4_Pexcrete = P_excrete) %>% 
  select(sampleid, doy, taxon, group, H4_Nexcrete, H4_Pexcrete) %>%
  as_tibble()
Hebert_tot_excretion_rate_u # in nmol per individual per hour 

# Transform nmol per individual per hour to ug/L per day
# First, transform to ug per individual per day 
Hebert_tot_ug_rate = Hebert_tot_excretion_rate_u %>% 
  mutate(N_nmol_indv_d = H4_Nexcrete*24, # change to a daily rate 
         P_nmol_indv_d = H4_Pexcrete*24) %>% 
  mutate(H4_ug_Nexcrete = (N_nmol_indv_d*14.01)/(1000000000*0.000001), #nmol -> mol -> micrograms 
         H4_ug_Pexcrete = (P_nmol_indv_d*30.97)/(1000000000*0.000001)) %>% #nmol -> mol -> micrograms 
  arrange(doy) %>%
  select(sampleid, doy, taxon, group, H4_ug_Nexcrete, H4_ug_Pexcrete)
Hebert_tot_ug_rate # in ug per indvidual per day 

# Join excretion rate data to density data 
Hebert_tot_join = left_join(Hebert_tot_ug_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
Hebert_tot_join
Hebert_tot_join_clean = Hebert_tot_join %>% 
  select(sampleid, doy, taxon, group.x, H4_ug_Nexcrete, H4_ug_Pexcrete, density) %>% #Select relevant columns 
  rename(group = group.x) %>%
  arrange(doy) %>%
  as_tibble()
Hebert_tot_join_clean # excretion and density joined together

# Do the summing step first before multiplying by density 
Hebert_tot_exc_sum_u = Hebert_tot_join_clean %>% 
  group_by(sampleid, doy, group) %>% # include larger taxonomic groupings together to not inflate rotifers 
  summarise(dens_sum = sum(density), 
            Nexc_sum = sum(H4_ug_Nexcrete), 
            Pexc_sum = sum(H4_ug_Pexcrete)) %>%
  ungroup() %>%
  mutate(Nexcrete_d = Nexc_sum*dens_sum, 
         Pexcrete_d = Pexc_sum*dens_sum) %>%
  group_by(sampleid, doy) %>%
  summarise(H_ug_Nexcrete_sum_d_upper = sum(Nexcrete_d),
            H_ug_Pexcrete_sum_d_upper = sum(Pexcrete_d)) %>% # get a daily sum 
  ungroup() %>% 
  arrange(doy) %>%
  as_tibble()
Hebert_tot_exc_sum_u # ug N or P per day per L, upper

### lower ###
Hebert_tot_excretion_rate_l = Hebert_tot_excretion_rate %>%
  select(!c(ln_P_excretion, ln_P_excretion_upperr, ln_N_excretion, ln_N_excretion_upperr)) %>%
  mutate(N_excrete = exp(ln_N_excretion_lowerr)) %>%
  mutate(P_excrete = exp(ln_P_excretion_lowerr)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  rename(H4_Nexcrete = N_excrete,
         H4_Pexcrete = P_excrete) %>% 
  select(sampleid, doy, taxon, group, H4_Nexcrete, H4_Pexcrete) %>%
  as_tibble()
Hebert_tot_excretion_rate_l # in nmol per individual per hour 

# Transform nmol per individual per hour to ug/L per day
# First, transform to ug per individual per day 
Hebert_tot_ug_rate = Hebert_tot_excretion_rate_l %>% 
  mutate(N_nmol_indv_d = H4_Nexcrete*24, # change to a daily rate 
         P_nmol_indv_d = H4_Pexcrete*24) %>% 
  mutate(H4_ug_Nexcrete = (N_nmol_indv_d*14.01)/(1000000000*0.000001), #nmol -> mol -> micrograms 
         H4_ug_Pexcrete = (P_nmol_indv_d*30.97)/(1000000000*0.000001)) %>% #nmol -> mol -> micrograms 
  arrange(doy) %>%
  select(sampleid, doy, taxon, group, H4_ug_Nexcrete, H4_ug_Pexcrete)
Hebert_tot_ug_rate # in ug per indvidual per day 

# Join excretion rate data to density data 
Hebert_tot_join = left_join(Hebert_tot_ug_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
Hebert_tot_join
Hebert_tot_join_clean = Hebert_tot_join %>% 
  select(sampleid, doy, taxon, group.x, H4_ug_Nexcrete, H4_ug_Pexcrete, density) %>% #Select relevant columns 
  rename(group = group.x) %>%
  arrange(doy) %>%
  as_tibble()
Hebert_tot_join_clean # excretion and density joined together

# Do the summing step first before multiplying by density 
Hebert_tot_exc_sum_l = Hebert_tot_join_clean %>% 
  group_by(sampleid, doy, group) %>% # include larger taxonomic groupings together to not inflate rotifers 
  summarise(dens_sum = sum(density), 
            Nexc_sum = sum(H4_ug_Nexcrete), 
            Pexc_sum = sum(H4_ug_Pexcrete)) %>%
  ungroup() %>%
  mutate(Nexcrete_d = Nexc_sum*dens_sum, 
         Pexcrete_d = Pexc_sum*dens_sum) %>%
  group_by(sampleid, doy) %>%
  summarise(H_ug_Nexcrete_sum_d_lower = sum(Nexcrete_d),
            H_ug_Pexcrete_sum_d_lower = sum(Pexcrete_d)) %>% # get a daily sum 
  ungroup() %>% 
  arrange(doy) %>%
  as_tibble()
Hebert_tot_exc_sum_l # ug N or P per day per L, upper

# Combine estimate with upper and lower boundaries # 
join1 = left_join(Hebert_tot_exc_sum_l, Hebert_tot_exc_sum_u)
join1 

excretion_range = join1 %>%
  rename(Nlow = H_ug_Nexcrete_sum_d_lower,
         Nhigh = H_ug_Nexcrete_sum_d_upper,
         Plow = H_ug_Pexcrete_sum_d_lower,
         Phigh = H_ug_Pexcrete_sum_d_upper)
excretion_range

# Plot values with concentrations of inorganic nutrients #====================== 
# Need gvl19 from Step 3 # 
gvl19
gvl_inorg = gvl19 %>%
  select(doy, SRP_ugL, NOx_mgL)
gvl_inorg

# Combine with excretion, including upper and lower # 
# Isolate the average excretion 
averageexc = Hebert_tot_exc_sum_e %>% 
  select(doy, H_ug_Nexcrete_sum_d, H_ug_Pexcrete_sum_d) %>%
  rename(Nexc = H_ug_Nexcrete_sum_d, 
         Pexc = H_ug_Pexcrete_sum_d)
averageexc  

join = left_join(excretion_range, averageexc, by='doy')
join

join2 = left_join(join, gvl_inorg, by='doy')
join2

excretion_frac = join2 %>%
  mutate(Pf_mid = (Pexc/SRP_ugL)*100,
         Pf_upp = (Phigh/SRP_ugL)*100,
         Pf_low = (Plow/SRP_ugL)*100, 
         Nf_mid = (Nexc/(NOx_mgL*1000))*100, # Convert N milligrams to micrograms 
         Nf_upp = (Nhigh/(NOx_mgL*1000))*100, # Convert N milligrams to micrograms 
         Nf_low = (Nlow/(NOx_mgL*1000))*100) %>% # Convert N milligrams to micrograms 
  select(doy, Pf_mid, Pf_upp, Pf_low,
         Nf_mid, Nf_upp, Nf_low) %>% 
  drop_na() 
excretion_frac

# Plot the data #
windows(height = 5, width = 6)
par(omi=c(0.9,0.9,0.5,0.5), mai=c(0.1,0.2,0.1,0.1))
# Transparent colors # 
col2rgb('orchid2')
t_col = function(color, percent=50, name = NULL) {
  rgb.val = col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max=255,
               alpha = (100 - percent)*255/100, 
               names=name)
  invisible(t.col)
}
pcol = t_col('#c24ad7', percent = 50, name = 'transorch')
ncol = t_col('#004ebe', percent=50, name='transdodge')

# Phosphorus 
lowerbound_p = c(excretion_frac$Pf_low)
upperbound_p = c(excretion_frac$Pf_upp)
doy = c(excretion_frac$doy)
x = c(doy, rev(doy))
y = c(lowerbound_p, rev(upperbound_p))
plot(excretion_frac$doy, upperbound_p, type='l', xlim=c(140,260), col='white',
     cex.axis=1.1, cex=1.5, ylim = c(0,80))
#lines(excretion_frac$doy, lowerbound_p, type='l')
polygon(x=x, y=y, col=pcol, border=F)
lines(excretion_frac$doy, excretion_frac$Pf_mid, type='l', lwd=3, col='orchid3')

# Nitrogen 
lowerbound_n = c(excretion_frac$Nf_low)
upperbound_n = c(excretion_frac$Nf_upp)
doy = c(excretion_frac$doy)
x =c(doy, rev(doy))
y= c(lowerbound_n, rev(upperbound_n))
#lines(excretion_frac$doy, upperbound_n, type='l')
#lines(excretion_frac$doy, lowerbound_n, type='l')
polygon(x=x, y=y, col=ncol, border=F)
lines(excretion_frac$doy, excretion_frac$Nf_mid, type='l', lwd=2, col='dodgerblue3')

# Axis text # 
mtext(side=1, 'Day of Year, 2019', line=2, cex=1 )
mtext(side=2, 'Zooplankton excretion as a percentage 
of the inorganic nutrient pool', line=2.5, cex=1)

# Legend 
legend("topright", legend =c('Nitrogen', 'Phosphorus'), 
       pch=15, 
       pt.cex=1.5, cex=1, bty='n',
       col = c('dodgerblue3', 'orchid3'))
