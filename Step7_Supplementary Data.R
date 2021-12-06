## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#============================================================================================##
# STEP 7: SUPPLEMENTARY DATA - CALCULATIONS, FIGURES, AND TABLES 
#============================================================================================##

# Dataset # 
gv_nutrients
# Historical GV Data #============================ 
# Run Breakpoint analysis to determine early - late season dynamic 

# 
alm_hist # Total GV Data 2000 - 2019 
alm = alm_hist %>% filter(Year > 2010) %>% arrange(DOY) %>% 
  mutate(season = if_else(DOY <170, 'early', 'late')) %>% # Rough split in data from Breakpoints Analysis 
  filter(analyte == 'Ammonia-nitrogen (as N)' | analyte == 'Total Nitrogen' | 
                         analyte == 'Nitrate' | analyte == 'Total Phosphorus' | analyte == 'Orthophosphate (as P)' |
                         analyte == 'Fixed suspended solids' | analyte == 'Chlorophyll a') # Key Analytes

# Supplementary Figure S1 # 
windows(height=6, width=6)
par(mfrow=c(3,3), mai=c(0.6,0.6,0.06,0.1))

#Create Transparent Color Palette (use col2rgb() to transform fave color to RGB values)
ncol <- rgb(30,144,255, max = 255, alpha = 70, names = "blue_transp")
pcol <- rgb(147, 112, 219, max = 255, alpha = 70, names = "purple_transp")
fcol <- rgb(139, 117, 0, max = 255, alpha = 70, names = "tan_transp")
ccol <- rgb(60,179,113, max = 255, alpha = 70, names = "green_transp")

## Nitrogen ##
#Total Nitrogen
alm_tn = alm %>% filter(analyte == 'Total Nitrogen') %>% 
  mutate(result = (result*1000000)/(1000*14.01)) %>%
  select(!(unit))

alm_tn_e = alm_tn %>% filter(season == 'early') %>% as.data.frame()
plot(alm_tn_e[alm_tn_e$analyte=="Total Nitrogen", "DOYFrac"], alm_tn_e[alm_tn_e$analyte=="Total Nitrogen", "result"], pch=19, col=ncol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020), ylim = c(0, max(alm_tn[alm_tn$analyte == 'Total Nitrogen', 'result'])), ylab=expression(Total~N~"("*mu*M*")"), cex.lab=1.5)
alm_tn_l = alm_tn %>% filter(season == 'late') %>% as.data.frame()
points(alm_tn_l[alm_tn_l$analyte=="Total Nitrogen", "DOYFrac"], alm_tn_l[alm_tn_l$analyte=="Total Nitrogen", "result"], pch=19, col=ccol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020))

mean(alm_tn_e[alm_tn_e$analyte=="Total Nitrogen", "result"], na.rm = T)
lines(c(2011,2019), c(159.43, 159.43), lwd=4, col="dodgerblue4")
mean(alm_tn_l[alm_tn_l$analyte=="Total Nitrogen", "result"], na.rm = T)
lines(c(2011,2019), c(145.42, 145.42), lwd=4, col='seagreen4')

#Nitrate
alm_nox = alm %>% filter(analyte == 'Nitrate') %>% 
  mutate(result = (result*1000000)/(1000*14.01)) %>%
  select(!(unit))

alm_nox_e = alm_nox %>% filter(season == 'early') %>% as.data.frame()
plot(alm_nox_e[alm_nox_e$analyte=="Nitrate", "DOYFrac"], alm_nox_e[alm_nox_e$analyte=="Nitrate", "result"], pch=19, col=ncol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020), ylim = c(0, max(alm_tn[alm_tn$analyte == 'Total Nitrogen', 'result'])), ylab=expression(Nitrate~"("*mu*M*")"), cex.lab=1.5)
alm_nox_l = alm_nox %>% filter(season == 'late') %>% as.data.frame()
points(alm_nox_l[alm_nox_l$analyte=="Nitrate", "DOYFrac"], alm_nox_l[alm_nox_l$analyte=="Nitrate", "result"], pch=19, col=ccol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020))
mean(alm_nox_e[alm_nox_e$analyte=="Nitrate", "result"], na.rm = T)
lines(c(2011,2019), c(58.79, 58.79), lwd=4, col="dodgerblue4")
mean(alm_nox_l[alm_nox_l$analyte=="Nitrate", "result"], na.rm = T)
lines(c(2011,2019), c(19.09, 19.09), lwd=4, col='seagreen4')

#Amonnium
alm_nhx = alm %>% filter(analyte == 'Ammonia-nitrogen (as N)') %>% 
  mutate(result = (result*1000000)/(1000*14.01)) %>%
  select(!(unit))

alm_nhx_e = alm_nhx %>% filter(season == 'early') %>% as.data.frame()
plot(alm_nhx_e[alm_nhx_e$analyte=="Ammonia-nitrogen (as N)", "DOYFrac"], alm_nhx_e[alm_nhx_e$analyte=="Ammonia-nitrogen (as N)", "result"], pch=19, col=ncol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020), ylim = c(0, max(alm_tn[alm_tn$analyte == "Total Nitrogen", 'result'])), ylab=expression(Ammonium~"("*mu*M*")"), cex.lab=1.5)
alm_nhx_l = alm_nhx %>% filter(season == 'late') %>% as.data.frame()
points(alm_nhx_l[alm_nhx_l$analyte=="Ammonia-nitrogen (as N)", "DOYFrac"], alm_nhx_l[alm_nhx_l$analyte=="Ammonia-nitrogen (as N)", "result"], pch=19, col=ccol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020))

alm_pre_e = alm_nhx_e %>% filter(Year <2016)
mean(alm_pre_e[alm_pre_e$analyte=="Ammonia-nitrogen (as N)", "result"], na.rm = T)
lines(c(2011,2015), c(5.42, 5.42), lwd=4, col="dodgerblue4")
alm_pre_l = alm_nhx_l %>% filter(Year <2016)
mean(alm_pre_l[alm_pre_l$analyte=="Ammonia-nitrogen (as N)", "result"], na.rm = T)
lines(c(2011,2015), c(11.74, 11.74), lwd=4, col='seagreen4')

alm_post_e = alm_nhx_e %>% filter(Year >2015)
mean(alm_post_e[alm_post_e$analyte=="Ammonia-nitrogen (as N)", "result"], na.rm = T)
lines(c(2016,2019), c(1.56, 1.56), lwd=4, col="dodgerblue4")
alm_post_l = alm_nhx_l %>% filter(Year >2015)
mean(alm_post_l[alm_post_l$analyte=="Ammonia-nitrogen (as N)", "result"], na.rm = T)
lines(c(2016,2019), c(0.632, 0.632), lwd=4, col='seagreen4')

legend('topright', legend=c('DOY < 170', 'DOY > 170'), cex=1.1, pch=19, col=c(ncol, ccol))

## Phosphorus ##
#Total PHosphorus
alm_tp = alm %>% filter(analyte == 'Total Phosphorus') %>% 
  mutate(result = (result*1000000)/(1000*30.97)) %>%
  select(!(unit))

alm_tp_e = alm_tp %>% filter(season == 'early') %>% as.data.frame()
plot(alm_tp_e[alm_tp_e$analyte=="Total Phosphorus", "DOYFrac"], alm_tp_e[alm_tp_e$analyte=="Total Phosphorus", "result"], pch=19, col=ncol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020), ylab=expression(Total~P~"("*mu*M*")"), cex.lab=1.5, ylim = c(0, max(alm_tp[alm_tp$analyte == "Total Phosphorus", 'result'])))
alm_tp_l = alm_tp %>% filter(season == 'late') %>% as.data.frame()
points(alm_tp_l[alm_tp_l$analyte=="Total Phosphorus", "DOYFrac"], alm_tp_l[alm_tp_l$analyte=="Total Phosphorus", "result"], pch=19, col=ccol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020))
mean(alm_tp_e[alm_tp_e$analyte=="Total Phosphorus", "result"], na.rm = T)
lines(c(2011,2019), c(3.68, 3.68), lwd=4, col="dodgerblue4")
mean(alm_tp_l[alm_tp_l$analyte=="Total Phosphorus", "result"], na.rm = T)
lines(c(2011,2019), c(9.88, 9.88), lwd=4, col='seagreen4')

#Soluble Reactive P
alm_srp = alm %>% filter(analyte == 'Orthophosphate (as P)') %>%
  mutate(result = (result*1000000)/(1000*30.97)) %>%
  distinct(DOYFrac, .keep_all = TRUE)
alm_srp

alm_srp_e = alm_srp %>% filter(season == 'early') %>% as.data.frame()
plot(alm_srp_e[alm_srp_e$analyte=="Orthophosphate (as P)", "DOYFrac"], alm_srp_e[alm_srp_e$analyte=="Orthophosphate (as P)", "result"], pch=19, col=ncol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020), ylab=expression(Soluble~P~"("*mu*M*")"), cex.lab=1.5, ylim = c(0, max(alm_tp[alm_tp$analyte == "Total Phosphorus", 'result'])))
alm_srp_l = alm_srp %>% filter(season == 'late') %>% as.data.frame()
points(alm_srp_l[alm_srp_l$analyte=="Orthophosphate (as P)", "DOYFrac"], alm_srp_l[alm_srp_l$analyte=="Orthophosphate (as P)", "result"], pch=19, col=ccol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020))
mean(alm_srp_e[alm_srp_e$analyte=="Orthophosphate (as P)", "result"], na.rm = T)
lines(c(2011,2019), c(0.747, 0.747), lwd=4, col="dodgerblue4")
mean(alm_srp_l[alm_srp_l$analyte=="Orthophosphate (as P)", "result"], na.rm = T)
lines(c(2011,2019), c(4.426, 4.426), lwd=4, col='seagreen4')

#Fixed Suspended Solids 
alm_e = alm %>% filter(season == 'early') %>% as.data.frame()
plot(alm_e[alm_e$analyte=="Fixed suspended solids", "DOYFrac"], alm_e[alm_e$analyte=="Fixed suspended solids", "result"], pch=19, col=ncol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020), ylab=expression(Inorg~Part~"("*mg~L^-1*")"), cex.lab=1.5, ylim = c(0, max(alm[alm$analyte == "Fixed suspended solids", 'result'])))
alm_l = alm %>% filter(season == 'late') %>% as.data.frame()
points(alm_l[alm_l$analyte=="Fixed suspended solids", "DOYFrac"], alm_l[alm_l$analyte=="Fixed suspended solids", "result"], pch=19, col=ccol, cex=1.5, xlab="", cex.axis=1.1, xlim=c(2010,2020))
mean(alm_e[alm_e$analyte=="Fixed suspended solids", "result"], na.rm = T)
lines(c(2011,2019), c(4.97, 4.97), lwd=4, col="dodgerblue4")
mean(alm_l[alm_l$analyte=="Fixed suspended solids", "result"], na.rm = T)
lines(c(2011,2019), c(8.35, 8.35), lwd=4, col='seagreen4')

# Plankton Data 
zoop_hist 

zoop_hist_clean = zoop_hist %>% 
  group_by(yearfrac, doy) %>% 
  summarise(biomass = sum(biomass)) %>%
  ungroup() %>%
  mutate(season = if_else(doy <170, 'early', 'late')) %>%
  filter(!(doy == 157)) %>% # remove lost sample on DOY 157
  arrange(yearfrac)
zoop_hist_clean 

zoop_clean_e = zoop_hist_clean %>% filter(season == 'early')
zoop_clean_l = zoop_hist_clean %>% filter(season == 'late')

# Zooplankton 
plot(zoop_clean_e$yearfrac, log10(zoop_clean_e$biomass), col=ncol, pch=19, cex=1.5, xlab="", yaxt ='n', cex.axis=1.1, xlim=c(2010,2020),ylab=expression(Zooplankton~"("*mu*g~L^-1*")"), cex.lab=1.4, ylim=c(log10(1), log10(500)))
points(zoop_clean_l$yearfrac, log10(zoop_clean_l$biomass), col=ccol, pch=19, cex=1.5)
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),
                  log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90),
                  log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000), log10(2000)), 
     labels=c("1","","","","","","","","","10","","","","","","","","","100","","","","","","","","","1000",'2000'), las=3, cex.lab=1.1)

mean(zoop_clean_e$biomass, na.rm = T)
lines(c(2011,2019), c(log10(223.74), log10(223.74)), lwd=4, col="dodgerblue4")
mean(zoop_clean_l$biomass, na.rm = T)
lines(c(2011,2019), c(log10(82.74), log10(82.73)), lwd=4, col='seagreen4')

# Phytoplankton

phyto_hist 
phyto_clean = phyto_hist %>%
  filter(Year > 2010) %>% # Separate into post 2010 after a large lake renovation where two large silk dikes were put in
  select(Year, DOYFrac, PhytoBiomass, Cyanophyta) %>%
  mutate(non_Cyanophyta = PhytoBiomass - Cyanophyta) %>%
  mutate(doy = lubridate::date_decimal(DOYFrac)) %>% # Convert DOYFrac to a date value 
  mutate(doy = yday(doy)) %>% 
  select(Year, doy, DOYFrac, PhytoBiomass, Cyanophyta, non_Cyanophyta)
phyto_clean # Need to add a doy column to split early and late 

phyto_e = phyto_clean %>% mutate(season = if_else(doy <170, 'early', 'late')) %>%
  filter(season == 'early')
phyto_l = phyto_clean %>% mutate(season = if_else(doy <170, 'early', 'late')) %>% 
  filter(season == 'late')

# non_Cyanophyta 
plot(phyto_e$DOYFrac, log10(phyto_e$non_Cyanophyta), col=ncol, pch=19, cex=1.5, yaxt="n", xlab="Year", cex.axis=1.1, xlim=c(2010,2020),ylab=expression(Non~Cyanos~"("*mg~L^-1*")"), cex.lab=1.4, ylim=c(log10(1), log10(2000)))
points(phyto_l$DOYFrac, log10(phyto_l$non_Cyanophyta), col = ccol, pch=19, cex=1.5)
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90), log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000), log10(2000)), 
     labels=c("1","","","","","","","","","10","","","","","","","","","100","","","","","","","","","1000", '2000'), las=3, cex.lab=1.3)
mean(phyto_e$non_Cyanophyta, na.rm = T)
lines(c(2011,2019), c(log10(15.41), log10(15.41)), lwd=4, col="dodgerblue4")
mean(phyto_l$non_Cyanophyta, na.rm = T)
lines(c(2011,2019), c(log10(5.11), log10(5.11)), lwd=4, col='seagreen4')

# Cyanophyta
plot(phyto_e$DOYFrac, log10(phyto_e$Cyanophyta), col=ncol, pch=19, cex=1.5, yaxt="n", xlab="", cex.axis=1.1, xlim=c(2010,2020),ylab=expression(Cyanos~"("*mg~L^-1*")"), cex.lab=1.4, ylim=c(log10(1), log10(2000)))
points(phyto_l$DOYFrac, log10(phyto_l$Cyanophyta), col = ccol, pch=19, cex=1.5)
axis(side=2, at=c(log10(1), log10(2), log10(3), log10(4), log10(5), log10(6), log10(7), log10(8), log10(9),log10(10), log10(20), log10(30), log10(40), log10(50), log10(60), log10(70), log10(80), log10(90), log10(100), log10(200), log10(300), log10(400), log10(500), log10(600), log10(700), log10(800), log10(900),
                  log10(1000), log10(2000)), 
     labels=c("1","","","","","","","","","10","","","","","","","","","100","","","","","","","","","1000", '2000'), las=3, cex.lab=1.3)
mean(phyto_e$Cyanophyta, na.rm = T)
lines(c(2011,2019), c(log10(275.24), log10(275.24)), lwd=4, col="dodgerblue4") # Mean of early 
mean(phyto_l$Cyanophyta, na.rm = T)
lines(c(2011,2019), c(log10(195.12), log10(195.12)), lwd=4, col='seagreen4') # Mean of late 

# Phytoplankton GALD v. Zooplankton Body Mass #=======================================
# Supplementary Figure S2 # 
gv_gald_bodymass
ridge2 = gv_gald_bodymass %>% 
  mutate(gald = gald*2.5) %>% # convert from ocular units to micrometers 
  pivot_longer(cols = c(gald, mass), names_to = 'measure', values_to = 'value')
ridge2

ridge_fct2 = ridge2 %>%
  mutate(doyfct = fct_rev(as.factor(doy)))
gald_ridge = ridge_fct2 %>% filter(measure == 'gald')
mass_ridge = ridge_fct2 %>% filter(measure == 'mass')

# Can't combine - um v. ug so plot side by side 
windows(height=6, width=6)
p1 <- ggplot(
  gald_ridge, 
  aes(y = doyfct)) +
  geom_density_ridges(
    aes(x = value, fill = paste(doyfct, measure)), scale=1,
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  xlab(label = bquote(GALD~(mu*g))) + 
  ylab(label = 'Day of Year, 2019') +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = 'b') +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size = 12, colour = 'black')) + 
  scale_fill_cyclical(
    breaks = c("gald"),
    labels = c(`gald` = "phytoplankton GALD"),
    values = c('#008a64',  #143
               '#009d73',
               '#00b181',
               '#00c48f',
               '#00d89d', #172
               '#00ecac',
               '#00ffba',
               '#14ffbf', #199
               '#27ffc5',
               '#3bffca',
               '#4fffcf', #213
               '#62ffd4', 
               '#76ffda', 
               '#89ffdf', 
               '#9dffe4', 
               '#ffd4fe'),
    name = "measure", guide = "legend") 

p2 <- ggplot(
  mass_ridge, 
  aes(y = doyfct)) +
  geom_density_ridges(
    aes(x = value, fill = paste(doyfct, measure)), scale=1,
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  xlab(label = bquote(zooplankton~body~mass~(mu*g))) + 
  ylab(label = 'Day of Year, 2019') +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = 'b') +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size = 12, colour = 'black')) + 
  scale_fill_cyclical(
    breaks = c("mass"),
    labels = c( `mass` = "Zooplankton bodymass"),
    values = c( '#630060', #143
                '#780075',
                '#8e008a',
                '#a4009f', #172
                '#b900b4',
                '#cf00ca',
                '#e500df', #199
                '#fa00f4',
                '#ff11f9',
                '#ff67fb', #213
                '#ff7dfc', 
                '#ff93fc', 
                '#ffa8fd', #234
                '#ffa8fd', 
                '#ffbefd', 
                '#ffd4fe'),
    name = "measure", guide = "legend") 
ggarrange(p1,p2,ncol=2)

# Phytoplankton GALD v. Zooplankton Length/Body Mass Regressions #========================
# Supplementary Figure S3 # 
# phytoplankton mean GALD 
gald_mean = gald_ridge %>%
  group_by(doy) %>%
  summarise(gald_avg = mean(value, na.rm = T)) %>%
  ungroup()
gald_mean

# mean zooplankton body mass 
mass_mean = mass_ridge %>%
  group_by(doy) %>%
  summarise(mass_avg = mean(value, na.rm = T)) %>%
  ungroup()
mass_mean

# mean zooplankton length 
length_mean = length %>%
  group_by(doy) %>%
  summarise(length_avg = mean(value, na.rm = T)) %>%
  ungroup()
length_mean

# Relationship between phytoplankton GALD v. zooplankton length 
meanmod1 = left_join(length_mean, gald_mean, by = 'doy')
meanmod1$length_avg[is.nan(meanmod1$length_avg)] <-NA
meanmod1$gald_avg[is.nan(meanmod1$gald_avg)] <-NA

m1 = meanmod1 %>% rename(l = length_avg) %>% rename(g = gald_avg)
mod1 = lm(g~l, data = m1)
print(mod1)
summary(mod1)

graphics.off()
windows(height=4, width=6)
par(mfrow=c(1,2), mai=c(0.9,0.9,0.5,0.2))
with(m1, plot(l,g,  col=ncol, cex = 1.5, pch=19, cex.lab=1.1, ylab = 'Phytoplankton GALD', xlab = 'Zooplankton Length'))
abline(mod1)

meanmod2 = left_join(mass_mean, gald_mean, by = 'doy')
meanmod2 
meanmod2$mass_avg[is.nan(meanmod2$mass_avg)] <-NA
meanmod2$gald_avg[is.nan(meanmod2$gald_avg)] <-NA

m2 = meanmod2 %>% rename(m = mass_avg) %>% rename(g = gald_avg)
mod2 = lm(g~m, data = m2)
print(mod2)
summary(mod2)
par(mai=c(0.9,0.3,0.5,0.9))
with(m2, plot(m,g,  col=ncol, cex = 1.5, pch=19, cex.lab=1.1, ylab = '', xlab = 'Zooplankton Body Mass'))
abline(mod2)

# Zooplankton Excretion #=====================================
Hebert_tot_exc_sum_e # ug N or P per day per L, estimate from Step4

# Convert to uM # 
Hebert_tot_exc_uM = Hebert_tot_exc_sum_e %>%
  rename(Nexc = H_ug_Nexcrete_sum_d, 
         Pexc = H_ug_Pexcrete_sum_d) %>% 
  mutate(Pexc_uM = (Pexc*1000000)/(1000000*30.97), # ug N or P per day per L to uM N or P per day  
         Nexc_uM = (Nexc*1000000)/(1000000*14.01))
Hebert_tot_exc_uM  

# Wen and Peters excretion equations 
zp_raw

# get the dry biomass (in micrograms) per taxa per day of year 
gv19_DM = zp_raw %>%
  select(sampleid, doy, taxon, group, drymass)
gv19_DM  

# get the density (number of individuals per liter) per taxa per day of year 
gv19_dens = zp_raw %>%
  select(sampleid, doy, taxon, group, density)
gv19_dens

# Need to add on a temperature column to incorporate Temp variable 
doy = c(211, 143, 150, 164, 171, 178, 192, 199,206,213, 220, 227, 234, 245,251,273)
temp = c(26,18,24,24,22,22,26,27,26,26,26,25,26,23,23,20)
gv19_temp = data.frame(doy, temp)
gv19_temp = gv19_temp %>%
  mutate(temp_K = temp+273.15, # Wen and Peters equations require temp in Kelvin
         temp_c = temp) 

gv19_DM_join = gv19_DM %>%
  left_join(gv19_temp, gv19_DM, by = 'doy') %>%
  as_tibble()
gv19_DM_join

WP_multi_excretion_rate = gv19_DM_join %>% # Dry mass already in ug 
  mutate(log_P_excretion = -5.28+(0.61*log10(drymass))+(0.01*temp_K)+(0.02*24)) %>% 
  mutate(log_N_excretion = -3.47+(0.74*log10(drymass))+(0.00002*temp_K^2)-(0.02*24)) %>%  
  mutate(N_excrete = (10^log_N_excretion)) %>%
  mutate(P_excrete = (10^log_P_excretion)) %>%
  mutate(N_excrete = replace_na(N_excrete, 0),
         P_excrete = replace_na(P_excrete, 0)) %>% # Replace NAs with 0s
  rename(WP2_Nexcrete = N_excrete,
         WP2_Pexcrete = P_excrete) %>%
  select(sampleid, doy, taxon, group, WP2_Nexcrete, WP2_Pexcrete) %>%
  as_tibble()
WP_multi_excretion_rate # in ug per day 

# Transform to ug/L per day 
# Join excretion rate data to density data 
WP_multi_join = left_join(WP_multi_excretion_rate, gv19_dens, by = c('doy', 'sampleid', 'taxon'))
WP_multi_join
WP_multi_join_clean = WP_multi_join %>% 
  select(sampleid, doy, taxon, group.x, WP2_Nexcrete, WP2_Pexcrete, density) %>% #Select relevant columns 
  rename(group = group.x) %>%
  arrange(doy) %>%
  as_tibble()
WP_multi_join_clean # excretion and density joined together

# Do the summing step first before multiplying by density 
WP_multi_exc_sum = WP_multi_join_clean %>% 
  group_by(sampleid, doy, group) %>% # include larger taxonomic groupings together to not inflate rotifers 
  summarise(dens_sum = sum(density), 
            Nexc_sum = sum(WP2_Nexcrete), 
            Pexc_sum = sum(WP2_Pexcrete)) %>%
  ungroup() %>%
  mutate(Nexcrete_d = Nexc_sum*dens_sum, 
         Pexcrete_d = Pexc_sum*dens_sum) %>%
  group_by(sampleid, doy) %>%
  summarise(WP2_Nexcrete_sum_d = sum(Nexcrete_d),
            WP2_Pexcrete_sum_d = sum(Pexcrete_d)) %>% # get a daily sum 
  ungroup() %>% 
  arrange(doy) %>%
  as_tibble()
WP_multi_exc_sum # ug N or P per day per L

# Convert to uM 
WP_multi_uM = WP_multi_exc_sum %>% 
  rename(Nexc = WP2_Nexcrete_sum_d, 
         Pexc = WP2_Pexcrete_sum_d) %>%
  mutate(Nexc_uM = (Nexc*1000000)/(1000000*14.01), # ug N or P per day per L to uM N or P per day  
         Pexc_uM = (Pexc*1000000)/(1000000*30.97))
WP_multi_uM
  