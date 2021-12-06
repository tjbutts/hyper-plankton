## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts October 2021

#===============================================================================================##
# STEP 2: VISUALIZE ZOOPLANKTON-PHYTOPLANKTON DYNAMICS + ZOOPLANKTON:PHYTOPLANKTON BIOMASS RATIO
#===============================================================================================##
## NOTE: Be sure to run Step1_Dataset Tidying first ## 

# Datasets required 
zp_raw 
phy_biomass
phy_grouping 

# Select Color Scheme # 
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)

# Zooplankton Colors
display.brewer.pal(9, 'BuPu')
box()
brewer.pal(9, 'BuPu') # Light to Dark = Left to Right

# Phytoplankton Colors
display.brewer.pal(9, 'BuGn')
box()
brewer.pal(9, 'BuGn') # Light to Dark = Left to Right

# Set dimensions for figure #
windows(height=6,width=6)
par(mfrow=c(2,1), omi=c(0.1,0.9,0.1,0.1), mai=c(0.55,0.2,0.1,0.9))

# Zooplankton Biomass Time Series #==========================
# Sum Zooplankton group totals to construct polygons 
group_sums = zp_raw %>% select(doy, group, taxon, biomass) %>% # Sum together zooplankton by group 
  group_by(doy, group) %>% 
  summarise(biomass = sum(biomass)) %>%
  ungroup()

# First Plot 

plot(0,0, pch=NA, xlab="", ylab="", xlim=c(143,280), ylim=c(0,275),xaxt="n", cex.axis=1.1)
axis(1, at=c(140,150,160,170, 180, 
             190, 200, 210, 220, 230, 
             240, 250, 260, 270, 280),labels=c("140","","160","","180","",'200', '',"220","","240","","260","", '280' ), cex.axis=1.1)
mtext(side=2, line=2, cex = 1, expression(atop(Zooplankon, Biomass~"("*mu*g~L^-1*")")), font = 2)

doy = c(143,150,164,171,178,192,199,206,211,213,220,227,234,245,251,273)
doy = data.frame(doy)
doy

#Nauplii polygon
sumzoop = c(0, group_sums[group_sums$group =='Daphnia', 'biomass'] + #4F007F
              group_sums[group_sums$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
              group_sums[group_sums$group == 'Simocephalus', 'biomass'] + 
              group_sums[group_sums$group == 'Ostracod', 'biomass'] + #f075c0
              group_sums[group_sums$group =='Bosmina','biomass'] + #94090D
              group_sums[group_sums$group =='Chydorus', 'biomass'] + #FF1D23
              group_sums[group_sums$group =='Rotifer', 'biomass'] + #f1b6da
              group_sums[group_sums$group =='Calanoid', 'biomass']+ #8FCBFF
              group_sums[group_sums$group =='Cyclopoid', 'biomass']+ #0389FF
              group_sums[group_sums$group =='Nauplii', 'biomass'], 0) #003F76
totbiomass = sumzoop$biomass
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col='#3C3B3D', border=F)


#Cyclopoid polygon
group_sums2 = group_sums %>% filter(!(group == 'Nauplii'))
group_sums2

sumzoop2 = c(0, group_sums2[group_sums2$group =='Daphnia', 'biomass'] + #4F007F
               group_sums2[group_sums2$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
               group_sums2[group_sums2$group == 'Simocephalus', 'biomass'] + 
               group_sums[group_sums$group == 'Ostracod', 'biomass'] + #f075c0
               group_sums2[group_sums2$group =='Bosmina','biomass'] + #94090D
               group_sums2[group_sums2$group =='Chydorus', 'biomass'] + #FF1D23
               group_sums2[group_sums2$group =='Rotifer', 'biomass'] + #f1b6da
               group_sums2[group_sums2$group =='Calanoid', 'biomass']+ #8FCBFF
               group_sums2[group_sums2$group =='Cyclopoid', 'biomass'], 0) #0389FF

totbiomass2 = sumzoop2$biomass
totb2 = data.frame(doy, totbiomass2)
totb2

polygon(
  c(min(totb2$doy), totb2$doy , max(totb2$doy)) , 
  c(0 , totb2$totbiomass , 0),  col='#6D6C70', border=F)


#Calanoid polygon
group_sums3 = group_sums %>% filter(!c(group == 'Nauplii' | group == 'Cyclopoid'))

sumzoop3 = c(0, group_sums3[group_sums3$group =='Daphnia', 'biomass'] + #4F007F
               group_sums3[group_sums3$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
               group_sums3[group_sums3$group == 'Simocephalus', 'biomass'] + 
               group_sums[group_sums$group == 'Ostracod', 'biomass'] + #f075c0
               group_sums3[group_sums3$group =='Bosmina','biomass'] + #94090D
               group_sums3[group_sums3$group =='Chydorus', 'biomass'] + #FF1D23
               group_sums3[group_sums3$group =='Rotifer', 'biomass'] + #f1b6da
               group_sums3[group_sums3$group =='Calanoid', 'biomass'], 0) #8FCBFF

totbiomass3 = sumzoop3$biomass
totb3 = data.frame(doy, totbiomass3)
totb3

polygon(
  c(min(totb3$doy), totb3$doy , max(totb3$doy)) , 
  c(0 , totb3$totbiomass , 0),  col='#A2A1A6', border=F)


#Rotifera polygon
group_sums4 = group_sums %>% filter(!c(group == 'Nauplii' | 
                                         group == 'Cyclopoid' | 
                                         group == 'Calanoid'))

sumzoop4 = c(0, group_sums4[group_sums4$group =='Daphnia', 'biomass'] + #4F007F
               group_sums4[group_sums4$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
               group_sums4[group_sums4$group == 'Simocephalus', 'biomass'] + 
               group_sums[group_sums$group == 'Ostracod', 'biomass'] + #f075c0
               group_sums4[group_sums4$group =='Bosmina','biomass'] + #94090D
               group_sums4[group_sums4$group =='Chydorus', 'biomass'] + #FF1D23
               group_sums4[group_sums4$group =='Rotifer', 'biomass'], 0) #f1b6da

totbiomass4 = sumzoop4$biomass
totb4 = data.frame(doy, totbiomass4)
totb4

polygon(
  c(min(totb4$doy), totb4$doy , max(totb4$doy)) , 
  c(0, totb4$totbiomass , 0),  col='#f1b6da', border=F)

#Ostracod polygon (visually indistinguishable, but included here)
group_sums_ostracod = group_sums %>% filter(!c(group == 'Nauplii' | 
                                                 group == 'Cyclopoid' | 
                                                 group == 'Calanoid' | 
                                                 group == 'Rotifer'))

sumzoop_ost = c(0, group_sums4[group_sums4$group =='Daphnia', 'biomass'] + #4F007F
               group_sums4[group_sums4$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
               group_sums4[group_sums4$group == 'Simocephalus', 'biomass'] + 
               group_sums4[group_sums4$group =='Bosmina','biomass'] + #94090D
               group_sums4[group_sums4$group =='Chydorus', 'biomass'] + #FF1D23
               group_sums[group_sums$group == 'Ostracod', 'biomass'], 0) #f075c0

totbiomassost = sumzoop_ost$biomass
totbost = data.frame(doy, totbiomassost)
totbost

polygon(
  c(min(totbost$doy), totbost$doy , max(totbost$doy)) , 
  c(0, totbost$totbiomassost , 0),  col='#f075c0', border=F)

# Chydorus polygon
group_sums5 = group_sums %>% filter(!c(group == 'Nauplii' | 
                                         group == 'Cyclopoid' | 
                                         group == 'Calanoid' | 
                                         group == 'Rotifer' |
                                         group == 'Ostracod'))

sumzoop5 = c(0, group_sums5[group_sums5$group =='Daphnia', 'biomass'] + #4F007F
               group_sums5[group_sums5$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
               group_sums5[group_sums5$group == 'Simocephalus', 'biomass'] + 
               group_sums5[group_sums5$group =='Bosmina','biomass'] + #94090D
               group_sums5[group_sums5$group =='Chydorus', 'biomass'], 0) #FF1D23


totbiomass5 = sumzoop5$biomass
totb5 = data.frame(doy, totbiomass5)
totb5

polygon(
  c(min(totb5$doy), totb5$doy , max(totb5$doy)) , 
  c(0 , totb5$totbiomass , 0),  col='#40004b', border=F)

# Bosmina polygon
group_sums6 = group_sums %>% filter(!c(group == 'Nauplii' | 
                                         group == 'Cyclopoid' | 
                                         group == 'Calanoid' | 
                                         group == 'Rotifer' | 
                                         group == 'Ostracod' |
                                         group == 'Chydorus'))

sumzoop6 = c(0, group_sums6[group_sums6$group =='Daphnia', 'biomass'] + #4F007F
               group_sums6[group_sums6$group == 'Ceriodaphnia', 'biomass'] + #9F00FF
               group_sums6[group_sums6$group == 'Simocephalus', 'biomass'] + 
               group_sums6[group_sums6$group =='Bosmina','biomass'], 0) #94090D

totbiomass6 = sumzoop6$biomass
totb6 = data.frame(doy, totbiomass6)
totb6

polygon(
  c(min(totb6$doy), totb6$doy , max(totb6$doy)) , 
  c(0 , totb6$totbiomass , 0),  col='#762a83', border=F)

#Ceriodaphnia polygon
group_sums8 = group_sums %>% filter(!c(group == 'Nauplii' | 
                                         group == 'Cyclopoid' | 
                                         group == 'Calanoid' | 
                                         group == 'Ostracod' | 
                                         group == 'Rotifer' | 
                                         group == 'Chydorus' | 
                                         group == 'Bosmina'))

sumzoop8 = c(0, group_sums6[group_sums6$group =='Daphnia', 'biomass'] + #4F007F
               group_sums6[group_sums6$group == 'Simocephalus', 'biomass'] +
               group_sums6[group_sums6$group == 'Ceriodaphnia', 'biomass'], 0) #9F00FF


totbiomass8 = sumzoop8$biomass
totb8 = data.frame(doy, totbiomass8)
totb8

polygon(
  c(min(totb8$doy), totb8$doy , max(totb8$doy)) , 
  c(0, totb7$totbiomass , 0),  col='#9970ab', border=F)

#Daphnia + Simocephalus polygon (Simocephalus grouped with Daphnia) 
group_sums9 = group_sums %>% filter(!c(group == 'Nauplii' | 
                                         group == 'Cyclopoid' | 
                                         group == 'Calanoid' | 
                                         group == 'Ostracod' | 
                                         group == 'Rotifer' | 
                                         group == 'Chydorus' | 
                                         group == 'Bosmina' | 
                                         group == 'Ceriodaphnia'))

sumzoop9 = c(0, group_sums6[group_sums6$group =='Daphnia', 'biomass'] + 
               group_sums6[group_sums6$group == 'Simocephalus', 'biomass'], 0) #4F007F


totbiomass9 = sumzoop9$biomass
totb9 = data.frame(doy, totbiomass9)
totb9

polygon(
  c(min(totb9$doy), totb9$doy , max(totb9$doy)) , 
  c(0, totb9$totbiomass, 0),  col='#c2a5cf', border=F)

# Need to reverse taxa order to follow the order of the polygons above 
taxa = rev(c(expression(italic('Daphnia')), expression(italic('Ceriodaphnia')),
             expression(italic('Bosmina')), expression(italic('Chydorus')), 'Rotifera','Ostracoda','Calanoid', 'Cyclopoid', 'Nauplii'))
# Need to reverse colors to match reverse above 
col= rev(c("#c2a5cf", "#9970ab", "#762a83" ,"#40004b", "#f1b6da", '#f075c0', "#A2A1A6", '#6D6C70', '#3C3B3D'))

# Phytoplankton Biomass Time Series #==========================
# Transform long form data to wide format, make column headers consistent, and prepare for join with phytoplankton group information
phy_wide = phy_biomass %>% 
  rename(doy = Day) %>% # Rename column headers to be more consistent 
  select(!(DIVISION)) %>%
  filter(Treatment == 'Pre') %>%
  select(!(Treatment)) %>%
  pivot_wider(names_from = TAXON, 
              values_from = BIOMASS.MG.L) %>%
  arrange(doy)
phy_wide[is.na(phy_wide)] = 0
phy_wide = phy_wide %>% select(!c(SAMPLE.ID, LAKE.NO))
phy_wide # Check to make sure everything is in an interpretable format

# Gather data and add on larger taxonomic grouping 
phy_wide = modify_if(phy_wide,is.character, as.factor) # Turn character columns to factors 
phy_wide
phy_long = phy_wide %>% gather(key='species', value='biovolume', -c(doy)) %>% # Need long format to add grouping identifier 
  rename(taxon = species)

# Add on taxonomic grouping with a left join
phy_groups = left_join(phy_long, phy_grouping, by = 'taxon')

# Readjust phytoplankton group names 
unique(phy_groups$group) # Combine Chrysophyta, Cryptophyta, and Euglenophyta into other; Cyanophyta into other Cyanos 
phy_groups$group <- gsub("Cryptophyta", "Other", phy_groups$group)
phy_groups$group <- gsub("Chrysophyta", "Other", phy_groups$group)
phy_groups$group <- gsub("Euglenophyta", "Other", phy_groups$group)
phy_groups$group <- gsub("Cyanophyta", "Misc_Cyanos", phy_groups$group)
phy_groups$group <- gsub('Cyanobacteria', "Misc_Cyanos", phy_groups$group)
phy_groups

# Get the group sums of phytoplankton as we did with zooplankton above 
phy_polygons <- phy_groups %>% group_by(doy, group) %>%
  summarise(totbiom = sum(biovolume)) %>%
  mutate(percentage = totbiom/ sum(totbiom)) %>% 
  ungroup() %>%
  as_tibble()
phy_polygons

pdat = phy_polygons %>% mutate(division = group) %>% select(!c(group, percentage))
pdat # Easier name for writing polygon code 

# Last round of readjusting phytoplankton group names 
unique(pdat$division) # Combine Aphanizomenon, Dolichospermum, Misc_Cyanos into Other Cyanos 
pdat$division <- gsub('Aphanizomenon', 'Misc_Cyanos', pdat$division)
pdat$division <- gsub('Dolichospermum', 'Misc_Cyanos', pdat$division)
pdat$division <- gsub('Other', 'Non-Cyanophytes', pdat$division)

# Get the group sums of the newly adjusted phytoplankton group names 
pdat = pdat %>%
  group_by(doy, division) %>%
  summarise(totbiom = sum(totbiom)) %>%
  ungroup() %>% 
  as_tibble()
pdat

# Order the phytoplankton groups in such a way all cyanophytes are last 
pdat$division <- factor(pdat$division, levels = c('Bacillariophyta', 'Chlorophyta', 'Non-Cyanophytes', 'Aphanothece', 'Microcystis', 'Misc_Cyanos'))
pdat

# Plot Phytoplankton polygon # 

plot(0,0, pch=NA, xlab="", ylab="", xlim=c(143,280), ylim=c(0,350),xaxt="n", cex.axis=1.1)
axis(1, at=c(140,150,160,170, 180, 
             190, 200, 210, 220, 230, 
             240, 250, 260, 270, 280),labels=c("140","","160","","180","",'200', '',"220","","240","","260","", '280' ), cex.axis=1.1)
mtext(side=2, line=2, cex=1,expression(atop(Phytoplankton,Biomass~"("*mg~L^-1*")")))
mtext(side=1, line=2, cex=1, 'Day of Year, 2019')

doy = c(143,150,157,164,172,178,192,199,206,211,213,220,227,245,251)
doy = data.frame(doy)
doy

#Misc_Cyanos+ Microcystis+Aphanothece+
## Non-Cyanophytes+ Chlorophyta+ Bacillariophyta
## Color Selection ## 
# Bacillariophyta - #075807 - #075807
# Chlorophyta - #097609 - #097609
# Non-Cyanophyte - #70af1a - #70AF1A
# Aphanothece - #74ceb7 - #74CEB7
# Microcystis - #2c858d - #2C858D
# Misc_Cyanos - #004056 - #004056

sumphy = c(pdat[pdat$division == 'Bacillariophyta', 'totbiom'] +
             pdat[pdat$division == 'Chlorophyta', 'totbiom'] + 
             pdat[pdat$division == 'Non-Cyanophytes', 'totbiom'] + 
             pdat[pdat$division == 'Aphanothece', 'totbiom'] + 
             pdat[pdat$division == 'Microcystis', 'totbiom'] +
             pdat[pdat$division == 'Misc_Cyanos', 'totbiom'])
sumphy

totbiomass = sumphy$totbiom
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col="#00441b", border=F)

#Microcystis+Aphanothece+
## Non-Cyanophytes+ Chlorophyta+ Bacillariophyta

sumphy = c(pdat[pdat$division == 'Bacillariophyta', 'totbiom'] +
             pdat[pdat$division == 'Chlorophyta', 'totbiom'] + 
             pdat[pdat$division == 'Non-Cyanophytes', 'totbiom'] + 
             pdat[pdat$division == 'Aphanothece', 'totbiom'] + 
             pdat[pdat$division == 'Microcystis', 'totbiom'])
sumphy

totbiomass = sumphy$totbiom
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col="#5aae61", border=F)


#Aphanothece+
## Non-Cyanophytes+ Chlorophyta+ Bacillariophyta

sumphy = c(pdat[pdat$division == 'Bacillariophyta', 'totbiom'] +
             pdat[pdat$division == 'Chlorophyta', 'totbiom'] + 
             pdat[pdat$division == 'Non-Cyanophytes', 'totbiom'] + 
             pdat[pdat$division == 'Aphanothece', 'totbiom'])
sumphy

totbiomass = sumphy$totbiom
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col="#d9f0d3", border=F)

## Non-Cyanophytes+ Chlorophyta+ Bacillariophyta

sumphy = c(pdat[pdat$division == 'Bacillariophyta', 'totbiom'] +
             pdat[pdat$division == 'Chlorophyta', 'totbiom'] + 
             pdat[pdat$division == 'Non-Cyanophytes', 'totbiom'])
sumphy

totbiomass = sumphy$totbiom
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col="#3C3B3D" , border=F)


##Chlorophyta+ Bacillariophyta

sumphy = c(pdat[pdat$division == 'Bacillariophyta', 'totbiom'] +
             pdat[pdat$division == 'Chlorophyta', 'totbiom'])
sumphy

totbiomass = sumphy$totbiom
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col='#6D6C70', border=F)


#Bacillariophyta

sumphy = c(pdat[pdat$division == 'Bacillariophyta', 'totbiom'])
sumphy

totbiomass = sumphy$totbiom
totb = data.frame(doy, totbiomass)
totb

polygon(
  c(min(totb$doy), totb$doy , max(totb$doy)) , 
  c(0 , totb$totbiomass , 0),  col='#A2A1A6', border=F)

# Create Legends #====================

# Create zooplankton legend 
windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend('center', legend = taxa, 
       pch=15, bty = 'n',
       pt.cex=2.5, cex=0.8,
       col = col, ncol=2)

# Create Phytoplankton Legend 

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend('center', legend = rev(c('Bacillariophyta','Chlorophyta', 'Chryso-& Cryptophytes', 
                               expression(italic('Aphanothece')), expression(italic('Microcystis')), 'Other Cyanophytes')), 
       pch=15, bty='n',
       pt.cex=2.5, cex=0.8,
       col = c("#00441b", "#5aae61","#d9f0d3", "#3C3B3D", '#6D6C70', '#A2A1A6'))

# Zooplankton: Phytoplankton Biomass Ratio #========================
# Total Biomass 
# Set working directory to folder containing datasets derived from raw data 
zoop_biomass = group_sums # Zooplankton biomass information 
phyto_biomass = pdat # Phytoplankton biomass information 

zoob_tot <- zoop_biomass %>% # Get total zooplankton biomass per day 
  group_by(doy) %>%
  summarise(
    zp_totalbiom = sum(biomass)) %>%
  ungroup() %>%
  arrange(doy)
as_tibble(zoob_tot)

# Rename DOY 171 to DOY 172 - sample ID mishap between zooplankton and phytoplankton sample IDs, 
## samples taken on DOY 172
doy = 172
zp_totalbiom = 110.150061
x = data.frame(doy, zp_totalbiom)
x

zoob = rbind(x,zoob_tot) %>% arrange(doy) %>% filter(!(doy==171))
zoob

# Format phytoplankton biomass 
phyb = phyto_biomass %>% # Get total phytoplankton biomass per day 
  group_by(doy) %>%
  summarise(
    phy_totalbiom = sum(totbiom)) %>% 
  ungroup() %>%
  arrange(doy) %>% 
  mutate(phy_totalbiom = phy_totalbiom*1000) # Convert phytoplankton from mg -> ug 
as_tibble(phyb)

join_biomass = left_join(phyb, zoob, by = 'doy')
join_biomass # Each in ug/L 

ratio = join_biomass %>% mutate(z2p = (zp_totalbiom/phy_totalbiom)*100)
ratio

