## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#=================================================================================================##
# STEP 5: REDUNDANCY ANALYSIS OF EXCRETION & ENVIRONMENTAL VARIABLES ON PHYTOPLANKTON COMMUNITY 
#=================================================================================================##
## NOTE: Be sure to run Step1_Dataset Tidying first ## 

#Format phytoplankton community data #=============================
phy_biomass

phy_community = phy_biomass %>% 
  rename(doy = Day) %>% # make column name consistent with other datasets 
  select(!(DIVISION)) %>% # taxonomic grouping will be joined later with taxonomic groupings 
  filter(Treatment == 'Pre') %>% # Data come from a mesocosm experiment from a separate study. 
# The 'Pre' treatment represents phytoplankton collected the day of sampling. 'Post' represents phytoplankton following 24 hours of zooplankton grazing in a 10 L mesocosm. 
  arrange(doy) %>%
  select(!(Treatment)) %>% # All values are pre so no longer need this identifier column 
  pivot_wider(names_from = TAXON, values_from = BIOMASS.MG.L) %>% # columns as species and rows as sites (i.e., sampling day)
  select(!c(SAMPLE.ID, LAKE.NO)) # The samples are all from one lake and the DOY specifies which sample is which so these columns are uneccesary 
phy_community[is.na(phy_community)] = 0 # NAs in this case represent true 0 values, species was not identified in the sample 
phy_community # Check to make sure dataset is formatted correctly and to get number of columns 

# We will be comparing the phytoplankton community to variables derived from zooplankton so have to remove DOY 157 which was a lost sample for zooplankton
phy_community = filter(phy_community, !(doy == 157))

# Put data into a matrix format # 
phy_community.df = as.data.frame(phy_community[,2:34])
row.names(phy_community.df) = phy_community$doy

# To reduce the weight of rare species and species that had very low biomass 
# I removed species that had <1% of biomass per DOY as well as species that only ever occurred once
total_col = apply(phy_community.df[,-1], 1, sum)
pcts = lapply(phy_community.df[,-1], function(x) {
  (x/total_col)*100
})
phy_assess = as.data.frame(pcts) # Contains species that only occur once and contribute less than 1% biomass 
drops = c(drops = c('Asterionella', 'Cosmarium', 'Elakatothrix', 'Staurastrum', 'Trachelomonas', 'Unknown centric bacillariophyte', 'Woronichinia', 'Monoraphidium', 'Mallomonas', 'Euglena'))

# remove those species 
species.cull = phy_assess[, !(names(phy_assess) %in% drops)]
species.cull

# Perform a Hellinger transformation to deal with zero-inflated species data 
species.hell = decostand(species.cull, method = 'hellinger')

# Format environmental data to use as explanatory variables #==============================
# Explanatory Variables: Inorganic N, Inorganic P, Zoop Excretion N & P, Zoop Body Stoich, temperature, pH, total dissolved solids 
# Read in necessary data 

# INORGANIC NUTRIENTS & DAILY ZOOPLANKTON EXCRETION OF N AND P 
#=============================================================#
# Take derived data from Step4: and combine them 
exc_estimate_rda = filter(exc_estimate, !(doy == 234 | doy == 273))
gvl_inorg_rda = filter(gvl_inorg, !(doy == 157 | doy == 234 | doy == 273)) # Need to match the phytoplankton and zooplankton datasets 
## removed DOYs where there's a lost or missing sample in either zooplankton or phytoplankton community data 

# Create vectors of each explanatory variable for standardization later # 
inorg_P = gvl_inorg_rda$SRP_ugL
inorg_N = gvl_inorg_rda$NOx_mgL
exc_P = exc_estimate_rda$Pexc
exc_N = exc_estimate_rda$Nexc

# ZOOPLANKTON N:P RATIOS # 
#=============================================================#
zp_stoic_sum # Zooplankton community N:P ratios from Step3 
zp_stoic_sum_rda = filter(zp_stoic_sum, !(doy == 234 | doy == 273))

# Create vector of explanatory variable for standardization later # 
zoop_np = zp_stoic_sum_rda$zp_np

# WATER QUALITY DATA FROM EXO SONDE 
# Load in high frequency EXO Sonde data 
gv_exo_hf # Need to match sample dates of other explanatory variables 

hf_select = gv_exo_hf %>% 
  select(doy, temp, ph, tds) %>%
  filter(doy == 143 | # Match Sampling Dates 
           doy == 150 | 
           doy == 164 | 
           doy == 172 | 
           doy == 178 | 
           doy == 192 | 
           doy == 199 | 
           doy == 206 |
           doy == 211 |
           doy == 213 | 
           doy == 220 | 
           doy == 227 | 
           doy == 245 | 
           doy == 251) %>%
  group_by(doy) %>%
  summarise(avg_temp = mean(temp, na.rm =T), # Get the daily average of these explanatory variables 
            avg_ph = mean(ph, na.rm = T), 
            avg_tds = mean(tds, na.rm = T))
hf_select  
hf_var = c(hf_select$doy, hf_select$avg_temp, hf_select$avg_ph, hf_select$avg_tds)
temp = hf_select$avg_temp 
ph = hf_select$avg_ph
tds = hf_select$avg_tds

# Explanatory variable are dimensionally non-homogeneous so we will Z-score the data to standardize it 
doy = c(143, 150, 164, 172, 178, 192, 199, 206,211,213,220, 227, 245, 251)
`inorganic P` = ((inorg_P - mean(inorg_P, na.rm = T))/sd(inorg_P, na.rm = T))
`inorganic N` = ((inorg_N - mean(inorg_N, na.rm = T))/sd(inorg_N, na.rm = T))
`P excretion` = ((exc_P - mean(exc_P, na.rm = T))/sd(exc_P, na.rm = T))
`N excretion` = ((exc_N - mean(exc_N, na.rm = T))/sd(exc_N, na.rm = T))
`zoop N:P` = ((zoop_np - mean(zoop_np, na.rm = T))/sd(zoop_np, na.rm = T))
`temp` = ((temp - mean(temp, na.rm = T))/sd(temp, na.rm = T))
`pH` = ((ph - mean(ph, na.rm = T))/sd(ph, na.rm = T))
`TDS` = ((tds - mean(tds))/(sd(tds)))
env = data.frame(doy, `inorganic P`, `inorganic N`, `P excretion`, `N excretion`, `zoop N:P`, 
                 `temp`, `pH` , `TDS`)
env # Data frame of environmental explanatory variables 
env_explanatory = env[,2:9]
row.names(env_explanatory) = env$doy

# Run distance-based RDA of the data #============================== 
species.hell # Phytoplankton community composition data hellinger transformed with species that occur once and/or contributed <1% biomass removed
env_explanatory # Environmental variables standardized via Z-scoring due to dimensional non-homogeneity 

rankindex(env_explanatory, species.hell, indices = c('euc', 'man', 'gow', 'bra', 'kul'), stepacross = F, method = 'kendall', na.action = na.omit)
# Manhattan has best correlation but it does a really poor job with double-zero data 
# Will go with Bray-Curtis as it is in the top 3 and is the standard for this type of species composition data

## TEST ## 
# Assess ranking correlation after taking an average of the last valuebefore and after DOY 245 of inorganic nutrients 
## to see if NAs are screwing with the data frame  
test_P = env_explanatory %>%
  select(inorganic.P)
test_N = env_explanatory %>%
  select(inorganic.N)

test_P
mean(1.12980565, 1.26280282)
test_P = test_P %>% mutate(inorganic.P = replace_na(inorganic.P, 1.129806))
test_P

test_N
mean(-0.5163036, -0.8260857)
test_N = test_N %>% mutate(inorganic.N = replace_na(inorganic.N, -0.5163036))
test_N

env_explanatory2 = env_explanatory %>%
  cbind(test_P) %>%
  cbind(test_N)
env_explanatory2 = env_explanatory2[,3:10]
env_explanatory2

rankindex(env_explanatory2, species.hell, indices = c('euc', 'man', 'gow', 'bra', 'kul'), stepacross = F, method = 'kendall')
# Correlation isn't really different than just doing na.omit so will continue to do na.omit 

# Change row names from DOY to ordered list (1 - 14)
row.names(env_explanatory) = c(1:14)
row.names(species.hell) = c(1:14)

# Run the full model with all explanatory variables # 
full = dbrda(species.hell~., distance = 'bray', data = env_explanatory, na.action=na.omit, sqrt.dist = T) 


# Forward and backward stepwise regression to assess multicollinearity
ordistep(full, direction = 'both', pstep=1000, R2scop=TRUE) # R2scope only accepts models with lower adjusted R2

# Best model excludes pH and zooplankton N:P in this case 
env_explanatory_select = select(env_explanatory, !c(zoop.N.P, pH))
env_explanatory_select

part = dbrda(species.hell~., distance = 'bray', data = env_explanatory_select, na.action=na.omit, sqrt.dist = T)

# Is the model significant? 
anova.cca(part) # p = 0.002
anova.cca(part, by = 'axis', perm.max=999) # Test axes for significance 
anova.cca(part, by = 'terms', permu=999) # Tests for significance of explanatory variables 
summary(part) 
# Get the adjusted Rsquared
R2adj = RsquareAdj(part)$adj.r.squared
R2adj # Unbiased amount of explained variation, model explains 41.93% of variation in the phytoplankton community 

# Plot model results #=================================
windows(height = 4, width = 6)
par(omi=c(0.9,0.9,0.5,0.5), mai=c(0.1,0.2,0.1,0.1))
plot(part, type = 'n', ylim=c(-1.5,2.5), xlim=c(-5,3))
mtext(side=1, 'dbRDA1 (28.4%)', line=2, cex=1)
mtext(side=2, 'dbRDA2 (13.4%)', line=2, cex=1)
points(part, pch=19, 'sites', cex = 1.5, col = c(rep('#008a64', 3), rep('#630060', 10)))
text(part, 'sites', cex= 0.9, col = 'black', adj=c(-0.5,0.1), font=2)
text(part, 'bp', col = 'black', cex = 0.8)
legend('topleft', legend = c('Pre-Cyano Bloom', 'Post-Cyano Bloom'), 
       pch = 19, bty='n', col=c('#008a64', '#630060'))

