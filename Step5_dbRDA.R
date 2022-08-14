## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#=================================================================================================##
# STEP 5: REDUNDANCY ANALYSIS OF EXCRETION & ENVIRONMENTAL VARIABLES ON PHYTOPLANKTON COMMUNITY 
#=================================================================================================##
## NOTE: Be sure to run Steps 1-4 first ## 

#Format phytoplankton community data #=============================
phy_biomass

phy_community = phy_biomass %>% 
  rename(doy = DOY) %>% # make column name consistent with other datasets 
  select(!c(DIVISION, TAXON_GROUPING)) %>% # taxonomic grouping will be joined later with taxonomic groupings 
  arrange(doy) %>%
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
# Explanatory Variables: Inorganic N, Inorganic P, 
# Zoop Excretion N & P, Zoop Body Stoich, temperature, pH, total dissolved solids 
# Extra added: Zoop excretion N:P ratio, Zoop Biomass

# Read in necessary data 

# INORGANIC NUTRIENTS & DAILY ZOOPLANKTON EXCRETION OF N AND P & EXCRETION N:P RATIO
#=============================================================#
# Take derived data from Step4: and combine them 
exc_estimate_rda = filter(exc_estimate, !(doy == 234 | doy == 273))
gvl19_join = gvl19 %>%
  select(doy, SRP_mol, NOx_mol, orgP_mol, orgN_mol, TN_mol, TP_mol) %>%
  mutate(inorgP_uM = (SRP_mol*1000000), 
         inorgN_uM = (NOx_mol*1000000), 
         orgP_uM = (orgP_mol*1000000), 
         orgN_uM = (orgN_mol*1000000), 
         totalN_uM = (TN_mol*1000000),
         totalP_uM = (TP_mol*1000000)) %>%
  select(doy, inorgP_uM, inorgN_uM, orgP_uM, orgN_uM, totalP_uM, totalN_uM) %>%
  as_tibble()
gvl19_join

gv_pools = left_join(gvl19_join, zp_uM_sum, by='doy')
gv_pools

gv_uM_stocks = gv_pools %>% 
  select(doy,inorgN_uM, inorgP_uM) %>% # select useful column 
  as_tibble()
gv_uM_stocks
gvl_inorg_rda = filter(gv_uM_stocks, !(doy == 157 | doy == 234 | doy == 273)) # Need to match the phytoplankton and zooplankton datasets 
## removed DOYs where there's a lost or missing sample in either zooplankton or phytoplankton community data 

# Create vectors of each explanatory variable for standardization later # 
inorg_P = gvl_inorg_rda$inorgP_uM
inorg_N = gvl_inorg_rda$inorgN_uM
exc_P = exc_estimate_rda$Pexc
exc_N = exc_estimate_rda$Nexc

# Create vector of zooplankton excretion N:P, need to make molar ratio first # 
Hebert_tot_exc_sum_e # ug N or P per day per L, estimate from Step4

# Convert to Molar ratios  # 
Hebert_exc_np = Hebert_tot_exc_sum_e %>%
  rename(Nexc = H_ug_Nexcrete_sum_d, 
         Pexc = H_ug_Pexcrete_sum_d) %>% 
  mutate(Pexc_M = Pexc/30970000, # ug N or P per day per L to uM N or P per day  
         Nexc_M = Nexc/14010000) %>%
  select(doy, Pexc_M, Nexc_M) %>% 
  mutate(exc_np = Nexc_M/Pexc_M) %>%
  filter(!(doy == 234 | doy == 273))
Hebert_exc_np 

exc_NP = Hebert_exc_np$exc_np

# ZOOPLANKTON N:P RATIOS # 
#=============================================================#
zp_stoic_sum # Zooplankton community N:P ratios from Step3 
zp_stoic_sum_rda = filter(zp_stoic_sum, !(doy == 234 | doy == 273))

# Create vector of explanatory variable for standardization later # 
zoop_np = zp_stoic_sum_rda$zp_np

# ZOOPLANKTON BIOMASS # 
#==============================================================# 
zp_totbiomass = zp_raw %>% 
  select(doy, group, biomass) %>%
  group_by(doy) %>%
  summarize(biomass = sum(biomass)) %>%
  ungroup() %>%
  filter(!(doy == 234 | doy == 273))
zp_totbiomass

# Create vector of explanatory variable for standardization later # 
zoop_biomass = zp_totbiomass$biomass

# WATER QUALITY DATA FROM EXO SONDE 
# Load in high frequency EXO Sonde data 
gv_exo_hf # Need to match sample dates of other explanatory variables 

hf_select = gv_exo_hf %>% 
  select(doy, temp, ph, tds, chl) %>%
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
            avg_chl = mean(chl, na.rm = T))
hf_select  
hf_var = c(hf_select$doy, hf_select$avg_temp, hf_select$avg_ph)
temp = hf_select$avg_temp 
ph = hf_select$avg_ph


# Explanatory variable are dimensionally non-homogeneous so we will Z-score the data to standardize it 
doy = c(143, 150, 164, 172, 178, 192, 199, 206,211,213,220, 227, 245, 251)
`inorganic P` = ((inorg_P - mean(inorg_P, na.rm = T))/sd(inorg_P, na.rm = T))
`inorganic N` = ((inorg_N - mean(inorg_N, na.rm = T))/sd(inorg_N, na.rm = T))
#`P excretion` = ((exc_P - mean(exc_P, na.rm = T))/sd(exc_P, na.rm = T))
#`N excretion` = ((exc_N - mean(exc_N, na.rm = T))/sd(exc_N, na.rm = T))
`excretion N:P` = ((exc_NP - mean(exc_NP, na.rm = T))/sd(exc_NP, na.rm = T))
`zoop N:P` = ((zoop_np - mean(zoop_np, na.rm = T))/sd(zoop_np, na.rm = T))
`zoop biomass` = ((zoop_biomass - mean(zoop_biomass, na.rm = T))/sd(zoop_biomass, na.rm = T))
`temp` = ((temp - mean(temp, na.rm = T))/sd(temp, na.rm = T))
`pH` = ((ph - mean(ph, na.rm = T))/sd(ph, na.rm = T))
env = data.frame(doy, `inorganic P`, `inorganic N`, `excretion N:P`, `zoop N:P`,  
                 `zoop biomass`, `temp`, `pH`)
env # Data frame of environmental explanatory variables 
env_explanatory = env[,2:8]
row.names(env_explanatory) = env$doy

# Run distance-based RDA of the data #============================== 
species.hell # Phytoplankton community composition data hellinger transformed with species that occur once and/or contributed <1% biomass removed
env_explanatory # Environmental variables standardized via Z-scoring due to dimensional non-homogeneity 

rankindex(env_explanatory, species.hell, indices = c('euc', 'man', 'gow', 'bra', 'kul'), stepacross = F, method = 'kendall', na.action = na.omit)
# Manhattan has best correlation but it does a really poor job with double-zero data
# Kulczynski is better for biogeographic data rather than community assembly
# Will go with Bray-Curtis as it is in the top 3 and is the standard for this type of species composition data

## TEST ## 
# Assess ranking correlation after taking an average of the last valuebefore and after DOY 245 of inorganic nutrients 
## to see if NAs are screwing with the data frame  
test_P = env_explanatory %>%
  select(inorganic.P)
test_N = env_explanatory %>%
  select(inorganic.N)

test_P
mean(1.08940659, 1.22487336)
test_P = test_P %>% mutate(inorganic.P = replace_na(inorganic.P, 1.089407))
test_P

test_N
mean(-0.4599057, -0.7745881)
test_N = test_N %>% mutate(inorganic.N = replace_na(inorganic.N, -0.4599057))
test_N

env_explanatory2 = env_explanatory %>%
  cbind(test_P) %>%
  cbind(test_N)
env_explanatory2
env_explanatory2 = env_explanatory2[,3:9]
env_explanatory2

rankindex(env_explanatory2, species.hell, indices = c('euc', 'man', 'gow', 'bra', 'kul'), stepacross = F, method = 'kendall')
# Correlation isn't really different than just doing na.omit so will continue to do na.omit 

# Change row names from DOY to ordered list (1 - 14)
row.names(env_explanatory) = c(1:14)
row.names(species.hell) = c(1:14)

# Run the full model with all explanatory variables # 
full = dbrda(species.hell~., distance = 'bray', data = env_explanatory2, na.action=na.omit, sqrt.dist = T) 
anova.cca(full) # p = 0.005 

# Full model is significant so Type I error is not being inflated 

# To prevent adding too many explanatory variables, compute the global R^2 adj as a second stopping criteria
# global R2 
global_r2 = RsquareAdj(full)$adj.r.squared
global_r2

# Forward Selection using ordistep # 
mod0 = dbrda(species.hell~1, distance = 'bray', data = env_explanatory2, na.action=na.omit, sqrt.dist = T)
mod1 = dbrda(species.hell~., distance = 'bray', data = env_explanatory2, na.action=na.omit, sqrt.dist = T)

## With scope present, default direction is both ## 
step.res = ordiR2step(mod0, mod1, perm.max=999, R2scope = global_r2, direction='forward')
step.res$anova # finds P excretion and inorganic N as the primary explanatory variables 

## dbRDA result with selected variables 
rda2 = dbrda(species.hell~temp+inorganic.N, data=env_explanatory2, distance = 'bray')

# Check the global R2 with new model 
RsquareAdj(full)$adj.r.squared # Using 7 explanatory variables
# 0.2978916 
RsquareAdj(rda2)$adj.r.squared # Using 2 explanatory variables
# 0.3316421

## 2 explanatory variables explain almost as much as the RDA using 9 explanatory variables

# Check for multicollinearty before and afterwards # 
sqrt(vif.cca(full)) # Extremely high collinearty 
sqrt(vif.cca(rda2)) # No collinearty, all sqrt(VIF) under 2

# Assess Unexplained Variance # 
rda2$CA$eig[rda2$CA$eig > mean(rda2$CA$eig)] # Pretty high unexplained variance 

# Best model only includes temp and N.excretion 
env_explanatory_select = select(env_explanatory, c(temp, inorganic.N))
env_explanatory_select

part = dbrda(species.hell~., distance = 'bray', data = env_explanatory_select, na.action=na.omit, sqrt.dist = T)

# Is the model significant? 
anova.cca(part) # p = 0.001
anova.cca(part, by = 'axis', perm.max=999) # Test axes for significance 
anova.cca(part, by = 'terms', permu=999) # Tests for significance of explanatory variables 
summary(part) 
# Get the adjusted Rsquared
R2adj = RsquareAdj(part)$adj.r.squared
R2adj # Unbiased amount of explained variation, model explains 21.88% of variation in the phytoplankton community 

# Plot model results #=================================
windows(height = 4, width = 6)
par(omi=c(0.9,0.9,0.5,0.5), mai=c(0.1,0.2,0.1,0.1))
plot(part, type = 'n', ylim=c(-1.5,2.5), xlim=c(-5,3))
mtext(side=1, 'dbRDA1 (23.56%)', line=2, cex=1)
mtext(side=2, 'dbRDA2 (11.35%)', line=2, cex=1)
points(part, pch=19, 'sites', cex = 1.5, col = c(rep('#008a64', 3), rep('#630060', 10)))
text(part, 'sites', cex= 0.9, col = 'black', adj=c(-0.5,0.1), font=2)
text(part, 'bp', col = 'black', cex = 0.8)
legend('topleft', legend = c('Pre-Cyano Bloom', 'Post-Cyano Bloom'), 
       pch = 19, bty='n', col=c('#008a64', '#630060'))

