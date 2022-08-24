## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#=================================================================================================##
# STEP 5: DENSITY RIDGELINE PLOT OF PHYTOPLANKTON GALD AND ZOOPLANKTON LENGTH
#=================================================================================================##
## NOTE: Be sure to run Steps 1-5 first ## 

## Calculate zooplankton community feeding size range 
## Calculate the weighted mean of the minimum and maximum size range (weight based on biomass) 
## Use the min and the max of that weighted mean to get a range of effective community food size range ## 
# Additional Dataset 
gv_foodsize # Zooplankton feeding size range 
zp_raw # Zooplankton biomass data 

zp_raw # Zooplankton biomass information 
zp_raw$group <- as.factor(zp_raw$group) # makes the group column a factor, easier for later analysis 

gv19_zp = zp_raw %>% select(sampleid,doy, taxon,group,biomass) %>% mutate(doy = replace(doy, doy == 171, 172))
as_tibble(gv19_zp)


# Combine biomass with fsr trait data 
zp_fsr <- left_join(gv19_zp, gv_foodsize, by='taxon') # Join fsr with biomass data 
as_tibble(zp_fsr)
zp_fsr %<>% select(sampleid, doy, taxon, group.y, biomass, min_fsr, max_fsr) %>% # Combine into one dataset 
  rename(group = group.y) %>% as_tibble()
as_tibble(zp_fsr) 

# Calculate the weighted mean for the minumum and maximum fsr per day of year # 
zp_fsr_weighted = zp_fsr %>%
  group_by(doy) %>%
  summarise(wm_min = weighted.mean(x=min_fsr, w=biomass, na.rm = T), 
            wm_max = weighted.mean(x=max_fsr, w=biomass, na.rm = T)) %>%
  ungroup() %>%
  filter(!c(doy == 157 | doy == 234 | doy == 273)) %>%
  mutate(doyfct = fct_rev(as.factor(doy)))
zp_fsr_weighted # weighted minimum and maximum feeding size range 

# Phytoplankton GALD v. Zooplankton length - combine into one dataset
gv_gald = gv_gald %>% select(doy, gald)
gv_length = gv_length_mass %>%
  select(doy, length)

# The following datasets are the combination of the phytoplankton GALD and zooplankton length data.
## The analysis requires GALD and length to be in one data frame and two separate columns 
## For example, if there are more GALD observations than length observations, then the difference must be NAs in the zooplankton column. If there are 20 more GALD measurements than length measurements for DOY 143, then there need to be 20 NAs added to the zooplantkon column for DOY 143 to match the length of the GALD column 
# Combine GALD and Length datasets # 
## Add NAs to GALD data in order to match 
# Make GALD NA data frame 
doy = c(rep(150, 56), rep(164, 108), rep(172, 118), rep(178, 35), 
        rep(192, 37), rep(199, 22), rep(206, 26), rep(211, 24), 
        rep(213, 2), rep(227, 16), rep(234, 66), rep(251, 4), 
        rep(273, 65))
galdNAs = as.data.frame(doy)
galdNAs['gald'] <- NA

# Make length NA data frame 
doy= c(rep(143, 248), rep(157, 28), rep(220, 39), rep(245, 1))
lengthNAs = as.data.frame(doy)
lengthNAs['length'] <- NA 

# Measurement in DOY 211 entered in error. As entered it is 11215 um, should be 1215 um.
#lengthNAs = replace(lengthNAs$length, lengthNAs$length >11214, 1215)

# Make phytoplankton GALD - Zooplankton length data frame # 
# rbind galdNAs to gald data frame 
gv_gald_NAs = rbind(gv_gald, galdNAs) %>% arrange(doy)
gv_gald_NAs

# rbind lengthNAs to length data frame 
gv_length_NAs = rbind(gv_length, lengthNAs) %>% arrange(doy) %>% select(length)
gv_length_NAs$length[gv_length_NAs$length==11215] <- 1215

gv_gald_length = as_tibble(cbind(gv_gald_NAs, gv_length_NAs))
gv_gald_length

# Format data set for ggplot and convert GALD units from ocular units to micrometers 
ridge = gv_gald_length %>%
  pivot_longer(cols = c(gald, length), names_to = 'measure', values_to = 'value')
ridge

ridge_fct = ridge %>% 
  mutate(doyfct = fct_rev(as.factor(doy))) # Not sure why I have to do this, but it's the only way the DOY aligns correctly 

doyfct_select1 = ridge_fct %>% filter(!c(doy == 157 | doy ==234 | doy == 273))
doyfct_select2 = unique(doyfct_select1$doyfct)

windows(height=6, width=3)
p1 = ggplot(
  ridge_fct, 
  aes(y = doyfct)) +
  geom_density_ridges( # Create the density plots for both phytoplankton GALD and zooplankton length 
    aes(x = value, fill = paste(doyfct, measure)), jittered_points = FALSE, 
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  scale_y_discrete(expand = c(0.01, 0)) + 
  labs( # Axis labels 
    x = "Length (um)",
    y = "Day of Year, 2019"
  ) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + # Present data in a log scale
  annotation_logticks(sides = 'b') +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size =11, colour = 'black')) + 
  xlab(label = bquote(Length~(mu*m))) + 
  ylab(label = 'Day of Year, 2019') +
  scale_fill_cyclical( # Create a gradient color scheme to differentiate time 
    breaks = c("gald", "length"),
    labels = c(`gald` = "phytoplankton GALD", `length` = "zooplankton length"),
    values = c('#008a64', '#630060', #143
               '#009d73','#780075',
               '#00b181',
               '#00c48f','#8e008a',
               '#00d89d','#a4009f', #172
               '#00ecac','#b900b4',
               '#00ffba','#cf00ca',
               '#14ffbf','#e500df', #199
               '#27ffc5','#fa00f4',
               '#3bffca','#ff11f9',
               '#4fffcf','#ff67fb', #213
               '#62ffd4','#ff7dfc', 
               '#76ffda','#ff93fc', 
               '#ffa8fd', #234
               '#89ffdf', '#ffa8fd', 
               '#9dffe4', '#ffbefd', 
               '#ffd4fe'),
    name = "measure", guide = "legend") 
p1

# Create second plot of mean difference between GALD and zooplankton length
gald = ridge_fct %>% 
  filter(measure == 'gald') %>%
  select(!(doyfct)) %>%
  as_tibble()

length = ridge_fct %>%
  filter(measure == 'length') %>%
  select(!(doyfct)) %>%
  as_tibble()

gald
length

# mean 
gald_mean = gald %>%
  group_by(doy) %>%
  summarise(gald_avg = mean(value, na.rm = T)) %>%
  ungroup()
gald_mean

length_mean = length %>%
  group_by(doy) %>%
  summarise(length_avg = mean(value, na.rm = T)) %>%
  ungroup()
length_mean

# Combine length-GALD data 
comb = left_join(length_mean, gald_mean, by='doy')
comb

comb = comb %>%
  mutate(diff = length_avg - gald_avg) %>%
  mutate(doy_fct = as.factor(doy))
comb

windows(height=6, width=4)
p2 = ggplot(
  ridge_fct, 
  aes(y = doyfct)) +
  geom_density_ridges(
    aes(x = value, fill = paste(doyfct, measure)), 
    alpha = .8, color = "white", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  xlim(0,850) +
  theme_bw() + 
  geom_point(data=comb, mapping = aes(x=diff, y=doy_fct, size=3)) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size = 11, colour = 'black')) + 
  theme(axis.title.y=element_blank()) + 
  xlab(label = 'Length-GALD Mean Difference') +
  theme(legend.position = 'none') +
  scale_fill_cyclical(
    breaks = c("gald", "length"),
    labels = c(`gald` = "phytoplankton GALD", `length` = "zooplankton length"),
    values = c('white'),
    name = "measure", guide = "legend") 
p2

# Legend # 
# Legend
windows(height=3, width=6)
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend("center", legend = c('Phytoplankton GALD', 'Zooplankton Length'), 
       pch=15, 
       pt.cex=3, cex=1.5, bty='n',
       col = c('#008a64', '#630060'))
