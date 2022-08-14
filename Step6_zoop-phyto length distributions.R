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
gv_gald 
gv_length_mass

gv_length = gv_length_mass %>%
  

# Format data set for ggplot and convert GALD units from ocular units to micrometers 
ridge = gv_gald_length %>%
  pivot_longer(cols = c(gald, length), names_to = 'measure', values_to = 'value')
ridge

ridge_fct = ridge %>% 
  mutate(doyfct = fct_rev(as.factor(doy))) # Not sure why I have to do this, but it's the only way the DOY aligns correctly 

doyfct_select1 = ridge_fct %>% filter(!c(doy == 157 | doy ==234 | doy == 273))
doyfct_select2 = unique(doyfct_select1$doyfct)

fsr_lines_min = data.frame(doyfct = doyfct_select2, 
                           x0 = c(zp_fsr_weighted$wm_min))
fsr_lines_max = data.frame(doyfct = doyfct_select2, 
                           x1 = c(zp_fsr_weighted$wm_max))




windows(height=6, width=3)
p1 = ggplot(
  ridge_fct, 
  aes(y = doyfct)) +
  geom_density_ridges( # Create the density plots for both phytoplankton GALD and zooplankton length 
    aes(x = value, fill = paste(doyfct, measure)), jittered_points = FALSE, 
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  geom_segment(data = fsr_lines_min, aes(x=x0, xend=x0, y=as.numeric(doyfct),
                                         yend=(as.numeric(doyfct) + 0.9), 
                                         col='black', inherit.aes = FALSE)) +
  geom_segment(data = fsr_lines_max, aes(x=x1, xend=x1, y=as.numeric(doyfct), 
                                         yend=(as.numeric(doyfct) + 0.9), 
                                         col='black', inherit.aes = FALSE)) +
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

# PLOT: % GALD within zooplankton community feeding range 
ridge
zp_fsr_weighted2 = zp_fsr_weighted %>% select(!(doyfct))

gald_range = ridge %>% filter(measure == 'gald')
gald_range

join_fsr = left_join(gald_range, zp_fsr_weighted2, by ='doy')
join_fsr 

fsr_percent = join_fsr %>% 
  mutate(within_range = case_when(value >= wm_min & value <= wm_max ~ 'within'))
fsr_percent$within_range = fsr_percent$within_range %>% replace_na('outside') 
fsr_percent2 = fsr_percent %>% 
  mutate(count = 1)
fsr_percent2

fsr_percent_wide = fsr_percent2 %>% # Make the value inside or outside of the feeding range a categorical variable
  select(doy, within_range, count) %>% 
  mutate(row = seq.int(nrow(fsr_percent2))) %>%
  pivot_wider(names_from = within_range, values_from = count)
fsr_percent_wide[is.na(fsr_percent_wide)] <- 0
fsr_percent_wide

fsr_percent_wide$within = factor(fsr_percent_wide$within, c(0,1), labels = c('outside', 'within'))
fsr_percent_wide$doy = factor(fsr_percent_wide$doy)

cross = table(fsr_percent_wide$within, fsr_percent_wide$doy)
cross

percents = round(prop.table(cross, 2)*100, digits=0)
percents.m = as.matrix(percents)
percents = percents.m[, c(1,2,4,5,6,7,8,9,10,11,12,13,15,16)]
percents

doy = c(143,150,164, 172, 178, 192, 199, 206, 211,213,220,227,245,251)
percent_within = c(53,38,13,4,37,0,3,25,38,65,68,50,87,74)
fsr_percent_fin = as.data.frame(percent_within, row.names = doy)
fsr_percent_fin$doy = doy
fsr_percent_fin = fsr_percent_fin %>% 
  select(doy, percent_within) %>%
  mutate(doy = as.factor(doy)) %>%
  as.data.frame()
fsr_percent_fin

windows(height=3, width=5)
par(mai=c(0.9,0.9,0.6,0.5))
barplot(percents, col = c('black', 'gray90'))
box()
mtext(side=2, text='Percent GALD', line=2.5)
mtext(side=1, text='Day of Year, 2019', line = 2.5)

windows(height=5, width=5)
par(mai=c(0.9,1,0.6,1))
plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
legend('center', legend = c('Within', 'Outside'), 
       pch=15,
       pt.cex=2.5, cex=0.8,
       col = c('gray90', 'black'))
