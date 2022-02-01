## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts November 2021

#=================================================================================================##
# STEP 5: DENSITY RIDGELINE PLOT OF PHYTOPLANKTON GALD AND ZOOPLANKTON LENGTH
#=================================================================================================##
## NOTE: Be sure to run Steps 1-5 first ## 

# Phytoplankton GALD v. Zooplankton length 
gv_gald_length 

# Format data set for ggplot and convert GALD units from ocular units to micrometers 
ridge = gv_gald_length %>%
  mutate(gald = gald*2.5) %>% # Convert GALD measurement from ocular units to micrometers 
  pivot_longer(cols = c(gald, length), names_to = 'measure', values_to = 'value')
ridge

ridge_fct = ridge %>% 
  mutate(doyfct = fct_rev(as.factor(doy))) # Not sure why I have to do this, but it's the only way the DOY aligns correctly 

windows(height=6, width=3)
p1 = ggplot(
  ridge_fct, 
  aes(y = doyfct)) +
  geom_density_ridges( # Create the density plots for both phytoplankton GALD and zooplankton length 
    aes(x = value, fill = paste(doyfct, measure)), 
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
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

