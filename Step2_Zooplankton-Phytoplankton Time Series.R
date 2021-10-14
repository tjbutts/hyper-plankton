## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts October 2021

## NOTE: Be sure to run Step1_Dataset Tidying first ## 

# Datasets 
zp_raw
phy_biomass
phy_grouping 

# Zooplankton Biomass Time Series #==========================
# Select Color Scheme # 
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
display.brewer.pal(9, 'BuPu')
box()
brewer.pal(9, 'BuPu') # Light to Dark = Left to Right

# Sum Zooplankton group totals to construct polygons 
group_sums = zp_raw %>% select(doy, group, taxon, biomass) %>% 
  group_by(doy, group) %>% 
  summarise(biomass = sum(biomass)) %>%
  ungroup()

# Set dimensions for figure #
windows(height=6,width=6)
par(mfrow=c(2,1), omi=c(0.9,0.5,0.5,0.5), mai=c(0.75,0.2,0.1,0.9))

plot(0,0, pch=NA, xlab="", ylab="", xlim=c(143,280), ylim=c(0,275),xaxt="n", cex.axis=1.1)
axis(1, at=c(140,150,160,170, 180, 
             190, 200, 210, 220, 230, 
             240, 250, 260, 270, 280),labels=c("140","","160","","180","",'200', '',"220","","240","","260","", '280' ), cex.axis=1.1)
mtext(side=2, line=2, cex = 1.2, expression(Zooplankon~Biomass~"("*mu*g~L^-1*")"), font = 2)

doy = c(143,150,164,171,178,192,199,206,211,213,220,227,234,245,251,273)
doy = data.frame(doy)
doy
