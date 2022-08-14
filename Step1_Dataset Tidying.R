## Green Valley Zooplankton Nutrient Recycling Project ###
# Code originally written by TJ Butts August 2022

#============================================#
# STEP 1: LOAD IN DATASETS 
#============================================#
rm(list=ls())
graphics.off()

# Required Libraries for analysis and visualization
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(magrittr)) install.packages('magrittr')
library(magrittr)
if (!require(vegan)) install.packages('vegan')
library(vegan)
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
if (!require(ggridges)) install.packages('ggridges')
library(ggridges) 
if (!require(scales)) install.packages('scales')
library(scales) 
if (!require(ggpubr)) install.packages('ggpubr')
library(ggpubr) 
if (!require(lubridate)) install.packages('lubridate')
library(lubridate) 

# Download data from the Environmental Data Initiative #===========================
# Dataset DOI: https://doi.org/10.6073/pasta/46c2de115b4a4b16699f5ebc9976ca01
# Package ID: edi.1192.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Summer water chemistry, phytoplankton and zooplankton community composition, size structure, and biomass in a shallow, hypereutrophic reservoir in southwestern Iowa, USA (2019)..
# Data set creator:  Tyler Butts - UW-Madison 
# Data set creator:  Eric Moody - Middlebury 
# Data set creator:  Grace Wilkinson - UW-Madison 
# Data set creator:  Riley Barbour - Iowa State University 
# Contact:  Tyler Butts -  UW-Madison  - tjbutts@wisc.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/1192/1/8f972fe9b8040ffbfb35b0df9928de19" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year",     
                 "doy",     
                 "sampleDepth",     
                 "SRP_ugL",     
                 "flagSRP",     
                 "TP_ugL",     
                 "flagTP",     
                 "TN_mgL",     
                 "flagTN",     
                 "NOx_mgL",     
                 "flagNOx"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$year)=="factor") dt1$year <-as.numeric(levels(dt1$year))[as.integer(dt1$year) ]               
if (class(dt1$year)=="character") dt1$year <-as.numeric(dt1$year)
if (class(dt1$doy)=="factor") dt1$doy <-as.numeric(levels(dt1$doy))[as.integer(dt1$doy) ]               
if (class(dt1$doy)=="character") dt1$doy <-as.numeric(dt1$doy)
if (class(dt1$sampleDepth)=="factor") dt1$sampleDepth <-as.numeric(levels(dt1$sampleDepth))[as.integer(dt1$sampleDepth) ]               
if (class(dt1$sampleDepth)=="character") dt1$sampleDepth <-as.numeric(dt1$sampleDepth)
if (class(dt1$SRP_ugL)=="factor") dt1$SRP_ugL <-as.numeric(levels(dt1$SRP_ugL))[as.integer(dt1$SRP_ugL) ]               
if (class(dt1$SRP_ugL)=="character") dt1$SRP_ugL <-as.numeric(dt1$SRP_ugL)
if (class(dt1$flagSRP)!="factor") dt1$flagSRP<- as.factor(dt1$flagSRP)
if (class(dt1$TP_ugL)=="factor") dt1$TP_ugL <-as.numeric(levels(dt1$TP_ugL))[as.integer(dt1$TP_ugL) ]               
if (class(dt1$TP_ugL)=="character") dt1$TP_ugL <-as.numeric(dt1$TP_ugL)
if (class(dt1$flagTP)!="factor") dt1$flagTP<- as.factor(dt1$flagTP)
if (class(dt1$TN_mgL)=="factor") dt1$TN_mgL <-as.numeric(levels(dt1$TN_mgL))[as.integer(dt1$TN_mgL) ]               
if (class(dt1$TN_mgL)=="character") dt1$TN_mgL <-as.numeric(dt1$TN_mgL)
if (class(dt1$flagTN)!="factor") dt1$flagTN<- as.factor(dt1$flagTN)
if (class(dt1$NOx_mgL)=="factor") dt1$NOx_mgL <-as.numeric(levels(dt1$NOx_mgL))[as.integer(dt1$NOx_mgL) ]               
if (class(dt1$NOx_mgL)=="character") dt1$NOx_mgL <-as.numeric(dt1$NOx_mgL)
if (class(dt1$flagNOx)!="factor") dt1$flagNOx<- as.factor(dt1$flagNOx)

# Convert Missing Values to NA for non-dates

dt1$year <- ifelse((trimws(as.character(dt1$year))==trimws("NA")),NA,dt1$year)               
suppressWarnings(dt1$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$year))==as.character(as.numeric("NA"))),NA,dt1$year))
dt1$doy <- ifelse((trimws(as.character(dt1$doy))==trimws("NA")),NA,dt1$doy)               
suppressWarnings(dt1$doy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$doy))==as.character(as.numeric("NA"))),NA,dt1$doy))
dt1$sampleDepth <- ifelse((trimws(as.character(dt1$sampleDepth))==trimws("NA")),NA,dt1$sampleDepth)               
suppressWarnings(dt1$sampleDepth <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$sampleDepth))==as.character(as.numeric("NA"))),NA,dt1$sampleDepth))
dt1$SRP_ugL <- ifelse((trimws(as.character(dt1$SRP_ugL))==trimws("NA")),NA,dt1$SRP_ugL)               
suppressWarnings(dt1$SRP_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$SRP_ugL))==as.character(as.numeric("NA"))),NA,dt1$SRP_ugL))
dt1$TP_ugL <- ifelse((trimws(as.character(dt1$TP_ugL))==trimws("NA")),NA,dt1$TP_ugL)               
suppressWarnings(dt1$TP_ugL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TP_ugL))==as.character(as.numeric("NA"))),NA,dt1$TP_ugL))
dt1$TN_mgL <- ifelse((trimws(as.character(dt1$TN_mgL))==trimws("NA")),NA,dt1$TN_mgL)               
suppressWarnings(dt1$TN_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$TN_mgL))==as.character(as.numeric("NA"))),NA,dt1$TN_mgL))
dt1$NOx_mgL <- ifelse((trimws(as.character(dt1$NOx_mgL))==trimws("NA")),NA,dt1$NOx_mgL)               
suppressWarnings(dt1$NOx_mgL <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$NOx_mgL))==as.character(as.numeric("NA"))),NA,dt1$NOx_mgL))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year)
summary(doy)
summary(sampleDepth)
summary(SRP_ugL)
summary(flagSRP)
summary(TP_ugL)
summary(flagTP)
summary(TN_mgL)
summary(flagTN)
summary(NOx_mgL)
summary(flagNOx) 
# Get more details on character variables

summary(as.factor(dt1$flagSRP)) 
summary(as.factor(dt1$flagTP)) 
summary(as.factor(dt1$flagTN)) 
summary(as.factor(dt1$flagNOx))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/edi/1192/1/a194f640abd3e8d6918414894b6eaed8" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "doy",     
                 "genus",     
                 "division",     
                 "gald"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$doy)=="factor") dt2$doy <-as.numeric(levels(dt2$doy))[as.integer(dt2$doy) ]               
if (class(dt2$doy)=="character") dt2$doy <-as.numeric(dt2$doy)
if (class(dt2$genus)!="factor") dt2$genus<- as.factor(dt2$genus)
if (class(dt2$division)!="factor") dt2$division<- as.factor(dt2$division)
if (class(dt2$gald)=="factor") dt2$gald <-as.numeric(levels(dt2$gald))[as.integer(dt2$gald) ]               
if (class(dt2$gald)=="character") dt2$gald <-as.numeric(dt2$gald)

# Convert Missing Values to NA for non-dates

dt2$doy <- ifelse((trimws(as.character(dt2$doy))==trimws("NA")),NA,dt2$doy)               
suppressWarnings(dt2$doy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$doy))==as.character(as.numeric("NA"))),NA,dt2$doy))
dt2$gald <- ifelse((trimws(as.character(dt2$gald))==trimws("NA")),NA,dt2$gald)               
suppressWarnings(dt2$gald <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$gald))==as.character(as.numeric("NA"))),NA,dt2$gald))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(doy)
summary(genus)
summary(division)
summary(gald) 
# Get more details on character variables

summary(as.factor(dt2$genus)) 
summary(as.factor(dt2$division))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/edi/1192/1/f47fe3c7c3ec1e58bdf965a65674c1f2" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "doy",     
                 "taxon",     
                 "group",     
                 "length",     
                 "drymass"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$doy)=="factor") dt3$doy <-as.numeric(levels(dt3$doy))[as.integer(dt3$doy) ]               
if (class(dt3$doy)=="character") dt3$doy <-as.numeric(dt3$doy)
if (class(dt3$taxon)!="factor") dt3$taxon<- as.factor(dt3$taxon)
if (class(dt3$group)!="factor") dt3$group<- as.factor(dt3$group)
if (class(dt3$length)=="factor") dt3$length <-as.numeric(levels(dt3$length))[as.integer(dt3$length) ]               
if (class(dt3$length)=="character") dt3$length <-as.numeric(dt3$length)
if (class(dt3$drymass)=="factor") dt3$drymass <-as.numeric(levels(dt3$drymass))[as.integer(dt3$drymass) ]               
if (class(dt3$drymass)=="character") dt3$drymass <-as.numeric(dt3$drymass)

# Convert Missing Values to NA for non-dates

dt3$doy <- ifelse((trimws(as.character(dt3$doy))==trimws("NA")),NA,dt3$doy)               
suppressWarnings(dt3$doy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$doy))==as.character(as.numeric("NA"))),NA,dt3$doy))
dt3$length <- ifelse((trimws(as.character(dt3$length))==trimws("NA")),NA,dt3$length)               
suppressWarnings(dt3$length <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$length))==as.character(as.numeric("NA"))),NA,dt3$length))
dt3$drymass <- ifelse((trimws(as.character(dt3$drymass))==trimws("NA")),NA,dt3$drymass)               
suppressWarnings(dt3$drymass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt3$drymass))==as.character(as.numeric("NA"))),NA,dt3$drymass))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(doy)
summary(taxon)
summary(group)
summary(length)
summary(drymass) 
# Get more details on character variables

summary(as.factor(dt3$taxon)) 
summary(as.factor(dt3$group))
detach(dt3)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/edi/1192/1/934bc72b296b584ba88908931d1ecfea" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "year",     
                 "doyfrac",     
                 "doy",     
                 "timefrac",     
                 "chl_rfu",     
                 "chl",     
                 "cond",     
                 "odo_sat",     
                 "odo_satlocal",     
                 "odo",     
                 "salinity",     
                 "spcond",     
                 "pc_rfu",     
                 "pc",     
                 "tds",     
                 "ph",     
                 "ph_mv",     
                 "temp"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$year)=="factor") dt4$year <-as.numeric(levels(dt4$year))[as.integer(dt4$year) ]               
if (class(dt4$year)=="character") dt4$year <-as.numeric(dt4$year)
if (class(dt4$doyfrac)=="factor") dt4$doyfrac <-as.numeric(levels(dt4$doyfrac))[as.integer(dt4$doyfrac) ]               
if (class(dt4$doyfrac)=="character") dt4$doyfrac <-as.numeric(dt4$doyfrac)
if (class(dt4$doy)=="factor") dt4$doy <-as.numeric(levels(dt4$doy))[as.integer(dt4$doy) ]               
if (class(dt4$doy)=="character") dt4$doy <-as.numeric(dt4$doy)
if (class(dt4$timefrac)=="factor") dt4$timefrac <-as.numeric(levels(dt4$timefrac))[as.integer(dt4$timefrac) ]               
if (class(dt4$timefrac)=="character") dt4$timefrac <-as.numeric(dt4$timefrac)
if (class(dt4$chl_rfu)=="factor") dt4$chl_rfu <-as.numeric(levels(dt4$chl_rfu))[as.integer(dt4$chl_rfu) ]               
if (class(dt4$chl_rfu)=="character") dt4$chl_rfu <-as.numeric(dt4$chl_rfu)
if (class(dt4$chl)=="factor") dt4$chl <-as.numeric(levels(dt4$chl))[as.integer(dt4$chl) ]               
if (class(dt4$chl)=="character") dt4$chl <-as.numeric(dt4$chl)
if (class(dt4$cond)=="factor") dt4$cond <-as.numeric(levels(dt4$cond))[as.integer(dt4$cond) ]               
if (class(dt4$cond)=="character") dt4$cond <-as.numeric(dt4$cond)
if (class(dt4$odo_sat)=="factor") dt4$odo_sat <-as.numeric(levels(dt4$odo_sat))[as.integer(dt4$odo_sat) ]               
if (class(dt4$odo_sat)=="character") dt4$odo_sat <-as.numeric(dt4$odo_sat)
if (class(dt4$odo_satlocal)=="factor") dt4$odo_satlocal <-as.numeric(levels(dt4$odo_satlocal))[as.integer(dt4$odo_satlocal) ]               
if (class(dt4$odo_satlocal)=="character") dt4$odo_satlocal <-as.numeric(dt4$odo_satlocal)
if (class(dt4$odo)=="factor") dt4$odo <-as.numeric(levels(dt4$odo))[as.integer(dt4$odo) ]               
if (class(dt4$odo)=="character") dt4$odo <-as.numeric(dt4$odo)
if (class(dt4$salinity)=="factor") dt4$salinity <-as.numeric(levels(dt4$salinity))[as.integer(dt4$salinity) ]               
if (class(dt4$salinity)=="character") dt4$salinity <-as.numeric(dt4$salinity)
if (class(dt4$spcond)=="factor") dt4$spcond <-as.numeric(levels(dt4$spcond))[as.integer(dt4$spcond) ]               
if (class(dt4$spcond)=="character") dt4$spcond <-as.numeric(dt4$spcond)
if (class(dt4$pc_rfu)=="factor") dt4$pc_rfu <-as.numeric(levels(dt4$pc_rfu))[as.integer(dt4$pc_rfu) ]               
if (class(dt4$pc_rfu)=="character") dt4$pc_rfu <-as.numeric(dt4$pc_rfu)
if (class(dt4$pc)=="factor") dt4$pc <-as.numeric(levels(dt4$pc))[as.integer(dt4$pc) ]               
if (class(dt4$pc)=="character") dt4$pc <-as.numeric(dt4$pc)
if (class(dt4$tds)=="factor") dt4$tds <-as.numeric(levels(dt4$tds))[as.integer(dt4$tds) ]               
if (class(dt4$tds)=="character") dt4$tds <-as.numeric(dt4$tds)
if (class(dt4$ph)=="factor") dt4$ph <-as.numeric(levels(dt4$ph))[as.integer(dt4$ph) ]               
if (class(dt4$ph)=="character") dt4$ph <-as.numeric(dt4$ph)
if (class(dt4$ph_mv)=="factor") dt4$ph_mv <-as.numeric(levels(dt4$ph_mv))[as.integer(dt4$ph_mv) ]               
if (class(dt4$ph_mv)=="character") dt4$ph_mv <-as.numeric(dt4$ph_mv)
if (class(dt4$temp)=="factor") dt4$temp <-as.numeric(levels(dt4$temp))[as.integer(dt4$temp) ]               
if (class(dt4$temp)=="character") dt4$temp <-as.numeric(dt4$temp)

# Convert Missing Values to NA for non-dates

dt4$year <- ifelse((trimws(as.character(dt4$year))==trimws("NA")),NA,dt4$year)               
suppressWarnings(dt4$year <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$year))==as.character(as.numeric("NA"))),NA,dt4$year))
dt4$doyfrac <- ifelse((trimws(as.character(dt4$doyfrac))==trimws("NA")),NA,dt4$doyfrac)               
suppressWarnings(dt4$doyfrac <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$doyfrac))==as.character(as.numeric("NA"))),NA,dt4$doyfrac))
dt4$doy <- ifelse((trimws(as.character(dt4$doy))==trimws("NA")),NA,dt4$doy)               
suppressWarnings(dt4$doy <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$doy))==as.character(as.numeric("NA"))),NA,dt4$doy))
dt4$timefrac <- ifelse((trimws(as.character(dt4$timefrac))==trimws("NA")),NA,dt4$timefrac)               
suppressWarnings(dt4$timefrac <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$timefrac))==as.character(as.numeric("NA"))),NA,dt4$timefrac))
dt4$chl_rfu <- ifelse((trimws(as.character(dt4$chl_rfu))==trimws("NA")),NA,dt4$chl_rfu)               
suppressWarnings(dt4$chl_rfu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$chl_rfu))==as.character(as.numeric("NA"))),NA,dt4$chl_rfu))
dt4$chl <- ifelse((trimws(as.character(dt4$chl))==trimws("NA")),NA,dt4$chl)               
suppressWarnings(dt4$chl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$chl))==as.character(as.numeric("NA"))),NA,dt4$chl))
dt4$cond <- ifelse((trimws(as.character(dt4$cond))==trimws("NA")),NA,dt4$cond)               
suppressWarnings(dt4$cond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$cond))==as.character(as.numeric("NA"))),NA,dt4$cond))
dt4$odo_sat <- ifelse((trimws(as.character(dt4$odo_sat))==trimws("NA")),NA,dt4$odo_sat)               
suppressWarnings(dt4$odo_sat <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$odo_sat))==as.character(as.numeric("NA"))),NA,dt4$odo_sat))
dt4$odo_satlocal <- ifelse((trimws(as.character(dt4$odo_satlocal))==trimws("NA")),NA,dt4$odo_satlocal)               
suppressWarnings(dt4$odo_satlocal <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$odo_satlocal))==as.character(as.numeric("NA"))),NA,dt4$odo_satlocal))
dt4$odo <- ifelse((trimws(as.character(dt4$odo))==trimws("NA")),NA,dt4$odo)               
suppressWarnings(dt4$odo <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$odo))==as.character(as.numeric("NA"))),NA,dt4$odo))
dt4$salinity <- ifelse((trimws(as.character(dt4$salinity))==trimws("NA")),NA,dt4$salinity)               
suppressWarnings(dt4$salinity <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$salinity))==as.character(as.numeric("NA"))),NA,dt4$salinity))
dt4$spcond <- ifelse((trimws(as.character(dt4$spcond))==trimws("NA")),NA,dt4$spcond)               
suppressWarnings(dt4$spcond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$spcond))==as.character(as.numeric("NA"))),NA,dt4$spcond))
dt4$pc_rfu <- ifelse((trimws(as.character(dt4$pc_rfu))==trimws("NA")),NA,dt4$pc_rfu)               
suppressWarnings(dt4$pc_rfu <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$pc_rfu))==as.character(as.numeric("NA"))),NA,dt4$pc_rfu))
dt4$pc <- ifelse((trimws(as.character(dt4$pc))==trimws("NA")),NA,dt4$pc)               
suppressWarnings(dt4$pc <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$pc))==as.character(as.numeric("NA"))),NA,dt4$pc))
dt4$tds <- ifelse((trimws(as.character(dt4$tds))==trimws("NA")),NA,dt4$tds)               
suppressWarnings(dt4$tds <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$tds))==as.character(as.numeric("NA"))),NA,dt4$tds))
dt4$ph <- ifelse((trimws(as.character(dt4$ph))==trimws("NA")),NA,dt4$ph)               
suppressWarnings(dt4$ph <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$ph))==as.character(as.numeric("NA"))),NA,dt4$ph))
dt4$ph_mv <- ifelse((trimws(as.character(dt4$ph_mv))==trimws("NA")),NA,dt4$ph_mv)               
suppressWarnings(dt4$ph_mv <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$ph_mv))==as.character(as.numeric("NA"))),NA,dt4$ph_mv))
dt4$temp <- ifelse((trimws(as.character(dt4$temp))==trimws("NA")),NA,dt4$temp)               
suppressWarnings(dt4$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt4$temp))==as.character(as.numeric("NA"))),NA,dt4$temp))


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(year)
summary(doyfrac)
summary(doy)
summary(timefrac)
summary(chl_rfu)
summary(chl)
summary(cond)
summary(odo_sat)
summary(odo_satlocal)
summary(odo)
summary(salinity)
summary(spcond)
summary(pc_rfu)
summary(pc)
summary(tds)
summary(ph)
summary(ph_mv)
summary(temp) 
# Get more details on character variables

detach(dt4)               


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/edi/1192/1/bec6d14dc525c033b8a4a61c140569ed" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SAMPLE.ID",     
                 "LAKE.NO",     
                 "DIVISION",     
                 "TAXON",     
                 "TAXON_GROUPING",     
                 "BIOMASS.MG.L",     
                 "DOY"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$SAMPLE.ID)!="factor") dt5$SAMPLE.ID<- as.factor(dt5$SAMPLE.ID)
if (class(dt5$LAKE.NO)!="factor") dt5$LAKE.NO<- as.factor(dt5$LAKE.NO)
if (class(dt5$DIVISION)!="factor") dt5$DIVISION<- as.factor(dt5$DIVISION)
if (class(dt5$TAXON)!="factor") dt5$TAXON<- as.factor(dt5$TAXON)
if (class(dt5$TAXON_GROUPING)!="factor") dt5$TAXON_GROUPING<- as.factor(dt5$TAXON_GROUPING)
if (class(dt5$BIOMASS.MG.L)=="factor") dt5$BIOMASS.MG.L <-as.numeric(levels(dt5$BIOMASS.MG.L))[as.integer(dt5$BIOMASS.MG.L) ]               
if (class(dt5$BIOMASS.MG.L)=="character") dt5$BIOMASS.MG.L <-as.numeric(dt5$BIOMASS.MG.L)
if (class(dt5$DOY)=="factor") dt5$DOY <-as.numeric(levels(dt5$DOY))[as.integer(dt5$DOY) ]               
if (class(dt5$DOY)=="character") dt5$DOY <-as.numeric(dt5$DOY)

# Convert Missing Values to NA for non-dates

dt5$BIOMASS.MG.L <- ifelse((trimws(as.character(dt5$BIOMASS.MG.L))==trimws("NA")),NA,dt5$BIOMASS.MG.L)               
suppressWarnings(dt5$BIOMASS.MG.L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$BIOMASS.MG.L))==as.character(as.numeric("NA"))),NA,dt5$BIOMASS.MG.L))
dt5$DOY <- ifelse((trimws(as.character(dt5$DOY))==trimws("NA")),NA,dt5$DOY)               
suppressWarnings(dt5$DOY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt5$DOY))==as.character(as.numeric("NA"))),NA,dt5$DOY))


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SAMPLE.ID)
summary(LAKE.NO)
summary(DIVISION)
summary(TAXON)
summary(TAXON_GROUPING)
summary(BIOMASS.MG.L)
summary(DOY) 
# Get more details on character variables

summary(as.factor(dt5$SAMPLE.ID)) 
summary(as.factor(dt5$LAKE.NO)) 
summary(as.factor(dt5$DIVISION)) 
summary(as.factor(dt5$TAXON)) 
summary(as.factor(dt5$TAXON_GROUPING))
detach(dt5)               


inUrl6  <- "https://pasta.lternet.edu/package/data/eml/edi/1192/1/890facf08cc658b9d9a4c1c0e070ac21" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl"))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")


dt6 <-read.csv(infile6,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SAMPLE.ID",     
                 "LAKE.NO",     
                 "DOY",     
                 "TAXON",     
                 "BIOMASS.UG.L",     
                 "INDV.L",     
                 "BIOMASS.UG",     
                 "GROUP"    ), check.names=TRUE)

unlink(infile6)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt6$SAMPLE.ID)!="factor") dt6$SAMPLE.ID<- as.factor(dt6$SAMPLE.ID)
if (class(dt6$LAKE.NO)!="factor") dt6$LAKE.NO<- as.factor(dt6$LAKE.NO)
if (class(dt6$DOY)=="factor") dt6$DOY <-as.numeric(levels(dt6$DOY))[as.integer(dt6$DOY) ]               
if (class(dt6$DOY)=="character") dt6$DOY <-as.numeric(dt6$DOY)
if (class(dt6$TAXON)!="factor") dt6$TAXON<- as.factor(dt6$TAXON)
if (class(dt6$BIOMASS.UG.L)=="factor") dt6$BIOMASS.UG.L <-as.numeric(levels(dt6$BIOMASS.UG.L))[as.integer(dt6$BIOMASS.UG.L) ]               
if (class(dt6$BIOMASS.UG.L)=="character") dt6$BIOMASS.UG.L <-as.numeric(dt6$BIOMASS.UG.L)
if (class(dt6$INDV.L)=="factor") dt6$INDV.L <-as.numeric(levels(dt6$INDV.L))[as.integer(dt6$INDV.L) ]               
if (class(dt6$INDV.L)=="character") dt6$INDV.L <-as.numeric(dt6$INDV.L)
if (class(dt6$BIOMASS.UG)=="factor") dt6$BIOMASS.UG <-as.numeric(levels(dt6$BIOMASS.UG))[as.integer(dt6$BIOMASS.UG) ]               
if (class(dt6$BIOMASS.UG)=="character") dt6$BIOMASS.UG <-as.numeric(dt6$BIOMASS.UG)
if (class(dt6$GROUP)!="factor") dt6$GROUP<- as.factor(dt6$GROUP)

# Convert Missing Values to NA for non-dates

dt6$DOY <- ifelse((trimws(as.character(dt6$DOY))==trimws("NA")),NA,dt6$DOY)               
suppressWarnings(dt6$DOY <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$DOY))==as.character(as.numeric("NA"))),NA,dt6$DOY))
dt6$BIOMASS.UG.L <- ifelse((trimws(as.character(dt6$BIOMASS.UG.L))==trimws("NA")),NA,dt6$BIOMASS.UG.L)               
suppressWarnings(dt6$BIOMASS.UG.L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$BIOMASS.UG.L))==as.character(as.numeric("NA"))),NA,dt6$BIOMASS.UG.L))
dt6$INDV.L <- ifelse((trimws(as.character(dt6$INDV.L))==trimws("NA")),NA,dt6$INDV.L)               
suppressWarnings(dt6$INDV.L <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$INDV.L))==as.character(as.numeric("NA"))),NA,dt6$INDV.L))
dt6$BIOMASS.UG <- ifelse((trimws(as.character(dt6$BIOMASS.UG))==trimws("NA")),NA,dt6$BIOMASS.UG)               
suppressWarnings(dt6$BIOMASS.UG <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt6$BIOMASS.UG))==as.character(as.numeric("NA"))),NA,dt6$BIOMASS.UG))


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SAMPLE.ID)
summary(LAKE.NO)
summary(DOY)
summary(TAXON)
summary(BIOMASS.UG.L)
summary(INDV.L)
summary(BIOMASS.UG)
summary(GROUP) 
# Get more details on character variables

summary(as.factor(dt6$SAMPLE.ID)) 
summary(as.factor(dt6$LAKE.NO)) 
summary(as.factor(dt6$TAXON)) 
summary(as.factor(dt6$GROUP))
detach(dt6)               

# Clean Data Sets Plus add literature collated data from github #==================
# https://github.com/tjbutts/hyper-plankton 

#===============# Zooplankton Data #===================#
#Zooplankton Biomass (ug/L), Average Dry Mass (ug), and Density (#/L)
zp_dat = as_tibble(dt6) # Need to tidy data
zp_dat
zp_raw = zp_dat %>%
  rename(sampleid = SAMPLE.ID) %>% # Rename columns for convenience 
  rename(taxon = TAXON) %>%
  rename(lakeno = LAKE.NO) %>%
  rename(biomass = BIOMASS.UG.L) %>%
  rename(drymass = BIOMASS.UG) %>%
  rename(density = INDV.L) %>%
  rename(group = GROUP) %>% 
  rename(doy = DOY) %>%
  mutate(biomass = replace_na(biomass,0), # Replace NAs with 0
         drymass = replace_na(drymass,0),
         density = replace_na(density,0)) %>%
  arrange(doy)
zp_raw$group = as.factor(zp_raw$group) # makes the group column a factor, easier for later analysis 

#===============# Phytoplankton data # ======================#
# Phytoplankton Biomass Data 
phy_biomass = as_tibble(dt5)

#===============# Surface water nutrient concentrations #================#
gv_nutrients = as_tibble(dt1)

#===============# High frequency EXO data #====================#
gv_exo_hf =  as_tibble(dt4)

#================# Zooplankton-phytoplankton size data #=====================#
## phytoplankton GALD 
gv_gald = as_tibble(dt2)
## Zooplankton length and body mass 
gv_length_mass = dt3 

# Data collated from the literature ##===========================
# Describe literature sources
# Zooplankton Stoichiometry Data # 
## Stoichiometric data of Zooplankton was collected from the literature using the same methods as Moody & Wilkinson 2019, 
## Data from: Hamre, 2016; Hébert et al. 2016; Hessen et al. 2007 
    ##Hamre, K. (2016) Nutrient profiles of rotifers (Brachionus sp.) and rotifer diets from four different marine fish hatcheries. Aquaculture, 450, 136–142.
    ##Hébert, M. P. et al. (2016a) A compilation of quantitative functional traits for marine and freshwater crustacean zooplankton. Ecology, 97, 1081.
    ##Hessen, D. O. et al. (2007) RNA responses to N- and P-limitation; reciprocal regulation of stoichiometry and growth rate in Brachionus. Funct. Ecol., 21, 956–962.
zp_stoich = read_csv('https://raw.githubusercontent.com/tjbutts/hyper-plankton/main/2019_zoop_cnpratios.csv') 

# Zooplankton Feeding Size Range 
## Feeding size range of zooplankton was collected from the literature ## 
## Data from: Barnett et al. 2007, Helenius and Saiz 2017, Sweeney et al. 2022 
  ##Barnett, A. J. et al. (2007) Functional diversity of crustacean zooplankton communities: towards a trait-based classification. Freshw. Biol., 52, 796–813.
  ##Helenius, L. K. and Saiz, E. (2017) Feeding behaviour of the nauplii of the marine calanoid copepod Paracartia grani Sars: Functional response, prey size spectrum, and effects of the presence of alternative prey. PLoS One, 12, 1–20.
  ##Sweeney, K. et al. (2022) Grazing impacts of rotifer zooplankton on a cyanobacteria bloom in a shallow temperate lake (Vancouver Lake, WA, USA). Hydrobiologia.
gv_foodsize = read_csv('https://raw.githubusercontent.com/tjbutts/hyper-plankton/main/2019_zoop_feedingrange.csv')

# Supplemental #==========================

# Historical GVL data from the Ambient Lakes Monitoring Program # 
## These data sets were taken from the AQuIA database housing water quality and biological data on Iowa public lakes. These data are specific to Green Valley Lake, the study lake for this analysis.  
## AQuIA Database: https://programs.iowadnr.gov/aquia/ 
alm_hist = read_csv('https://raw.githubusercontent.com/tjbutts/hyper-plankton/main/gv_alm_historical.csv')
## Historical zooplankton and phytoplankton data from the Iowa State Ambient Lake Monitoring Program collected by the Iowa State Limnology Lab 
## For more information about these data contact: tjbutts@wisc.edu 
zoop_hist = read_csv('https://raw.githubusercontent.com/tjbutts/hyper-plankton/main/zp_historical.csv')
phyto_hist = read_csv('https://raw.githubusercontent.com/tjbutts/hyper-plankton/main/gv_plankton_historical.csv')
