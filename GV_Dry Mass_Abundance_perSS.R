# Calculate GV Dry Mass & GV Abundance per subsample # 
# Purpose of this script is to generate the data objects: gv19_DM and gv19_COUNT 
# Data objects are then used in the GV_Excretion Rates script.R 
rm(list=ls())

# DRY MASS ============================================
# Individual Zooplankton Dry Mass 
# Purpose: this script is designed to convert the length and count data for zooplankton samples in to biomass data, based on the allometric equations provided by the Downing Lab
# Purpose continued: Specifically, the dry biomass (ug) per individual to be implemented in regression equations for excretion
# Purpose continued: rate equations (nmol N or P per indvidual per hour)
# Script originally developed by Dr. Eric Moody - June 2017
# Updated by Dr. Grace Wilkinson - Feb 2021
# Modified by Tyler Butts - Apr 2021


# STEP 1: Place the files in a working directory
# Place all of the count files into a folder, no other files should be in this folder
# DO NOT put the sample log into this folder, only count files
# Make sure the files are saved as .csv
# Make sure the file names are consistent (they SHOULD just be the 12 character sample ID)
# Set the working directory in R to this folder
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton/Site 4 ZP Data")


# STEP 2: Set up empty vectors that will be filled using a for loop
filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
DOY <- c()
TAXON <- c()
BIOMASS.UG <- c()

# STEP 3: Use a for loop to fill the vectors above based on the information in the count files
# Note that this is set up to work with the exact column headers and row order for the
# ZooCountTemplate file used in the lab - another format will not work with this script

############ NOTE: RUN THE ENTIRE FOR-LOOP AT ONCE, LINES 27-296

i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 46 taxa in this list 
  # (TB added Ostracods as they were present in Green Valley Lake Samples) 
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichocerca","Trichotria", "Ostracod")
  
  #Calculate the biomass in micrograms per liter by converting the length and counts
  #for each taxa using the allometric equations below
  #Note that the equations are specific to each taxa, and reference the indexed list of 
  #taxon above (n=1-45)
  
  z<-c()
  y<-c()
  x<-c()
  BiomassSubsample<-c()
  
  # Need to get the average ug weighted by the abundance 
  
  #Alona 
  BiomassSubsample[1]<-mean(as.numeric(
    (15.92*(file[1,5:length(names(file))]/1000)^3.84)),na.rm=T)
  
  
  #Alonella
  BiomassSubsample[2]<-mean(as.numeric(
    (15.92*(file[2,5:length(names(file))]/1000)^3.84)),na.rm=T)
  
  #Bosmina
  BiomassSubsample[3]<-mean(as.numeric(
    (26.6*(file[3,5:length(names(file))]/1000)^3.13)),na.rm=T)
  
  #Camptocercus
  BiomassSubsample[4]<-mean(as.numeric(
    (15.92*(file[4,5:length(names(file))]/1000)^3.84)),na.rm=T)
  
  #Ceriodaphnia
  BiomassSubsample[5]<-mean(as.numeric(
    (1.76*10^-6)*(file[5,5:length(names(file))]^2.26)),na.rm=T)
  
  #Chydorus
  BiomassSubsample[6]<-mean(as.numeric(
    (89.43*(file[6,5:length(names(file))]/1000)^3.03)),na.rm=T)
  
  #Daphnia
  BiomassSubsample[7]<-mean(as.numeric(
    (1.5*10^-8)*(file[7,5:length(names(file))]^2.84)),na.rm=T)
  
  #Daphnia.lumholtzi
  BiomassSubsample[8]<-mean(as.numeric(
    (1.5*10^-8)*(file[8,5:length(names(file))]^2.84)),na.rm=T)
  
  #Diaphanosoma
  BiomassSubsample[9]<-mean(as.numeric(
    (1.76*10^-6)*(file[9,5:length(names(file))]^2.11)),na.rm=T)
  
  #Graptoleberis
  BiomassSubsample[10]<-mean(as.numeric(
    (15.92*(file[10,5:length(names(file))]/1000)^3.84)),na.rm=T)
  
  #Leptodora
  BiomassSubsample[11]<-mean(as.numeric(
    (-0.822+2.76*log(file[11,5:length(names(file))]/1000))),na.rm=T)
  
  #Moina
  BiomassSubsample[12]<-mean(as.numeric(
    (6.61*(file[12,5:length(names(file))]/1000)^2.57)),na.rm=T)
  
  #Pleuroxus
  BiomassSubsample[13]<-mean(as.numeric(
    (35.6*(file[13,5:length(names(file))]/1000)^4.03)),na.rm=T)
  
  #Scapholeberis
  BiomassSubsample[14]<-mean(as.numeric(
    (8.9*10^-8)*(file[14,5:length(names(file))]^2.7)),na.rm=T)
  
  #Simocephalus
  BiomassSubsample[15]<-mean(as.numeric(
    (7.43*(file[15,5:length(names(file))]/1000)^3.28)),na.rm=T)
  
  #Calanoida
  BiomassSubsample[16]<-mean(as.numeric(
    (7.9*10^-7)*(file[16,5:length(names(file))]^2.33)),na.rm=T)
  
  #Cyclopoida
  BiomassSubsample[17]<-mean(as.numeric(
    (1.1*10^-7)*(file[17,5:length(names(file))]^2.59)),na.rm=T)
  
  #Nauplii
  BiomassSubsample[18]<-mean(as.numeric(
    (1.1*10^-5)*(file[18,5:length(names(file))]^1.89)),na.rm=T)
  
  # ROTIFERS
  
  #Anuraeopsis
  BiomassSubsample[19]<-((0.1*(mean(as.numeric(
    (0.03*(file[19,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Ascomorpha
  BiomassSubsample[20]<-((0.1*(mean(as.numeric(
    (0.12*(file[20,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Asplanchna
  BiomassSubsample[21]<-((0.039*(mean(as.numeric(
    (0.23*(file[21,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Asplanchnopus
  BiomassSubsample[22]<-((0.039*(mean(as.numeric(
    (0.23*(file[22,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Brachionus
  BiomassSubsample[23]<-((0.1*(mean(as.numeric(
    (0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[23,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  BiomassSubsample[24]<-(sum((0.16*file[24,5:length(names(file))]/1000*
                                (file[25,5:length(names(file))]/1000)^2)/1000,na.rm=T))*10^6
  
  #Euchlanis
  BiomassSubsample[25]<-((0.1*(mean(as.numeric(
    (0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[26,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Filinia
  BiomassSubsample[26]<-((0.1*(mean(as.numeric(
    (0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.01*(mean(as.numeric((0.13*(file[27,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Gastropus
  BiomassSubsample[27]<-((0.1*(mean(as.numeric(
    (0.2*(file[28,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Hexarthra
  BiomassSubsample[28]<-((0.1*(mean(as.numeric(
    (0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.33*(mean(as.numeric((0.13*(file[29,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Kellicotia
  BiomassSubsample[29]<-((0.1*(mean(as.numeric(
    (0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+ 
      0.015*(mean(as.numeric((0.03*(file[30,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Keratella.cochlearis
  BiomassSubsample[30]<-((0.1*(mean(as.numeric(
    (0.02*(file[31,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Keratella.quadrata
  BiomassSubsample[31]<-((0.1*(mean(as.numeric(
    (0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.22*(file[32,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Lecane
  BiomassSubsample[32]<-((0.1*(mean(as.numeric(
    (0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.1*(mean(as.numeric((0.12*(file[33,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Lepadella
  BiomassSubsample[33]<-((0.1*(mean(as.numeric(
    ((file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.05*(mean(as.numeric((0.1*(file[34,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Macrochaetus
  BiomassSubsample[34]<-((0.1*(mean(as.numeric(
    (0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[35,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Monostyla
  BiomassSubsample[35]<-((0.1*(mean(as.numeric(
    (0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.12*(file[36,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Mytilina
  a<-as.numeric(file[37,5:length(names(file))])/1000
  b<-as.numeric(file[38,5:length(names(file))])/1000
  c<-((0.52*a*b^2)+(0.6*0.52*a*b^2))/1000
  BiomassSubsample[36]<-(mean(as.numeric(c),na.rm=T)*10^6)
  
  #Notholca
  BiomassSubsample[37]<-((0.1*(mean(as.numeric(
    (0.035*(file[39,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Platyias
  BiomassSubsample[38]<-(((mean(as.numeric(
    (0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +(mean(as.numeric((0.12*(file[40,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Ploesoma
  BiomassSubsample[39]<-((0.1*(mean(as.numeric(
    (0.23*(file[41,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Polyarthra
  BiomassSubsample[40]<-((0.1*(mean(as.numeric(
    (0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.28*(file[42,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Pompholyx
  BiomassSubsample[41]<-((0.1*(mean(as.numeric(
    (0.15*(file[43,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Synchaeta
  BiomassSubsample[42]<-((0.1*(mean(as.numeric(
    (0.1*(file[44,5:length(names(file))]/1000)^3)),na.rm=T)/1000))*10^6)
  
  #Testudinella
  BiomassSubsample[43]<-((0.1*(mean(as.numeric(
    (0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000)
    +0.1*(mean(as.numeric((0.08*(file[45,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  #Trichocerca
  x<-as.numeric(file[46,5:length(names(file))])/1000
  y<-as.numeric(file[47,5:length(names(file))])/1000
  z<-((0.52*x*y^2)+(0.6*0.52*x*y^2))/1000
  BiomassSubsample[44]<-sum(z,na.rm=T)*10^6
  
  #Trichotria
  BiomassSubsample[45]<-(((mean(as.numeric(
    (0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000)+
      0.05*(mean(as.numeric((0.1*(file[48,5:length(names(file))]/1000)^3)),na.rm=T)/1000))
    *10^6)
  
  # OSTRACODS
  #Ostracoda
  BiomassSubsample[46]<-mean(as.numeric(
    (0.81058*(file[49,5:length(names(file))]/1000)^2.4755)),na.rm=T) # Ostracod regression: Jimenez et al. 2011, JGLR
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
# SAMPLE IDs 
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,12),46)) #46 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),46)) #46 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),46)) #pond number is the 4-6th spot
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),46)) #DOY is the 7-9th spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  BIOMASS.UG <- append(BIOMASS.UG, BiomassSubsample)
  
}

# STEP 4: MAKE A USEFUL OUTPUT OF THE AVERAGE DRY MASS PER TAXA ===========================

# Start by making a data frame of the useful info from above
Zoop.gv<-data.frame(SAMPLE.ID, LAKE.NO, DOY, TAXON, BIOMASS.UG)

# Use the tidyverse to add more columns
library(tidyverse)
Zoop.gv.dry = Zoop.gv %>%
  # Create a new column ("GROUP") that creates the broader taxonomic groups for analysis
  mutate(GROUP = case_when(.$TAXON %in% c("Alona",
                                          "Alonella",
                                          "Bosmina",
                                          "Chydorus",
                                          "Pleuroxus") ~ "SmCladocera",
                           .$TAXON %in% c("Camptocercus",
                                          "Ceriodaphnia",
                                          "Daphnia",
                                          "Daphnia.lumholtzi",
                                          "Diaphanosoma",
                                          "Graptoleberis",
                                          "Leptodora",
                                          "Moina",
                                          "Scapholeberis",
                                          "Simocephalus") ~ "LgCladocera",
                           .$TAXON %in% c("Anuraeopsis",
                                          "Ascomorpha",
                                          "Asplanchna",
                                          "Asplanchnopus",
                                          "Brachionus",
                                          "Conochilus",
                                          "Euchlanis",
                                          "Filinia",
                                          "Gastropus",
                                          "Hexarthra",
                                          "Kellicottia",
                                          "Keratella.cochlearis",
                                          "Keratella.quadrata",
                                          "Lecane", 
                                          "Lepadella",
                                          "Macrochaetus",
                                          "Monostyla",
                                          "Mytilina",
                                          "Notholca",
                                          "Platyias",
                                          "Ploesoma",
                                          "Polyarthra",
                                          "Pompholyx",
                                          "Synchaeta",
                                          "Testudinella",
                                          "Trichocerca",
                                          "Trichotria") ~ "Rotifer",
                           .$TAXON %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$TAXON %in% c("Calanoida") ~ "Calanoid",
                           .$TAXON %in% c("Nauplii") ~ "Nauplii", 
                           .$TAXON %in% c("Ostracod") ~ "Ostracod"))


# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
Zoop.gv.dry
#Set working directory to data file 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
#write.csv(Zoop.gv.dry,"2019_site4_gv_Zoopdry_19Apr2021.csv", row.names = F)

# Turn raw R output into cleaned zoop data
zp_raw = read.csv('2019_site4_gv_Zoopdry_19Apr2021.csv')
zp_raw2 = zp_raw %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID,
         taxon = TAXON,
         lakeno = LAKE.NO,
         dry_biomass = BIOMASS.UG,
         group = GROUP,
         doy = DOY)
zp_raw2$group <- as.factor(zp_raw2$group) # makes the group column a factor, easier for later analysis 

gv19_DM = zp_raw2 %>% 
  select(sampleid,doy, taxon, group, dry_biomass) %>% 
  mutate(dry_biomass = replace_na(dry_biomass, 0)) %>% # Replace NAs with 0s 
  filter(!(doy == 162 | doy == 157)) %>% # Remove the ALM sampling dates plus DOY 157 which we determined was wonky
  as_tibble()
gv19_DM

# Count data ============================================
# STEP 5: COUNT - collect count of organisms to get a weighted mean to plug into the excretion rate equations 
# reset working directory to where files are present 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton/Site 4 ZP Data")

filenames <- list.files(path=getwd())  
numfiles <- length(filenames)

SAMPLE.ID <- c()
LAKE.NO <- c()
DOY <- c()
TAXON <- c()
COUNT <- c()

i<-1
for (i in c(1:numfiles)){  
  filenames[i] <- paste(filenames[i],sep="")  
  file<- read.csv(filenames[i])
  
  #This is the list of taxa that we identify and the order that they are in for the rows
  #in the ZooCountTemplate; there are 46 taxa in this list 
  # (TB added Ostracods as they were present in Green Valley Lake Samples) 
  Taxon<-c("Alona","Alonella","Bosmina","Camptocercus","Ceriodaphnia","Chydorus",
           "Daphnia","Daphnia.lumholtzi","Diaphanosoma","Graptoleberis","Leptodora",
           "Moina","Pleuroxus","Scapholeberis","Simocephalus","Calanoida","Cyclopoida",
           "Nauplii","Anuraeopsis","Ascomorpha","Asplanchna","Asplanchnopus",
           "Brachionus","Conochilus","Euchlanis","Filinia","Gastropus","Hexarthra",
           "Kellicottia","Keratella.cochlearis","Keratella.quadrata","Lecane",
           "Lepadella","Macrochaetus","Monostyla","Mytilina","Notholca","Platyias",
           "Ploesoma","Polyarthra","Pompholyx","Synchaeta","Testudinella",
           "Trichocerca","Trichotria", "Ostracod")
  
  #Calculate the biomass in micrograms per liter by converting the length and counts
  #for each taxa using the allometric equations below
  #Note that the equations are specific to each taxa, and reference the indexed list of 
  #taxon above (n=1-45)
  
  z<-c()
  y<-c()
  x<-c()
  CountSubsample<-c()
  
  #Alona 
  CountSubsample[1]<-file$Count[1]
  
  #Alonella
  CountSubsample[2]<-file$Count[2]
  
  #Bosmina
  CountSubsample[3]<-file$Count[3]
  
  #Camptocercus
  CountSubsample[4]<-file$Count[4]
  
  #Ceriodaphnia
  CountSubsample[5]<-file$Count[5]
  
  #Chydorus
  CountSubsample[6]<-file$Count[6]
  
  #Daphnia
  CountSubsample[7]<-file$Count[7]
  
  #Daphnia.lumholtzi
  CountSubsample[8]<-file$Count[8]
  
  #Diaphanosoma
  CountSubsample[9]<-file$Count[9]
  
  #Graptoleberis
  CountSubsample[10]<-file$Count[10]
  
  #Leptodora
  CountSubsample[11]<-file$Count[11]
  
  #Moina
  CountSubsample[12]<-file$Count[12]
  
  #Pleuroxus
  CountSubsample[13]<-file$Count[13]
  
  #Scapholeberis
  CountSubsample[14]<-file$Count[14]
  
  #Simocephalus
  CountSubsample[15]<-file$Count[15]
  
  #Calanoida
  CountSubsample[16]<-file$Count[16]
  
  #Cyclopoida
  CountSubsample[17]<-file$Count[17]
  
  #Nauplii
  CountSubsample[18]<-file$Count[18]
  

  # ROTIFERS
  
  #Anuraeopsis
  CountSubsample[19]<-file$Count[19]
  
  #Ascomorpha
  CountSubsample[20]<-file$Count[20]
  
  #Asplanchna
  CountSubsample[21]<-file$Count[21]
  
  #Asplanchnopus
  CountSubsample[22]<-file$Count[22]
  
  #Brachionus
  CountSubsample[23]<-file$Count[23]
  
  #Conochilus
  #NOTE: two measurements for this genus, hence the shift in indexing
  CountSubsample[24]<-file$Count[24]
  
  #Euchlanis
  CountSubsample[25]<-file$Count[26]
  
  #Filinia
  CountSubsample[26]<-file$Count[27]
  
  #Gastropus
  CountSubsample[27]<-file$Count[28]
  
  #Hexarthra
  CountSubsample[28]<-file$Count[29]
  
  #Kellicotia
  CountSubsample[29]<-file$Count[30]
  
  #Keratella.cochlearis
  CountSubsample[30]<-file$Count[31]
  
  #Keratella.quadrata
  CountSubsample[31]<-file$Count[32]
  
  #Lecane
  CountSubsample[32]<-file$Count[33]
  
  #Lepadella
  CountSubsample[33]<-file$Count[34]
  
  #Macrochaetus
  CountSubsample[34]<-file$Count[35]
  
  #Monostyla
  CountSubsample[35]<-file$Count[36]
  
  #Mytilina
  CountSubsample[36]<-file$Count[36]
  
  #Notholca
  CountSubsample[37]<-file$Count[39]
  
  #Platyias
  CountSubsample[38]<-file$Count[40]
  
  #Ploesoma
  CountSubsample[39]<-file$Count[41]
  
  #Polyarthra
  CountSubsample[40]<-file$Count[42]
  
  #Pompholyx
  CountSubsample[41]<-file$Count[43]
  
  #Synchaeta
  CountSubsample[42]<-file$Count[44]
  
  #Testudinella
  CountSubsample[43]<-file$Count[45]
  
  #Trichocerca
  CountSubsample[44]<-file$Count[46]
  
  #Trichotria
  CountSubsample[45]<-file$Count[48]
  
  #Ostracoda 
  CountSubsample[46]<-file$Count[49]
  
  #Extract information like the sampleID, Lake number, and DOY of sampling from the files
  
  # SAMPLE IDS 
  # NOTE: remove the '#' in front of line 277 or 280 to run
  # ONLY 277 OR 280 should be run - NOT BOTH!! Use '#' in front to comment out one line
  
  #If the file names are saved as the 12-character sampleID (e.g., A19114204001)
  #and you want to keep all 12 characters/digits, use this: 
  SampleID<-(rep(substr(filenames[i],1,12),46)) #45 is the number of taxa
  
  #If the file names are saved as the 9-character sampleID (e.g., A19114204) use this:
  # SampleID<-(rep(substr(filenames[i],1,9),45)) #45 is the number of taxa
  
  #Extract the 3-digit lake number from the sampleID which is the file name
  LakeNo<-(rep(substr(filenames[i],4,6),46)) #pond number is the 4-6th spot
  
  #Extract the 3-digit day of year from the sampleID which is the file name
  doy<- (rep(substr(filenames[i],7,9),46)) #DOY is the 7-9th spot
  
  SAMPLE.ID <- append(SAMPLE.ID, SampleID)
  LAKE.NO <- append(LAKE.NO, LakeNo)
  DOY <- append(DOY, doy)
  TAXON <- append(TAXON, Taxon)
  COUNT <- append(COUNT, CountSubsample)
  
}

# STEP 6: MAKE A USEFUL OUTPUT OF THE COUNT DATA TO CALCULATE A WEIGHTED MEAN ============================

# Start by making a data frame of the useful info from above
Zoop.gv.count <-data.frame(SAMPLE.ID, LAKE.NO, DOY, TAXON, COUNT)

# Use the tidyverse to add more columns
library(tidyverse)
Zoop.count = Zoop.gv.count %>%
  # Create a new column ("GROUP") that creates the broader taxonomic groups for analysis
  mutate(GROUP = case_when(.$TAXON %in% c("Alona",
                                          "Alonella",
                                          "Bosmina",
                                          "Chydorus",
                                          "Pleuroxus") ~ "SmCladocera",
                           .$TAXON %in% c("Camptocercus",
                                          "Ceriodaphnia",
                                          "Daphnia",
                                          "Daphnia.lumholtzi",
                                          "Diaphanosoma",
                                          "Graptoleberis",
                                          "Leptodora",
                                          "Moina",
                                          "Scapholeberis",
                                          "Simocephalus") ~ "LgCladocera",
                           .$TAXON %in% c("Anuraeopsis",
                                          "Ascomorpha",
                                          "Asplanchna",
                                          "Asplanchnopus",
                                          "Brachionus",
                                          "Conochilus",
                                          "Euchlanis",
                                          "Filinia",
                                          "Gastropus",
                                          "Hexarthra",
                                          "Kellicottia",
                                          "Keratella.cochlearis",
                                          "Keratella.quadrata",
                                          "Lecane", 
                                          "Lepadella",
                                          "Macrochaetus",
                                          "Monostyla",
                                          "Mytilina",
                                          "Notholca",
                                          "Platyias",
                                          "Ploesoma",
                                          "Polyarthra",
                                          "Pompholyx",
                                          "Synchaeta",
                                          "Testudinella",
                                          "Trichocerca",
                                          "Trichotria") ~ "Rotifer",
                           .$TAXON %in% c("Cyclopoida") ~ "Cyclopoid",
                           .$TAXON %in% c("Calanoida") ~ "Calanoid",
                           .$TAXON %in% c("Nauplii") ~ "Nauplii", 
                           .$TAXON %in% c("Ostracod") ~ "Ostracod"))


# Create a CSV file of the data frame -- NOTE: this will be in the working directory
# Will need to remove file from working directory before re-running the analysis
Zoop.count
#Set working directory to data file 
setwd("C:/Users/Owner/Box/Iowa Data/Biology Data/Zooplankton/2019 Green Valley Zooplankton")
#write.csv(Zoop.count,"2019_site4_gv_ZoopCount_20Apr2021.csv", row.names = F)

# Turn raw R output into cleaned zoop data
zp_count = read.csv('2019_site4_gv_ZoopCount_20Apr2021.csv')
zp_count2 = zp_count %>% # renames columns to better names 
  rename(sampleid = SAMPLE.ID,
         taxon = TAXON,
         lakeno = LAKE.NO,
         count = COUNT,
         group = GROUP,
         doy = DOY)
zp_count2$group <- as.factor(zp_count2$group) # makes the group column a factor, easier for later analysis 

gv19_COUNT = zp_count2 %>% 
  select(sampleid,doy, taxon, group, count) %>% 
  mutate(count = replace_na(count, 0)) %>% # Replace NAs with 0s 
  filter(!(doy == 162 | doy == 157)) %>% # Remove the ALM sampling dates plus DOY 157 which we determined was wonky
  as_tibble()
gv19_COUNT

