# Code to compile Pacific Salmon Explorer data for COSEWIC
# The code was originally written by Eric Hertz at the Pacific Salmon Foundation ehertz@psf.ca 

rm(list = ls(all=TRUE)); #Remove all the objects in the memory
setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/1_Active/SARA/Data & Analysis/COSEWIC-compilation/")
library(tidyverse)
library(broom)



#### 1 Read in and manipulate data

#read in CU level data
cc_file <- read.csv("data/dataset_1part1.Dec072020_CC.csv", header = T)
fraser_file <- read.csv("data/dataset_1part1.Jul282020_Fraser.csv", header = T)
vimi_file <- read.csv("data/dataset_1part1.Jul282021_VIMI.csv", header = T)
nass_file <- read.csv("data/dataset_1part1.Dec092020_Nass.csv", header = T)
skeena_file <- read.csv("data/dataset_1part1.Dec092020_Skeena.csv", header = T)
hg_file <- read.csv("data/dataset_1part1.Oct252021_HG.csv", header = T)
columbia_file <- read.csv("data/dataset_1part1.NOV272019_Columbia.csv", header = T)

# combine files from each region
cu_dat <- rbind(cc_file,fraser_file,vimi_file,nass_file,skeena_file,hg_file,columbia_file)
cu_dat <- select(cu_dat,CUID,Species,Year,LGL.counts,Region)

#check species names
unique(cu_dat$Species)
cu_dat$Species[cu_dat$Species=="River Sockeye"] <- "Sockeye"
cu_dat$Species[cu_dat$Species=="Lake Sockeye"] <- "Sockeye"

names(cu_dat)[names(cu_dat) == "LGL.counts"] <- "Spawner.Abundance"

write.csv(cu_dat, "Output/CU_Spawner_Abund_20220112.csv", row.names=FALSE)


#### read in CU decoder
cu_decoder <- read.csv("data/All_regions_CU_decoder_CA.csv", header = T)

#read in other meta-data type files
cu_dq <- read.csv("data/AllRegions_CU_data_quality.csv", header = T)
cu_enh <- read.csv("data/CU_enhancement_levelAug252021.csv", header = T)
cu_sites <- read.csv("data/NuSEDS_sites.csv", header = T)

# join metadata files to cu_decoder
cu_2 <- left_join(cu_decoder, cu_dq, by=c("cuid"))
cu_3 <- left_join(cu_2, cu_enh, by=c("cuid"))
cu_4 <- left_join(cu_3, cu_sites, by=c("FULL_CU_IN"))

#select variables of interest
cu_5 <- select(cu_4,cuid,cuname.x,cu_acronym,du_number,du_acronym,DU_name,FULL_CU_IN,spp,gen_length,COSEWIC_status,survey_quality,survey_coverage,survey_execution,catch_quality,dq_score,cu_enh_rank,Sites)


write.csv(cu_5, "Output/CU_Metadata_20220112.csv", row.names=FALSE)


