# Code to compile Pacific Salmon Explorer data for COSEWIC
# The code was originally written by Eric Hertz at the Pacific Salmon Foundation ehertz@psf.ca 
library(tidyverse)

#### 1 Read in and manipulate data

#read in CU level data
cc_file <- read.csv("data/dataset_1part1.Dec072020_CC.csv", header = T)
fraser_file <- read.csv("data/dataset_1part1.Jul282020_Fraser.csv", header = T)
vimi_file <- read.csv("data/dataset_1part1.Jul282021_VIMI.csv", header = T)
nass_file <- read.csv("data/dataset_1part1.Dec092020_Nass.csv", header = T)
skeena_file <- read.csv("data/dataset_1part1.Dec092020_Skeena.csv", header = T)
hg_file <- read.csv("data/dataset_1part1.Oct252021_HG.csv", header = T)
columbia_file <- read.csv("data/dataset_1part1.NOV272019_Columbia.csv", header = T) %>%
  #commas in these counts were messing things up
  mutate(LGL.counts = as.integer(gsub(",", "", LGL.counts)), 
         NuSEDS.counts.by.CU = as.integer(gsub(",", "", NuSEDS.counts.by.CU))) 

# combine files from each region, switched this to bind_rows to keep the functions in 
  #tidyverse-land. bind_rows has useful warnings.  
cu_dat <- bind_rows(cc_file,fraser_file,vimi_file,nass_file,skeena_file,hg_file,columbia_file) %>%
  select(CUID, Species, Year, LGL.counts, Region)

#check species names
unique(cu_dat$Species)

#any 0's? in the spawners?
length(filter(cu_dat, LGL.counts==0)$LGL.counts)


cu_dat <- cu_dat %>%
  mutate(Species = ifelse(grepl("Sockeye", Species), "Sockeye", Species),
         Spawner.Abundance = ifelse(LGL.counts==0, NA, LGL.counts)) %>%
  select(-LGL.counts)

write.csv(cu_dat, "Output/CU_Spawner_Abund_20220112.csv", row.names=FALSE)

rm(cc_file, fraser_file, vimi_file, nass_file, skeena_file, hg_file, columbia_file)

#### read in CU decoder
cu_decoder <- read.csv("data/All_regions_CU_decoder_CA.csv", header = T)

#read in other meta-data type files
cu_dq <- read.csv("data/AllRegions_CU_data_quality.csv", header = T)
cu_enh <- read.csv("data/CU_enhancement_levelAug252021.csv", header = T) %>%
  rename(cuid = ï..cuid) #fix weird special char in colname
cu_sites <- read.csv("data/NuSEDS_sites.csv", header = T) %>%
  rename(FULL_CU_IN = ï..FULL_CU_IN) #fix weird special char in colname

# join metadata files to cu_decoder
cu_metadata <- left_join(cu_decoder, cu_dq, by="cuid") %>%
  left_join(., cu_enh, by = "cuid") %>%
  left_join(., cu_sites, by = "FULL_CU_IN") %>%
  select(cuid, cuname.x, cu_acronym, du_number, du_acronym, DU_name, FULL_CU_IN, spp,
         gen_length, COSEWIC_status, survey_quality, survey_coverage, survey_execution,
         catch_quality, dq_score, cu_enh_rank, Sites)

write.csv(cu_metadata, "Output/CU_Metadata_20220112.csv", row.names=FALSE)
