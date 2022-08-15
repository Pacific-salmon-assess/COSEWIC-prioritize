# comparison of OLS and Bayesian estimates of percent change in abundance using the MetricsCOSEWIC pkg

library(tidyverse)
library(MetricsCOSEWIC)

raw_sp_data <- read.csv("./data/CU_Spawner_Abund_20220812.csv", 
                        header=TRUE) %>%
  mutate(logSp=log(Spawner.Abundance))

cu_metadata <- read.csv("./data/CU_Metadata_20220812.csv",  
                        header=TRUE, na.strings=c("", "NA")) %>%
  filter(!is.na(gen_length)) %>%
  rename(CUID=cuid)

# join, dump pops with NA DUs ----
sp_data <- left_join(raw_sp_data, select(cu_metadata, Region, CUID, du_number, gen_length, Sites, cu_enh_rank, COSEWIC_status, spp), 
                     by="CUID") %>%
  drop_na(du_number, Spawner.Abundance) %>%
  mutate(Species = spp) %>%
  group_by(du_number, Region.y, Year, gen_length, Sites, cu_enh_rank, COSEWIC_status, Species) %>%
  rename(Region = Region.y) %>%
  summarise(mat_ind = sum(Spawner.Abundance))%>%
  arrange(du_number, Year)

# filter to DUs with spawner estimates in at least 50% of last three generations (and 20% of all years) ----
last3_gens <- sp_data %>%
  group_by(du_number) %>%
  mutate(three_gens = gen_length*3) %>%
  filter(Year > (last(Year) - three_gens), Year <= last(Year))

deficient_recent_perc <- 0.3

deficient_recent <- last3_gens %>%
  group_by(du_number) %>%
  summarise(n = n(), #years with data  
            n_proper = mean(gen_length)*3) %>% # years for 3 gens of data if fully sampled
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_recent_perc) %>%
  pull(du_number)

last3_gens_filtered <- filter(last3_gens, !(du_number %in% deficient_recent))

deficient_historical_perc <- 0.2

deficient_historical <- sp_data %>%
  group_by(du_number) %>%
  summarise(n = n(),
            n_proper = last(Year) - first(Year) + 1) %>%
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_historical_perc) %>%
  pull(du_number)

sp_data_filtered <- filter(sp_data, !(du_number %in% deficient_historical))

# add years with no observations (in between years with observations) as NAs, have to do separately for pinks and non-pinks due to fixed 2 yr life cycle
last3_gens_filtered2noPink <- last3_gens_filtered %>%
  filter(Species != "Pink") %>%
  mutate(Region = case_when(Region == "HG" ~ "Haida Gwaii",
                            Region == "CC" ~ "Central Coast",
                            Region == "VIMI" ~ "Van. Isl. Main. Inl.", 
                            TRUE ~ Region)) %>%
  group_by(du_number) %>%
  as.data.frame() %>%
  group_by(du_number) %>%
  mutate(first_yr = first(Year),
         lst_yr = last(Year)) %>%
  filter(Year >= (first_yr),
         Year <= (lst_yr)) %>%
          complete(nesting(du_number), Year = full_seq(Year, period = 1), fill = list(mat_ind = NA))

last3_gens_filtered2Pink <- last3_gens_filtered %>%
  filter(Species == "Pink") %>%
  mutate(Region = case_when(Region == "HG" ~ "Haida Gwaii",
                            Region == "CC" ~ "Central Coast",
                            Region == "VIMI" ~ "Van. Isl. Main. Inl.", 
                            TRUE ~ Region)) %>%
  group_by(du_number) %>%
  as.data.frame() %>%
  group_by(du_number) %>%
  mutate(first_yr = first(Year),
         lst_yr = last(Year)) %>%
  filter(Year >= (first_yr),
         Year <= (lst_yr)) %>%
  complete(nesting(du_number), Year = full_seq(Year, period = 2), fill = list(mat_ind = NA))

last3_gens_filtered2 <- rbind(last3_gens_filtered2noPink,last3_gens_filtered2Pink)
        
# fit linear model with jags using MetricsCOSEWIC package
slope_posterior <- NULL
jags_summary <- NULL

for(i in unique(last3_gens_filtered2$du_number)){
  sub_data <- filter(last3_gens_filtered2, du_number == i)
  jags_mod <- calcPercChangeMCMC(as.numeric(scale(log(sub_data$mat_ind))), method="jags", out.type="long") 
  
  summary <- data.frame(jags_mod$pchange)  %>%
    mutate(du_number = i,
           Species = sub_data$Species[1],
           Region = sub_data$Region[1],
           pchange = jags_mod.pchange) %>%
    relocate(du_number, Region, Species, pchange) %>%
    select(du_number, Region, Species, pchange) 

  jags_summary <- bind_rows(summary, jags_summary)
}

# merge with master summary with OLS based estimates of change
org_master <- read.csv("./output/master-prob-status/master_status.csv")
xx <- left_join(org_master, jags_summary, by = "du_number") %>%
  mutate(diff_in_est = per_change_recent-pchange)

# estimates are generally in the same direction and general magnitude but definitely not lining up tightly, need to explore more to diagnose why. 
