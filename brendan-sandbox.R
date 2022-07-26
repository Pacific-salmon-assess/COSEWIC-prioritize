##  Brendan's sandbox
library("tidyverse")
library(MetricsCOSEWIC)

# read in data ----
raw_sp_data <- read.csv("./data/CU_Spawner_Abund_20220725.csv", 
                        header=TRUE) %>%
  mutate(logSp=log(Spawner.Abundance))

cu_metadata <- read.csv("./data/CU_Metadata_20220725.csv",  
                        header=TRUE, na.strings=c("", "NA")) %>%
  filter(!is.na(gen_length)) %>%
  rename(CUID=cuid)

#join, dump pops with NA DUs
sp_data <- left_join(raw_sp_data, select(cu_metadata, CUID, du_number, gen_length, Sites, cu_enh_rank, COSEWIC_status, spp), 
                     by="CUID") %>%
  drop_na(du_number, Spawner.Abundance) %>%
  mutate(Species = spp) %>%
  group_by(du_number, Region, Year, gen_length, Sites, cu_enh_rank, COSEWIC_status, Species) %>%
  summarise(mat_ind = sum(Spawner.Abundance))%>%
  arrange(du_number, Year)


# filter to DUs with mat individual estimates in at least 30% of last three generations ----
last3_gens <- sp_data %>%
  group_by(du_number) %>%
  mutate(three_gens = gen_length*3) %>%
  filter(Year > (last(Year) - three_gens), Year <= last(Year))

deficient_recent_perc <- 0.5

deficient_recent <- last3_gens %>%
  group_by(du_number) %>%
  summarise(n = n(), #years with data  
            n_proper = mean(gen_length)*3) %>% #years for 3 gens of data if fully sampled
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_recent_perc) %>%
  pull(du_number)

last3_gens_filtered <- filter(last3_gens, !(du_number %in% deficient_recent))

deficient_historical_perc <- 0.5

deficient_historical <- sp_data %>%
  group_by(du_number) %>%
  summarise(n = n(),
            n_proper = last(Year) - first(Year) + 1) %>%
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_historical_perc) %>%
  pull(du_number)

sp_data_filtered <- filter(sp_data, !(du_number %in% deficient_historical))

# estimates rate of change simple lm ----
summary_table <- NULL

for(i in unique(sp_data_filtered$du_number)){
  sub_raw <- filter(sp_data, du_number==i)
  sub_data <- filter(sp_data_filtered, du_number==i)
  sub_last3 <- filter(last3_gens_filtered, du_number==i)
  if(nrow(sub_data)==0){
    slope_all <- NA
  } else{
    if(nrow(sub_last3)==0){
      slope_recent <- NA
    } else{
      model_all <- lm(log(sub_data$mat_ind)~sub_data$Year)
      model_recent <- lm(log(sub_last3$mat_ind)~sub_last3$Year)
      per_change_all <- round(as.numeric((exp(model_all$coefficients[2]*(3*unique(sub_data$gen_length)))-1)*100))
      per_change_recent <- round(as.numeric((exp(model_recent$coefficients[2]*(3*unique(sub_data$gen_length)))-1)*100))
    }
  }
  recent_abundance <- last(sub_raw$mat_ind)
  last_year_monitored <- last(sub_raw$Year)
  
  output <- data.frame(du_number=i, region = unique(sub_raw$Region), per_change_recent, per_change_all, recent_abundance, last_year_monitored)
  
  summary_table <- bind_rows(output, summary_table)
}

head(summary_table)

prob_desg <- summary_table %>%
  mutate(prob_designation_recent = case_when(
    per_change_recent  > -29 ~ "Not at risk",
    per_change_recent < -29 & per_change_recent > -49 ~ "Special concern",
    per_change_recent < -49 & per_change_recent > -69 ~ "Threatened",
    per_change_recent < -69 ~ "Endangered")) %>%
  mutate(prob_designation_all = case_when(
    per_change_all  > -29 ~ "Not at risk",
    per_change_all < -29 & per_change_all > -49 ~ "Special concern",
    per_change_all < -49 & per_change_all > -69 ~ "Threatened",
    per_change_all < -69 ~ "Endangered")) %>%
  mutate(region_full = case_when(
    region == "Yukon" ~ "Yukon",
    region == "Nass" ~ "Nass",
    region == "Skeena" ~ "Skeena",
    region == "HG" ~ "Haida Gwaii",
    region == "CC" ~ "Central Coast",
    region == "VIMI" ~ "Van. Isl. Main. Inl.",
    region == "Fraser" ~ "Fraser" ))

master_desg <- left_join(cu_metadata, select(prob_desg, region_full, du_number, per_change_recent, per_change_all, recent_abundance, last_year_monitored, prob_designation_recent,prob_designation_all), 
                     by="du_number") 

master_desg$COSEWIC_status <- master_desg$COSEWIC_status %>%
  replace_na("Not assessed")

assessed <- master_desg %>%
  filter(COSEWIC_status != "Not assessed")

not_assessed <- master_desg %>%
  filter(COSEWIC_status == "Not assessed") 

not_assessed$prob_designation_recent <- not_assessed$prob_designation_recent %>%
  replace_na("Data defficient")

not_assessed$prob_designation_all <- not_assessed$prob_designation_all %>%
  replace_na("Data defficient")

designations <- rbind(assessed,not_assessed)%>%
  mutate(species = spp) %>%
  select(region_full, du_number,species, gen_length, COSEWIC_status,per_change_recent, per_change_all, recent_abundance, last_year_monitored, prob_designation_recent, prob_designation_all)%>%
  arrange(du_number)%>%
  distinct(du_number,.keep_all = TRUE)

unassessed <- designations %>%
  filter(COSEWIC_status == "Not assessed",
         prob_designation_recent != "Data defficient")

# plot based on recent trend----
count_bins <- unassessed%>%
  group_by(species, region_full)%>%
  summarize(du_count=n())


count_bins_status <- unassessed%>%
  group_by(species, region_full, prob_designation_recent)%>%
  summarize(du_count=n())

count_bins_status$prob_designation_recent <- factor(count_bins_status$prob_designation_recent, levels = rev(c("Endangered","Threatened", "Special concern", "Not at risk")))
count_bins_status$region_full <- factor(count_bins_status$region_full, levels = c("Yukon", "Nass","Skeena", "Haida Gwaii", "Central Coast", "Van. Isl. Main. Inl.", "Fraser"))

# hard code Fraser pinks which have 8 DUs but only one CU
count_bins_status[37,4] <- 1
ggplot(count_bins_status, aes(x = species, y = du_count)) + 
  geom_col(aes(fill=prob_designation_recent)) +
  facet_wrap(~region_full, scales="free_y", ncol=4) +
  theme(axis.text.x = element_text(angle=45, hjust=1), legend.position = "top") +
  scale_fill_manual(values = c("dark green", "yellow", "orange","red")) +
  xlab("Species") +
  ylab("Number of DUs") +
  labs(fill = "Probable designation")
ggsave("./output/plots/brendan-sandox-plots/status-plot-recent.jpeg",height=5,width=8)

# plot based on long-term trend----
count_bins <- unassessed%>%
  group_by(species, region)%>%
  summarize(du_count=n())

count_bins_status <- unassessed%>%
  group_by(species, region,prob_designation_all)%>%
  summarize(du_count=n())

count_bins_status$prob_designation_all <- factor(count_bins_status$prob_designation_all, levels = rev(c("Endangered","Threatened", "Special concern", "Not at risk")))

ggplot(count_bins_status, aes(x = species, y = du_count)) + 
  geom_col(aes(fill=prob_designation_all)) +
  facet_wrap(~region, scales="free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  scale_fill_manual(values = c("dark green", "yellow", "orange","red")) +
  xlab("Species") +
  ylab("Number of DUs") +
  labs(fill = "Probable designation")
ggsave("./output/plots/brendan-sandox-plots/status-plot-all.jpeg",height=6,width=8)























# estimates rate of change Bays ---- # gives wonky results, revist later
slope_posterior <- NULL
jags_summary <- NULL

for(i in unique(last3_gens_filtered$du_number)){
  sub_data <- filter(last3_gens_filtered, du_number == i)
  jags_mod <- calcPercChangeMCMC(sub_data$mat_ind, method="jags", out.type="long") 
  
  summary <- data.frame(jags_mod$summary) %>%
    slice(1:3) %>%
    select(mean, sd, X2.5., X50., X97.5., n.eff, Rhat) %>%
    rename(perc_2.5 = X2.5., 
           perc_97.5 = X97.5.,
           median = X50.) %>%
    mutate(du_number = i,
           region = sub_data$Region[1],
           species = sub_data$Species[1]) %>%
    relocate(du_number, region, species, .before = mean) 
  
  rownames(summary) <- NULL #rownames to columns - probably a better way
  summary$var <- c("intercept", "slope", "sigma")
  
  jags_summary <- bind_rows(summary, jags_summary)
  
  slope_posterior <- bind_rows(data.frame(du_number =i, region = unique(sub_data$Region),
                                          species = unique(sub_data$Species),
                                          draw = jags_mod$samples$slope), slope_posterior)
}

jags_summary <- relocate(jags_summary, var, .before = mean)


perc_category <- slope_posterior %>%
  group_by(species, region, du_number) %>%
  summarise(not_at_risk = round(sum(draw>(-30))/n()*100, 2),
            least_concern = round(sum((-30)>draw & draw>(-50))/n()*100, 2), 
            threatened = round(sum(draw<(-50) & draw>(-70))/n()*100, 2), 
            endangered = round(sum(draw<(-70))/n()*100, 2)) %>%
  mutate(prob_designation = case_when(
    not_at_risk > 50 ~ "not at risk",
    least_concern > 50 ~ "least concern",
    threatened > 50 ~ "threatened",
    endangered > 50 ~ "endangered"))
  

head(perc_category)
