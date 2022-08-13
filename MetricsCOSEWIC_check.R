library(tidyverse)
library(MetricsCOSEWIC)

#trying to see why the calculations from the simple lm() are different from the 
  #CalcPercChangeMCMC() fun from MetricsCOSEWIC package

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

# tidy up names and scale the data WITHIN populations
last3_gens_filtered2 <- last3_gens_filtered %>%
  mutate(Region = case_when(Region == "HG" ~ "Haida Gwaii",
                            Region == "CC" ~ "Central Coast",
                            Region == "VIMI" ~ "Van. Isl. Main. Inl.", 
                            TRUE ~ Region)) %>%
  group_by(du_number) %>%
  as.data.frame() %>%
  mutate(mat_ind_scaled = (mat_ind-mean(mat_ind)/sd(mat_ind))) #maybe I also need to explicitly add NAs??
  #^ had to longhand this because scale() was messing up even after adding as.data.frame()
  
# fit the model - it runs...
slope_posterior <- NULL
jags_summary <- NULL

for(i in unique(last3_gens_filtered2$du_number)){
  sub_data <- filter(last3_gens_filtered2, du_number == i)
  jags_mod <- calcPercChangeMCMC(sub_data$mat_ind_scaled, method="jags", out.type="long") 
  
  summary <- data.frame(jags_mod$summary) %>%
    slice(1:3) %>%
    select(mean, sd, X2.5., X50., X97.5., n.eff, Rhat) %>%
    rename(perc_2.5 = X2.5., 
           perc_97.5 = X97.5.,
           median = X50.) %>%
    mutate(du_number = i,
           Species = sub_data$Species[1],
           Region = sub_data$Region[1]) %>%
    relocate(du_number, Region, Species, .before = mean) 
  
  rownames(summary) <- NULL #rownames to columns - probably a better way
  summary$var <- c("intercept", "slope", "sigma")
  
  jags_summary <- bind_rows(summary, jags_summary)
  
  slope_posterior <- bind_rows(data.frame(du_number =i, Region = unique(sub_data$Region),
                                          Species = unique(sub_data$Species),
                                          draw = jags_mod$samples$slope), slope_posterior)
}

jags_summary <- relocate(jags_summary, var, .before = mean)

jags_slopes <- jags_summary %>%
  filter(var == "slope")


#helper function to center limits
symmetric_limits <- function(x){
  max <- max(abs(x))
  c(-max, max)
}

max_facets <- 20
rows <- 4
cols <- 5

for(i in unique(slope_posterior$Region)){
  post_region <- filter(slope_posterior, Region == i)
  for(j in unique(post_region$Species)){
    sub_data <- filter(post_region, Species == j)
    if(length(unique(sub_data$du_number)) > max_facets){
      pages <- ceiling(length(unique(sub_data$du_number))/max_facets)
      for(k in 1:pages){
        sub_data_2 <- filter(sub_data, du_number %in% 
                               unique(sub_data$du_number)[((k-1)*max_facets)+1:(max_facets*k)])
        p <- ggplot(data = sub_data_2, aes(draw)) +
          geom_density(fill = "gray") + 
          scale_x_continuous(limits = symmetric_limits) +
          geom_rect(aes(xmin=-30, xmax=Inf, ymin=0, ymax= Inf), fill="green", alpha=0.002) +
          geom_rect(aes(xmin=-50 , xmax=-30, ymax=Inf, ymin=-Inf), fill="yellow", alpha=0.002) +
          geom_rect(aes(xmin=-70, xmax=-50, ymax=Inf, ymin=-Inf), fill="orange", alpha=0.002) +
          geom_rect(aes(xmin=-Inf, xmax=-70, ymax=Inf, ymin=-Inf), fill="red", alpha=0.002) +
          theme_classic() +
          ggh4x::facet_wrap2(vars(du_number), nrow=rows, ncol=cols, trim_blank=FALSE, 
                             scales="free") +
          labs(title = paste(i, j, "slope posterior", "(", k, " of ", pages, ")"), 
               x = "MCMC draws") +
          guides(x = "none", y = "none")
        print(p)
      }
    }else{      p <- ggplot(data = sub_data, aes(draw)) +
      geom_density(fill = "gray") + 
      scale_x_continuous(limits = symmetric_limits) +
      geom_rect(aes(xmin=-30, xmax=Inf, ymin=0, ymax= Inf), fill="green", alpha=0.002) +
      geom_rect(aes(xmin=-50 , xmax=-30, ymax=Inf, ymin=-Inf), fill="yellow", alpha=0.002) +
      geom_rect(aes(xmin=-70, xmax=-50, ymax=Inf, ymin=-Inf), fill="orange", alpha=0.002) +
      geom_rect(aes(xmin=-Inf, xmax=-70, ymax=Inf, ymin=-Inf), fill="red", alpha=0.002) +
      theme_classic() +
      ggh4x::facet_wrap2(vars(du_number), nrow=rows, ncol=cols, trim_blank=FALSE, 
                         scales="free") +
      labs(title = paste(i, j, "slope posterior"), x = "MCMC draws") +
      guides(x = "none", y = "none")
    print(p)
    }
  }
}

# estimates rate of change using simple lm ----
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
  
  output <- data.frame(du_number=i, Region = unique(sub_raw$Region), per_change_recent, per_change_all, recent_abundance, last_year_monitored)
  
  summary_table <- bind_rows(output, summary_table)
}

lm_slopes <- summary_table %>%
  rename(lm_slope = per_change_recent) %>%
  select(du_number, lm_slope)

compare_declines <- jags_summary %>%
  filter(var == "slope") %>%
  rename(jags_slope_median = median) %>%
  select(du_number, jags_slope_median) %>%
  left_join(., lm_slopes, by = "du_number") 
