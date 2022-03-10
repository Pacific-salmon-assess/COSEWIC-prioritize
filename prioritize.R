#load packages
library("tidyverse")
#library(SOMETHING STAN?)

# read in and reshape data ----------------------------------------------------------

#download Eric's compilation repo. keen on suggestions for a better robust way to pull
  #most recent spawner abundance data.
download.file(url = "https://github.com/hertzPSF/COSEWIC-compilation/archive/master.zip",
              destfile = "compilation/hertzPSF-compilation.zip")
unzip(zipfile = "compilation/hertzPSF-compilation.zip", exdir = "compilation")

erics_output <- list.files("compilation/COSEWIC-compilation-main/output")

#this just grabs whatever file contains the grep() argument - need to talk w/ Eric about 
  #how we want to make the compilation code robust (e.g. timestamps, overwriting etc.)
  #if we overwrite files in the output this will work fine.
  #all start at 1950 and end between 2017-2020
raw_sp_data <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                           erics_output[grep("CU_Spawner_Abund", erics_output)]), 
                        header = TRUE) %>%
         mutate(logSp = log(Spawner.Abundance))

cu_metadata <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                               erics_output[grep("CU_Metadata", erics_output)]), 
                        header = TRUE, na.strings = c("", "NA")) %>%
  filter(!is.na(gen_length)) %>%
  rename(CUID = cuid)

#join, dump pops with NA DUs, and create a du_cu key
sp_data <- left_join(raw_sp_data, select(cu_metadata, CUID, du_number, gen_length), 
                     by = "CUID") %>%
  drop_na(du_number) %>% #do we want to boot the NA DUs? - if not turn off
  mutate(du_cu = paste(du_number, CUID)) %>%
  relocate(du_cu, 1) %>%
  arrange(du_cu, Year)

#isolate the last 3 generations of data #
final_survey_yr <- sp_data %>% #helper object
  drop_na(Spawner.Abundance) %>%
  group_by(du_cu) %>%
  summarise(last_year = max(Year))

last3_gens <- left_join(sp_data, final_survey_yr, by = "du_cu") %>%
  group_by(du_cu) %>%
  mutate(three_gens = gen_length*3) %>%
  filter(Year>= (last_year - three_gens), Year <= last_year)
  
rm(raw_sp_data, erics_output, du_cu_yrs, final_survey_yr) #remove helpers

# plots -----------------------------------------------------------------------------
max_facets <- 20 #how many facets per plot?
save_plots <- FALSE #toggle if you want to save plots 

#raw spawner abundance and log spawners with different lms 
  #long and ugly indexing here makes for less cluttered, robust plots in the output 
for(i in unique(sp_data$Species)){
  sub_data <- filter(sp_data, Species == i) %>%
    na.omit(Spawner.Abundance)
  
  #for j in region...
C  
  if(length(unique(sub_data$du_cu)) > max_facets){
    pages <- ceiling(length(unique(sub_data$du_cu))/max_facets)
    
    for(j in 1:pages){
      sub_sub_data <- filter(sub_data, du_cu %in% 
                               unique(sub_data$du_cu)[((j-1)*max_facets)+1:(max_facets*j)])
      
      sub_sub_last3 <- filter(sub_last3, du_cu %in% 
                                unique(sub_last3$du_cu)[((j-1)*max_facets)+1:(max_facets*j)])
      
      p <- ggplot(data = sub_sub_data, aes(x= Year, y= Spawner.Abundance/1000)) +
        geom_line(color = "grey") +
        geom_point() +
        facet_wrap(~du_cu, scales = "free_y") +
        labs(x= "Year", y = "Spawners (thousands)", 
             title = paste0(i, " spawner abundance ", "(", j, " of ", pages, ")")) +
        theme_bw() +
        theme(strip.background = element_blank())
      
      print(p)
      
      p_log <- ggplot(data = sub_sub_data, aes(x= Year, y= logSp)) +
        geom_line(color = "grey") +
        geom_point() +
        stat_smooth(method="lm", formula = y~x, color = "black", lty= "dashed", 
                    se = FALSE) +
        stat_smooth(data = sub_sub_last3, formula = y~x, method = "lm", color = "red", 
                    se = FALSE, na.rm = TRUE) +
        facet_wrap(~du_cu, scales = "free_y") +
        labs(x= "Year", y = "ln(Spawners)", 
             title = paste0("ln ", i, " spawner abundance ", "(", j, " of ", pages, ")")) +
        theme_bw() +
        theme(strip.background = element_blank())
      
      print(p_log)
      
      if(save_plots){
        ggsave(paste0("output/plots/","sp_abundance_", i, "_", j, ".png"), plot = p)
        ggsave(paste0("output/plots/","ln_sp_lm_", i, "_", j, ".png"), plot = p_log)
      }
    }
  }else{
    p <- ggplot(data = sub_data, aes(x= Year, y= Spawner.Abundance/1000)) +
      geom_line(color = "grey") +
      geom_point() +
      facet_wrap(~du_cu, scales = "free_y") +
      labs(x= "Year", y = "Spawners (thousands)", title = paste(i, "spawner abundance")) +
      theme_bw() +
      theme(strip.background = element_blank())
    
    print(p)
    
    p_log <- ggplot(data = sub_sub_data, aes(x= Year, y= logSp)) +
      geom_line(color = "grey") +
      geom_point() +
      stat_smooth(formula = y~x, method="lm", color = "black", lty= "dashed", se = FALSE) +
      stat_smooth(data = sub_sub_last3, formula = y~x, method = "lm", color = "red", 
                  se = FALSE, na.rm = TRUE) +
      facet_wrap(~du_cu, scales = "free_y") +
      labs(x= "Year", y = "ln(Spawners)", 
           title = paste0("ln ", i, "spawner abundance ", "(", j, " of ", pages, ")")) +
      theme_bw() +
      theme(strip.background = element_blank())
    
    print(p_log)
    
    if(save_plots){
      ggsave(paste0("output/plots/", "sp_abundance_", i, ".png"), plot = p)
      ggsave(paste0("output/plots/","ln_sp_lm_", i, ".png"), plot = p_log)
    }
  }
}

# filter out data deficient stocks to disclude from analysis -----------------------------
  #define threshold for what not to include in recent assessments and all assessments. 

recent_perc_na <- 0.33 #what % NA means data deficient RECENT pops

deficient_recent_du_cus <- last3_gens %>%
  group_by(du_cu) %>%
  mutate(helper = ifelse(is.na(Spawner.Abundance), 1, 0)) %>%
  summarise(yrs_not_assessed = sum(helper), 
            yrs = n()) %>%
  mutate(perc_missing = (yrs_not_assessed/yrs)) %>%
  filter(perc_missing >= recent_perc_na) %>%
  pull(du_cu)

min_yr <- 1990
all_perc_na <- 0.8 #% deficient for data starting in min_yr

deficient_all <- sp_data %>%
  filter(Year> min_yr) %>%
  group_by(du_cu) %>%
  mutate(helper = ifelse(is.na(Spawner.Abundance), 1, 0)) %>%
  summarise(yrs_not_assessed = sum(helper), 
            yrs = n()) %>%
  mutate(perc_missing = (yrs_not_assessed/yrs)) %>%
  filter(perc_missing >= all_perc_na) %>%
  pull(du_cu)

#join the info of the last 3 gens into one object to feed to regressions 
sp_data_filtered <- left_join(sp_data, select(last3_gens, du_cu, Year, last_year), 
                              by = c("du_cu", "Year")) %>%
  mutate(last3 = ifelse(!is.na(last_year), "yes", "no")) %>%
  select(-last_year) %>% #remove col that was just a helper
  filter(!du_cu %in% c(deficient_recent_du_cus, deficient_all, no_gen_len))

# summarise % change with multiple methods ------------------------------------------
  #slope of lm from the last 15 years, slope from the entire timeseries, slope HPD from 
  #Stan, recent abundance, last year of data, n data for last 3 generations (as a %?)
summary_table <- NULL

for(i in unique(sp_data_filtered$du_cu)){
  sub_data <- filter(sp_data_filtered, du_cu == i)
  sub_last3 <- filter(sp_data_filtered, du_cu == i, last3 == "yes")
  
  model_all <- lm(sub_data$logSp~sub_data$Year)
  model_last3 <- lm(sub_last3$logSp ~ sub_last3$Year)
  
  output <- data.frame(i, round(as.numeric((exp(model_last3$coefficients[2]*15)-1)*100)),
                       round(as.numeric((exp(model_all$coefficients[2]*15)-1)*100)),
                       round(exp(last(na.omit(sub_last3$logSp)))), max(sub_last3$Year))
  
  summary_table <- bind_rows(output, summary_table)
}

colnames(summary_table) <- c("DU CU", "% change (recent)", "% change (all)", 
                             "recent abundance", "last year with data")

#dig up the rules for how steep the slope needs to be for the pop to fit into a given 
  #COSEWIC category, then write these probabilities out. 

#plot HPD over these categories too in a visualization - perhaps rank the plotting order
  #of importance by pop. 