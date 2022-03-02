#load packages
library("tidyverse")
#library(SOMETHING STAN?)

# read in and reshape data ----------------------------------------------------------

#download Eric's compilation repo. keen on suggestions for a better robust way to pull
  #most recent spawner abundance data. A non robust way is just to read.csv() the 
  #"raw.githubusercontent.com...."
download.file(url = "https://github.com/hertzPSF/COSEWIC-compilation/archive/master.zip",
              destfile = "compilation/hertzPSF-compilation.zip")
unzip(zipfile = "compilation/hertzPSF-compilation.zip", exdir = "compilation")

erics_output <- list.files("compilation/COSEWIC-compilation-main/output")

#get most recent timestamp
  #this just grabs whatever file contains the grep() argument - need to talk w/ Eric
  #about how we want to make the compilation code robust (e.g. with timestamps,
  #overwriting files, etc.)
  #if we overwrite files in the output this will work fine. 
raw_sp_data <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                           erics_output[grep("CU_Spawner_Abund", erics_output)]), 
                    header = TRUE) %>%
  mutate(Spawner.Abundance = as.numeric(gsub(",", "", Spawner.Abundance)), 
         logSp = log(Spawner.Abundance))

cu_metadata <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                               erics_output[grep("CU_Metadata", erics_output)]), 
                        header = TRUE, na.strings = c("", "NA")) %>%
  rename(CUID = cuid)

#following brendan's example I join these, dump pops with NA DUs, and create a du_cu key
sp_data <- left_join(raw_sp_data, select(cu_metadata, CUID, du_number, gen_length), 
                     by = "CUID") %>%
  drop_na(du_number) %>% #do we want to boot the NA DUs? - if not turn off
  mutate(du_cu = paste(du_number, CUID)) %>%
  relocate(du_cu, 1) %>%
  arrange(du_cu, Year)

#do some checks###
#species levels don't really lineup. do we care? 
unique(cu_metadata$spp)
unique(sp_data$Species) 

#do years overlap?
du_cu_yrs <- sp_data %>%
  group_by(du_cu) %>%
  summarise(yrs = n(), 
            start_yr = min(Year), 
            end_yr = max(Year))
  #yea looks good - all start at 1950 and some end 2017-2020

#isolate the last 3 generations of data #
final_survey_yr <- sp_data %>% #helper object
  drop_na(Spawner.Abundance) %>%
  group_by(du_cu) %>%
  summarise(last_year = max(Year))

last3_gens <- left_join(sp_data, final_survey_yr, by = "du_cu") %>%
  group_by(du_cu) %>%
  mutate(three_gens = gen_length*3) %>%
  filter(Year>= (last_year - three_gens) , Year <= last_year)
  
rm(raw_sp_data, erics_output, du_cu_yrs, final_survey_yr) #remove helpers

# plots -----------------------------------------------------------------------------
max_facets <- 20 #how many facets per plot?
save_plots <- TRUE #toggle if you want to save plots 

#raw spawner abundance and log spawners with different lms 
  #long and ugly indexing here makes for less cluttered, robust plots in the output 
for(i in unique(sp_data$Species)){
  sub_data <- filter(sp_data, Species == i) %>%
    na.omit(Spawner.Abundance)
  
  sub_last3 <- filter(last3_gens, Species == i)
  
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
        labs(x= "Year", y = "log(Spawners)", 
             title = paste0("log ", i, " spawner abundance ", "(", j, " of ", pages, ")")) +
        theme_bw() +
        theme(strip.background = element_blank())
      
      print(p_log)
      
      if(save_plots){
        ggsave(paste0("output/plots/","sp_abundance_", i, "_", j, ".png"), plot = p)
        ggsave(paste0("output/plots/","log_sp_lm_", i, "_", j, ".png"), plot = p_log)
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
      stat_smooth(data = sub_sub_last3, formula = y~x, method = "lm", color = "red", se = FALSE,
                  na.rm = TRUE) +
      facet_wrap(~du_cu, scales = "free_y") +
      labs(x= "Year", y = "log(Spawners)", 
           title = paste0("log ", i, "spawner abundance ", "(", j, " of ", pages, ")")) +
      theme_bw() +
      theme(strip.background = element_blank())
    
    print(p_log)
    
    if(save_plots){
      ggsave(paste0("output/plots/", "sp_abundance_", i, ".png"), plot = p)
      ggsave(paste0("output/plots/","log_sp_lm_", i, ".png"), plot = p_log)
    }
  }
}

# summarise % change with multiple methods ------------------------------------------

#determine what data deficient is, then filter out pops that don't have enough data over
  #the past 3 gens. 

#incorporate pop specific age@ mat to index the last 3 generations

#slope of lm from the last 15 years, slope from the entire timeseries, slope HPD from Stan,  
#recent abundance, last year of data, n data for last 3 generations (as a %?)


#dig up the rules for how steep the slope needs to be for the pop to fit into a given 
  #COSEWIC category, then write these probabilities out. 

#plot HPD over these categories too in a visualization - perhaps rank the plotting order
  #of importance by pop. 