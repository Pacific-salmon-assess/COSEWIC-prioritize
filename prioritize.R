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
sp_data <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                           erics_output[grep("CU_Spawner_Abund", erics_output)]), 
                    header = TRUE) %>%
  mutate(Spawner.Abundance = as.numeric(gsub(",", "", Spawner.Abundance)), 
         logSp = log(Spawner.Abundance))

cu_metadata <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                               erics_output[grep("CU_Metadata", erics_output)]), 
                        header = TRUE)


#if I were to copy brendan's example then I'd paste DU and CU together...
  #should I paste sp_data$CUID with cu_metadata$du_number??

#do some checks###
#species levels don't really lineup. do we care? 
unique(cu_metadata$spp)
unique(sp_data$Species) 

#do years overlap?
cu_yrs <- sp_data %>%
  group_by(CUID) %>%
  summarise(yrs = n(), 
            start_yr = min(Year), 
            end_yr = max(Year))
  #yea looks good - all start at 1950 and some end 2017-2020

# plots -----------------------------------------------------------------------------
max_facets <- 20 #how many facets per plot?
save_plots <- TRUE #toggle if you want to save plots 


#ugly indexing makes for less cluttered plots 
for(i in unique(sp_data$Species)){
  sub_data <- filter(sp_data, Species == i) %>%
    na.omit(Spawner.Abundance)
  
  if(length(unique(sub_data$CUID)) > max_facets){
    pages <- ceiling(length(unique(sub_data$CUID))/max_facets)
    
    for(j in 1:pages){
      
      sub_sub_data <- filter(sub_data, 
                             CUID %in% 
                               unique(sub_data$CUID)[((j-1)*max_facets)+1:(max_facets*j)])
      
      p <- ggplot(data = sub_sub_data, aes(x= Year, y= Spawner.Abundance/1000)) +
        geom_line(color = "grey") +
        geom_point() +
        facet_wrap(~CUID, scales = "free_y") +
        labs(x= "Year", y = "Spawners (thousands)", 
             title = paste0(i, " spawner abundance ", "(", j, " of ", pages, ")"))
      
      print(p)
      if(save_plots){
        ggsave(paste0("output/plots/", i, "_sp_abundance_", j, ".png"), plot = last_plot())
      }
    }
  }else{
    p <- ggplot(data = sub_data, aes(x= Year, y= Spawner.Abundance/1000)) +
      geom_line(color = "grey") +
      geom_point() +
      facet_wrap(~CUID, scales = "free_y") +
      labs(x= "Year", y = "Spawners (thousands)", title = paste(i, "spawner abundance"))
    
    print(p)
    if(save_plots){
      ggsave(paste0("output/plots/", i, "sp_abundance", ".png"), plot = last_plot())
    }
  }
}

#lm last 3 gens. 

#lm all

#smoothed 
  #^ should I just overlay these 3 methods on 1 fig per pop?


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