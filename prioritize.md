Rapid assessment of Pacific Salmon spawner abundnace to aide with
COSEWIC prioritizaiton
================
Dylan Glaser (DFO)

## Background

The purpose of this document is to import recent data on pacific salmon
spawner abundances, and their designateable and conservation units
(i.e. DUs and CUs). We plot abundances and examine trends, then explore
various metrics to prioritize populations for assessment.

### Load and wrangle data

We want to load the most recent data from a [GitHub
repo](https://github.com/hertzPSF/COSEWIC-compilation) managed by Eric
Hertz at the Pacific Salmon Foundation (PSF). We download the repo,
import data we want from it, and tidy it up.

``` r
download.file(url="https://github.com/hertzPSF/COSEWIC-compilation/archive/master.zip",
              destfile="compilation/hertzPSF-compilation.zip")

unzip(zipfile="compilation/hertzPSF-compilation.zip", exdir="compilation")

erics_output <- list.files("compilation/COSEWIC-compilation-main/output")

raw_sp_data <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                           erics_output[grep("CU_Spawner_Abund", erics_output)]), 
                        header=TRUE) %>%
         mutate(logSp=log(Spawner.Abundance))

cu_metadata <- read.csv(paste0("compilation/COSEWIC-compilation-main/Output/",
                               erics_output[grep("CU_Metadata", erics_output)]), 
                        header=TRUE, na.strings=c("", "NA")) %>%
  filter(!is.na(gen_length)) %>%
  rename(CUID=cuid)

#join, dump pops with NA DUs, and create a du_cu key
sp_data <- left_join(raw_sp_data, select(cu_metadata, CUID, du_number, gen_length), 
                     by="CUID") %>%
  drop_na(du_number, Spawner.Abundance) %>%
  mutate(du_cu=paste(du_number, CUID)) %>%
  relocate(du_cu, 1) %>%
  arrange(du_cu, Year)
```

#### Get the last 3 generations of spawner abundances

We need to use the metadata to get generation times for each population,
then use that to calculate a time span that includes the last 3
generations, as per IUCN guidelines.

``` r
last3_gens <- sp_data %>%
  group_by(du_cu) %>%
  mutate(three_gens = gen_length*3) %>%
  filter(Year > (last(Year) - three_gens), Year <= last(Year))
```

## Plot spawner abundance

We’ll make 2 plots for each species and region: raw spawner abundance
and ln-transformed spawner abundance with a linear regression passing
through all data points (black) and the last 3 generations (red). **Note
that that the y axis varies for ease of viewing**.  
This code is cumbersome in order to make it robust, so we’ve omitted it
from the knit, but it’s worth noting you can adjust facets, and these
options cascade into the posterior plots as well.

``` r
max_facets <- 20 #how many facets (i.e. panes) per plot?
rows <- 4 #how many rows/cols? note needs to be multiple of facets. 
cols <- 5
```

![](output/plots/abundance%20plots-1.png)<!-- -->![](output/plots/abundance%20plots-2.png)<!-- -->![](output/plots/abundance%20plots-3.png)<!-- -->![](output/plots/abundance%20plots-4.png)<!-- -->![](output/plots/abundance%20plots-5.png)<!-- -->![](output/plots/abundance%20plots-6.png)<!-- -->![](output/plots/abundance%20plots-7.png)<!-- -->![](output/plots/abundance%20plots-8.png)<!-- -->![](output/plots/abundance%20plots-9.png)<!-- -->![](output/plots/abundance%20plots-10.png)<!-- -->![](output/plots/abundance%20plots-11.png)<!-- -->![](output/plots/abundance%20plots-12.png)<!-- -->![](output/plots/abundance%20plots-13.png)<!-- -->![](output/plots/abundance%20plots-14.png)<!-- -->![](output/plots/abundance%20plots-15.png)<!-- -->![](output/plots/abundance%20plots-16.png)<!-- -->![](output/plots/abundance%20plots-17.png)<!-- -->![](output/plots/abundance%20plots-18.png)<!-- -->![](output/plots/abundance%20plots-19.png)<!-- -->![](output/plots/abundance%20plots-20.png)<!-- -->![](output/plots/abundance%20plots-21.png)<!-- -->![](output/plots/abundance%20plots-22.png)<!-- -->![](output/plots/abundance%20plots-23.png)<!-- -->![](output/plots/abundance%20plots-24.png)<!-- -->![](output/plots/abundance%20plots-25.png)<!-- -->![](output/plots/abundance%20plots-26.png)<!-- -->![](output/plots/abundance%20plots-27.png)<!-- -->![](output/plots/abundance%20plots-28.png)<!-- -->![](output/plots/abundance%20plots-29.png)<!-- -->![](output/plots/abundance%20plots-30.png)<!-- -->

## Linear models of population decline

#### Which populations are data deficient?

Define a threshold of acceptable data coverage then check the get a list
of DUs that don’t meet this threshold and filter them out of data that
will be used in the lms.  
First we’ll do this for the recent data.

``` r
deficient_recent_perc <- 0.7

deficient_recent <- last3_gens %>%
  group_by(du_cu) %>%
  summarise(n = n(), #years with data  
            n_proper = mean(gen_length)*3) %>% #years for 3 gens of data if fully sampled
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_recent_perc) %>%
  pull(du_cu)

last3_gens_filtered <- filter(last3_gens, !(du_cu %in% deficient_recent))
```

Then we’ll use similar logic but account for the whole time series using
a more forgiving cutoff and the first year of sampling.

``` r
deficient_historical_perc <- 0.6

deficient_historical <- sp_data %>%
  group_by(du_cu) %>%
  summarise(n = n(),
            n_proper = last(Year) - first(Year) + 1) %>%
  mutate(perc_na = n/n_proper) %>%
  filter(perc_na < deficient_historical_perc) %>%
  pull(du_cu)

sp_data_filtered <- filter(sp_data, !(du_cu %in% deficient_historical))
```

*we could add to this by making it specific to gen_length* (e.g. 3
missing data points from a short lived pop may have more of an impact on
our inference than 3 NAs in longer lived pops…) ## Population decline
### Basic linear models Now we’ll run basic linear models on ln(spawner
abundances) for the whole time series and for the last 3 generations. We
need to add some if statements in this loop to account for which du_cus
made it through the data filters above.  
**I’m open to clever suggestions to avoid these if else statements!**

``` r
summary_table <- NULL

for(i in unique(sp_data_filtered$du_cu)){
  sub_raw <- filter(sp_data, du_cu==i)
  sub_data <- filter(sp_data_filtered, du_cu==i)
  sub_last3 <- filter(last3_gens_filtered, du_cu==i)
  if(nrow(sub_data)==0){
    slope_all <- NA
  } else{
    if(nrow(sub_last3)==0){
      slope_recent <- NA
    } else{
      model_all <- lm(sub_data$logSp~sub_data$Year)
      model_recent <- lm(sub_last3$logSp~sub_last3$Year)
      slope_all <- round(as.numeric((exp(model_all$coefficients[2]*15)-1)*100))
      slope_recent <- round(as.numeric((exp(model_recent$coefficients[2]*15)-1)*100))
    }
  }
  recent_abundance <- last(sub_raw$Spawner.Abundance)
  last_year_monitored <- last(sub_raw$Year)
  
  output <- data.frame(du_cu=i, slope_recent, slope_all, recent_abundance, last_year_monitored)
  
  summary_table <- bind_rows(output, summary_table)
}

head(summary_table)
```

    ##       du_cu slope_recent slope_all recent_abundance last_year_monitored
    ## 1 SER42 426          613       146            14563                2017
    ## 2 SER41 216           99       133               91                2017
    ## 3 SER24 745          -19        54            49364                2017
    ## 4 SER23 742          114       -28              472                2016
    ## 5   SE9 713          -46       123             6650                2016
    ## 6   SE8 727          332       111             2128                2017

### Estimates with the MetricsCOSEWIC package

First some housekeeping. You can use devtools to download the
[MetricsCOSEWIC package](https://github.com/SOLV-Code/MetricsCOSEWIC)
from GitHub. We’ll wrangle the data into the format this package wants
as well. We’ll feed it the filtered last 3 generations. **note these are
preliminary, and the MetricsCOSEWIC package is still in development**
*Still need to make sure I did this right. The README says the first
argument to calcPercChangeMCMC() is a “vector with numeric values” so
I’ll assume that is spawner abundance with NAs omitted?*

``` r
library(MetricsCOSEWIC)
slope_posterior <- NULL
stan_summary <- NULL

for(i in unique(last3_gens_filtered$du_cu)){
  sub_data <- filter(last3_gens_filtered, du_cu == i)
  stan_mod <- calcPercChangeMCMC(sub_data$Spawner.Abundance, method="jags", out.type="long") 
  
  summary <- data.frame(stan_mod$summary) %>%
    slice(1:3) %>%
    select(mean, sd, X2.5., X50., X97.5., n.eff, Rhat) %>%
    rename(perc_2.5 = X2.5., 
           perc_97.5 = X97.5.,
           median = X50.) %>%
    mutate(du_cu = i,
           region = sub_data$Species[1],
           species = sub_data$Region[1]) %>%
    relocate(du_cu, region, species, .before = mean) 
  
  rownames(summary) <- NULL #rownames to columns - probably a better way
  summary$var <- c("intercept", "slope", "sigma")

  stan_summary <- bind_rows(summary, stan_summary)

  slope_posterior <- bind_rows(data.frame(du_cu =i, region = unique(sub_data$Region),
                                          species = unique(sub_data$Species),
                                          draw = stan_mod$samples$slope), slope_posterior)
}

stan_summary <- relocate(stan_summary, var, .before = mean)
```

We’ll also create an object that summarizes how many draws are in each
category. This will be a go-to summary table for COSEWIC thresholds for
population decline based on [category
A2](https://www.canada.ca/en/environment-climate-change/services/species-risk-act-accord-funding/listing-process/quantitative-criteria-guidelines-status-table-2.html),
where 50 and 30 percent reductions correspond to endangered and
threatened status, respectively; we’ll use 30, 50, and 70 % reductions
as thresholds for least concern, threatened, and endangered for now.

``` r
perc_category <- slope_posterior %>%
  group_by(species, region, du_cu) %>%
  summarise(not_at_risk = round(sum(draw>(-30))/n()*100, 2),
            least_concern = round(sum((-30)>draw & draw>(-50))/n()*100, 2), 
            threatened = round(sum(draw<(-50) & draw>(-70))/n()*100, 2), 
            endangered = round(sum(draw<(-70))/n()*100, 2))
```

### Plot slope posterior distributions

![](output/plots/slope%20posteriors-1.png)<!-- -->![](output/plots/slope%20posteriors-2.png)<!-- -->![](output/plots/slope%20posteriors-3.png)<!-- -->![](output/plots/slope%20posteriors-4.png)<!-- -->![](output/plots/slope%20posteriors-5.png)<!-- -->![](output/plots/slope%20posteriors-6.png)<!-- -->![](output/plots/slope%20posteriors-7.png)<!-- -->![](output/plots/slope%20posteriors-8.png)<!-- -->![](output/plots/slope%20posteriors-9.png)<!-- -->![](output/plots/slope%20posteriors-10.png)<!-- -->![](output/plots/slope%20posteriors-11.png)<!-- -->![](output/plots/slope%20posteriors-12.png)<!-- -->![](output/plots/slope%20posteriors-13.png)<!-- -->![](output/plots/slope%20posteriors-14.png)<!-- -->![](output/plots/slope%20posteriors-15.png)<!-- -->
