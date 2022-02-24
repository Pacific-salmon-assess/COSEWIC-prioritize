# code to read in, process, plot and calculate % change for Skeena and Nass sockeye 
# data are from Charmaine Carr-Harris (Sockeye; DFO)
# generation length is assumed to be 5-years for sockeye

#### load required libraries ----
library(tidyverse)
library(zoo)

#### load and wrangle data ----
skeena_sx_raw <- read.csv('data/TRTC-Results--Skeena-Nass.csv') # make sure values do not contain commas for thousands 
cu_du_decoder <- read.csv('data/skeena-du-cu-decoder.csv') # read in manually made decoder for mapping DUs to CUs
skeena_sx_raw[,1] <- gsub('.*_','', skeena_sx_raw[,1]) # remove "SX" from CU names

# merge datasets and filter
du_cu_skeena <- left_join(x = skeena_sx_raw,
                 y = cu_du_decoder,
                 by = "CU")

SkeenaNassSX <- du_cu_skeena %>%
  select(Year,TE,DU,CU,CU_Name) %>%
  drop_na(DU) 

# create a total wild Babine time series
SkeenaNassSX_Babine <- subset(SkeenaNassSX, CU_Name == "Babine-Early-Wild")
SkeenaNassSX_Babine$CU_Name <- "Babine-Total-Wild"
SkeenaNassSX_Babine$CU <- "L-21-02"
total_Babine_spw <- SkeenaNassSX %>% 
  filter(DU == 158) %>%
  group_by(Year) %>%
  mutate(total_TE = sum(TE)) %>%
  filter(CU_Name == "Babine-Early-Wild")
SkeenaNassSX_Babine$TE <- total_Babine_spw$total_TE
SkeenaNassSX <- rbind(SkeenaNassSX,SkeenaNassSX_Babine)

# create a combined DU number + name identifier 
SkeenaNassSX$du_cu <- paste(SkeenaNassSX$DU, SkeenaNassSX$CU_Name,sep = " ")

#### plots of spawners over time ----
# plot RAW spawner abundance over time by DU 
p <- ggplot(SkeenaNassSX, aes(x=Year,y=TE)) + 
      geom_line(color="grey")+
      geom_point()+
      facet_wrap(~du_cu,scales="free_y")+
      theme_bw()+
      xlab("Year") +
      ylab("Spawner abundance") +
      coord_cartesian(ylim=c(0,NA), xlim=c(1960,2019))+
      theme(text = element_text(size = 8),
            strip.background = element_blank())

jpeg("output/Skeena-Nass-sockeye-raw.jpeg", width = 10, height = 6, units = "in", res = 600)
print(p)
dev.off()

# plot LOG transformed and NOT smoothed spawner abundance over time by DU 
SkeenaNassSX_lm <- SkeenaNassSX %>% 
  group_by(du_cu) %>% 
  select(Year,TE,DU,du_cu) %>%
  mutate(LnSpw=log(TE))

SkeenaNassSX_lm_fit <- SkeenaNassSX_lm %>% 
  mutate(last_yr=last(Year))%>%
  filter(between(Year,2004,2019))

p <- ggplot(SkeenaNassSX_lm, aes(x=Year,y=LnSpw)) + 
  geom_line(color="grey")+
  geom_point()+
  facet_wrap(~du_cu,scales="free_y")+
  theme_bw()+
  xlab("Year") +
  ylab("ln spawner abundance") +
  coord_cartesian(xlim=c(1960,2019))+
  theme(text = element_text(size = 8),
        strip.background = element_blank())+
  geom_smooth(data=SkeenaNassSX_lm_fit, method='lm', formula= y~x)
  
jpeg("output/Skeena-Nass-sockeye-log-no-smooth.jpeg", width = 10, height = 6, units = "in", res = 600)
print(p)
dev.off()

#### calculate % change over time ----
# calculate % change over most recent three generations (based on all or just most recent three gens with data), 
# save in table with DU and recent generational mean spawner abundance

SkeenaNassSXFilter <- SkeenaNassSX %>% # drop DUs without data
  filter(DU != c(153),
         DU != c(168),
         DU != c(41)) 

summary_table <- matrix(NA,
                        nrow = length(unique(SkeenaNassSXFilter$du_cu)),
                        ncol = 5) # DU, %change (recent years), % change (all years), recent abundance, last year with data

summary_table[,1] <- unique(SkeenaNassSXFilter$du_cu)

for(i in unique(SkeenaNassSXFilter$du_cu)){
  data.in <- subset(SkeenaNassSXFilter, du_cu== i)
  data.in$LnSpw <- log(data.in$TE)
  lst_index_yr <- last(which(!is.na(data.in$LnSpw)))
  first_index_yr <- lst_index_yr-15
  first_index_yr <- ifelse(first_index_yr < 0, 1,first_index_yr)
  model <- lm(data.in$LnSpw[first_index_yr:lst_index_yr]  ~ data.in$Year[first_index_yr:lst_index_yr])
  model_all <- lm(data.in$LnSpw  ~ data.in$Year)
  DU_index <- which(unique(SkeenaNassSXFilter$du_cu)==i)
  summary_table[DU_index,2] <- round(as.numeric((exp(model$coefficients[2]*15)-1)*100))
  summary_table[DU_index,3] <- round(as.numeric((exp(model_all$coefficients[2]*15)-1)*100))
  summary_table[DU_index,4] <- round(exp(last(na.omit(data.in$LnSpw))))
  summary_table[DU_index,5] <- data.in$Year[lst_index_yr]
}

colnames(summary_table) <- c("DU", "% change (recent)", "% change (all)", "recent abundance", "last year with data")

# export table as csv
write.csv(summary_table, "output/skeena_nass_sx_no_smooth.perChange.csv", row.names = FALSE)

