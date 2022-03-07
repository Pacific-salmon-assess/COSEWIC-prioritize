# code to read in, process, plot and calculate % change for Skeena and Nass sockeye 
# data are from Charmaine Carr-Harris (Sockeye; DFO)
# generation length is assumed to be 5-years for sockeye
setwd('Skeena-example')

# load packages, data and reshape ---------------------------------------------------
library(tidyverse)

skeena_sx_raw <- read.csv('data/TRTC-Results--Skeena-Nass.csv') %>%
  mutate(CU = gsub('.*_','', CU)) #remove "SX" from CU names

#read in manually made decoder for mapping DUs to CUs
cu_du_decoder <- read.csv('data/skeena-du-cu-decoder.csv')

SkeenaNassSX <- left_join(skeena_sx_raw, cu_du_decoder, by = "CU") %>%
  select(Year,TE,DU,CU,CU_Name) %>%
  drop_na(DU) 

rm(skeena_sx_raw, cu_du_decoder) #garbage out

#create a total wild Babine time series - so confused what's going on between here and plots!
SkeenaNassSX_Babine <- filter(SkeenaNassSX, CU_Name == "Babine-Early-Wild") %>% 
  mutate(CU_Name = "Babine-Total-Wild", #why take early only then overwrite?
         CU = "L-21-02")

total_Babine_spw <- SkeenaNassSX %>% 
  filter(DU == 158) %>%
  group_by(Year) %>%
  mutate(total_TE = sum(TE)) %>%
  filter(CU_Name == "Babine-Early-Wild")
SkeenaNassSX_Babine$TE <- total_Babine_spw$total_TE

SkeenaNassSX <- rbind(SkeenaNassSX,SkeenaNassSX_Babine) %>%
  mutate(du_cu = paste(DU, CU_Name), 
         LnSpw=log(TE))

#### plots of spawners over time ----
# plot RAW spawner abundance over time by DU 
ggplot(SkeenaNassSX, aes(x=Year,y=TE)) +
  geom_line(color="grey")+
  geom_point(na.rm = TRUE)+
  facet_wrap(~du_cu,scales="free_y")+
  theme_bw()+
  labs(x = "Year", y = "Spawner abundance") +
  coord_cartesian(ylim=c(0,NA), xlim=c(1960,2019))+
  theme(text = element_text(size = 8),
        strip.background = element_blank())

ggsave("output/Skeena-Nass-sockeye-raw.jpeg", width = 10, height = 6, units = "in")

# plot LOG transformed and NOT smoothed spawner abundance over time by DU 
SkeenaNassSX_lm_fit <- SkeenaNassSX %>% 
  group_by(du_cu) %>% 
  select(Year,LnSpw,DU,du_cu) %>%
  mutate(last_yr=last(Year))%>% 
  filter(between(Year,2004,2019)) ##note this isn't explicitly the last year WITH DATA! 
                                    #it needs to "float" more

ggplot(SkeenaNassSX, aes(x=Year,y=LnSpw)) + 
  geom_line(color="grey") +
  geom_point(na.rm = TRUE) +
  facet_wrap(~du_cu, scales="free_y") +
  theme_bw() +
  labs(x = "Year", y = "ln spawner abundance") +
  coord_cartesian(xlim=c(1960,2019)) +
  theme(text = element_text(size = 8),
        strip.background = element_blank()) +
  geom_smooth(data=SkeenaNassSX_lm_fit, method='lm', se = FALSE, color = "red", 
              na.rm = TRUE)
  
ggsave("output/Skeena-Nass-sockeye-log-no-smooth.jpeg", width = 10, 
       height = 6, units = "in")


#### calculate % change over time ----
# calculate % change over most recent three generations (based on all or just most recent three gens with data), 
# save in table with DU and recent generational mean spawner abundance

SkeenaNassSXFilter <- filter(SkeenaNassSX, !(DU %in% c(153, 168, 41))) #drop DUs w/o data

#hol up - this should be robust, check it out with this chunk to make a rule:
if(TRUE){
  #need a helper fr next chunk
  yr_helper <- SkeenaNassSX %>%
    filter(!is.na(TE)) %>%
    group_by(du_cu) %>%
    filter(Year == max(Year)) %>%
    rename(max_yr = Year) %>%
    select(du_cu, max_yr)
  
  #how many of the last 15 years have escapement data? 
  data_check <- left_join(SkeenaNassSX, yr_helper, by = "du_cu") %>%
    group_by(du_cu) %>%
    #what is the most recent year with data and the last 15 years 
    filter(Year > (max_yr-15) & Year <= max_yr) %>% 
    summarise(check_yrs = n(),
           no_dat = sum(is.na(TE))) %>%
    #label the pops brendan removed 
    mutate(data_quality = ifelse(du_cu %in% c("153 Johnston", "168 Bear", 
                                          "41 Skeena River-high interior"), 
                                 "deficient", "OK")) 
  #ok so it appears pops where there are 4+ NAs in the last 15 years get booted. 
}

summary_table <- matrix(NA,
                        nrow = length(unique(SkeenaNassSXFilter$du_cu)),
                        ncol = 5) # DU, %change (recent years), % change (all years), recent abundance, last year with data

summary_table[,1] <- unique(SkeenaNassSXFilter$du_cu)

for(i in unique(SkeenaNassSXFilter$du_cu)){
  data.in <- subset(SkeenaNassSXFilter, du_cu== i)
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


#dylan way - well this isn't any sexier...
summary_table <- NULL
for(i in unique(SkeenaNassSXFilter$du_cu)){
  data.in <- filter(SkeenaNassSXFilter, du_cu== i)
  lst_index_yr <- last(which(!is.na(data.in$LnSpw)))
  first_index_yr <- lst_index_yr-15
  first_index_yr <- ifelse(first_index_yr < 0, 1,first_index_yr) #what's a case where this line could be important??
  model <- lm(data.in$LnSpw[first_index_yr:lst_index_yr]  ~ data.in$Year[first_index_yr:lst_index_yr])
  model_all <- lm(data.in$LnSpw  ~ data.in$Year)

  summary_table <- bind_rows(summary_table, 
                             data.frame(i, (exp(model$coefficients[2]*15)-1)*100,
                             (exp(model_all$coefficients[2]*15)-1)*100,
                             exp(last(na.omit(data.in$LnSpw))), 
                             data.in$Year[lst_index_yr]))
}

colnames(summary_table) <- c("DU", "% change (recent)", "% change (all)", "recent abundance", "last year with data")
rownames(summary_table) <- NULL

summary_table <- summary_table %>%
  #mutate_at(2:3, round(2)) #do this better 
