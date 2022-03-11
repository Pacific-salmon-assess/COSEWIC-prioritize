#multifit example from the MetricsCOSEWIC package straight out of the readme at 
  #https://github.com/SOLV-Code/MetricsCOSEWIC
library(MetricsCOSEWIC)


library(dplyr) 
library(tidyr) 

data.in <- SR_Sample %>% 
  select(Stock, Year, Spn) %>% 
  rename(DU=Stock, 
         Abd = Spn)

head(data.in)

write.csv(data.in,"tmp.csv")

window.in <- data.frame(DU = unique(data.in$DU),Window = 13)

multi.out <- multiFit(data.df = data.in, window.df = window.in, plot.file =  "Test_PercChange_Plots.pdf")
 #^ BROKEN!

#write.csv(multi.out$Output,"Test_Outputs.csv",row.names = FALSE)
#write.csv(multi.out$Summary,"Test_Summary.csv",row.names = FALSE)

#ok well that's busted so I'll try this  

stk <- "Stock3"
gen <- 4
yrs.do <- (3 * gen) +1
calc.yr <- 2017

test.df <- SR_Sample %>%
  dplyr::filter(Stock == stk) %>%
  select(Year,Spn)
head(test.df)

test.df.sub <- test.df %>% dplyr::filter(Year > calc.yr - yrs.do )
test.df.sub

fit.out <- comparePercChange(du.label = stk,
                             du.df = test.df,
                             yrs.window = yrs.do ,
                             calc.yr = 2017,
                             samples.out = TRUE,
                             plot.pattern = TRUE,
                             plot.posteriors = TRUE,
                             plot.boxes  = TRUE)
  #^WOW! that's busted too!

names(fit.out)
fit.out$Summary
