#load packages
library('tidyverse')
#library(SOMETHING STAN?)
#devtools to rip from GitHub?? 

# read in and reshape data ----------------------------------------------------------

#rip straight from Eric's GitHub

#reshape


# plots -----------------------------------------------------------------------------

#raw

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