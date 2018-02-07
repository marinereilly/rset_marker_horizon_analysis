####Packages to Load####
library(dadjoke)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(plotrix)
library(ggthemes)
library(ggpmisc)

####Calculate Shallow Subsidence####
##Make sure you have already dont the MH_analysis scripts and the SET_analysis scripts

####By SET####
View(set_graph)
View(mh_graph2_b)

ss<-set_graph %>% 
  full_join(., mh_graph2_b, by = c('site', 'set_name', 'days'))
View(ss)
