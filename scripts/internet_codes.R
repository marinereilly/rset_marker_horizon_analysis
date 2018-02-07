#messing around with StackOverflow solutions

library(dadjoke)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(stringr)
library(purrr)

#Data Source and Loading
#data is from raw_set_data.csv in this project folder
set<-read.csv(file = "raw_set_data.csv")
View(set)
#When the data is uploaded like this, set_name, reading_date, read_by = factor; 
#position, pin_number, measurement  = integer

#Data Manipulations
set$position<-as.factor(set$position)
set$pin_number<-as.factor(set$pin_number)
set$reading_date<-mdy(set$reading_date)
set$pinid<-as.factor(paste(set$set_name, set$position, set$pin_number, sep = "_"))
set$timestep<-as.factor(set$timestep)
View(set)

#load in fake data
data<-structure(list(pinid = structure(c(1L, 2L, 1L, 2L, 1L, 2L), .Label 
                                       = c("CP_South_1_1", "CP_South_1_2"), class = "factor"), reading_date = 
                       structure(c(16308, 16308, 16531, 16531, 16728, 16728), class = "Date"), 
                     timestep = c("t0", "t0", "t1", "t1", "t2", "t2"), measurement = c(189, 
                                                                                       186, 187, 185, 184, 181)), .Names = c("pinid", "reading_date", 
                                                                                                                             "timestep", "measurement"), row.names = c(NA, -6L), class = 
                  "data.frame")



#Analysis
#this step creates a new data table with just pinid and spreads the timesteps out
data<- data %>% as.data.table()
data<-data %>% dcast.data.table(formula = pinid~t%>% mestep, value.var = "measurement")
data2<- data %>% copy()
View(data)

set_sub<-set %>% as.data.table()
set_sub<-set_sub %>% dcast.data.table(formula = pinid~t%>% mestep, value.var = "measurement")
ss_2<-set_sub %>% copy()
View(set_sub)
#this step creates the list of names/combinations you want
comb<- expand.grid(names(data[,2:ncol(data)]), names(data[,2:ncol(data)])) %>% as.data.table()
comb<- comb[Var2 %>% as.character() > Var1 %>% as.character()][,var3:=paste(Var1, Var2, sep = "_")]
View(comb)


timecomb<-expand.grid(names(set_sub[,2:ncol(set_sub)]), names(set_sub[,2:ncol(set_sub)])) %>% as.data.table()
timecomb<-timecomb[Var2 %>% as.character()>Var1 %>% as.character()][,var3:=paste(Var1, Var2, sep = "_")]
View(timecomb)

#for loop for actually subracting!
for(i in comb$var3){
  data2[,(i) := get(word(string = i, start = 2, sep = "_")) - get(word(string = i, start = 1, sep = "_"))]
}
names_vars<-names(data[,2:ncol(data)])
data2<- data2[,!names_vars, with =F]
View(data2)

for(i in timecomb$var3){
  ss_2[,(i) := get(word(string = i, start = 2, sep = "_")) - get(word(string = i, start = 1, sep = "_"))]
}
names_vars<-names(set_sub[,2:ncol(set_sub)])
ss_2<- ss_2[,!names_vars, with =F]
View(ss_2)

#End Internet Code 1 - will need to filter and then join with the other data from 
#the original table but I think it works pretty well!

#Code 2
datatibble<- data %>%
  group_by(pinid) %>% 
  arrange( pinid, timestep) %>% 
  nest(timestep, measurement) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                   value = combn(.x$measurement, 2)[2,]- combn(.x$measurement, 2)[1,]))) %>% 
  unnest()
View(datatibble)
# I see spots where it makes sense to stop here 
#and use the pintibble vs the next step but the step below makes it look prettier
datatibble2<-datatibble %>% 
  spread(key, value)
View(datatibble2)

#with more data!
pintibble<- set %>%
  group_by(pinid) %>% 
  arrange( pinid, timestep) %>% 
  nest(timestep, measurement) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$measurement, 2)[2,]- combn(.x$measurement, 2)[1,]))) %>% 
  unnest()
View(pintibble)
# I see spots where it makes sense to stop here 
#and use the pintibble vs the next step but the step below makes it look prettier
pintibble2<-pintibble %>% 
  spread(key, value)
View(pintibble2)
