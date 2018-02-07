#This code is to analyze SET data
#Goal is to get a rate of cumulative elevation change for 
#1.  each SET 2. Sitewide
#and 3. make plots to show the regressions and changes by SET and 4. by site

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

####Data Source and Loading####
#data is from raw_set_data.csv in this project folder
set<-read.csv(file = "data/CBL_SET.csv")
View(set)
#When the data is uploaded like this, set_name, reading_date, read_by = factor; 
#position, pin_number, measurement  = integer

#Data Manipulations
set$site<-as.factor(set$site)
set$position<-as.factor(set$position)
set$pin_number<-as.factor(set$pin_number)
set$reading_date<-mdy(set$reading_date) #if your date data is in a different format look at the lubridate package to figure out what code you need
set$pinid<-as.factor(paste(set$set_name, set$position, set$pin_number, sep = "_"))
groan()
View(set)

#create a timestep value
settime<-set %>%
  select(set_name, reading_date) %>% 
  distinct()
settime<-settime %>% 
  group_by(set_name) %>% 
  mutate(timestep = dplyr::row_number(reading_date)) %>% 
  mutate(timestep = timestep-1) %>% 
  mutate(timestep = as.factor(paste("t", timestep, sep = "")))
set<-set %>% 
  left_join(settime, by = c("set_name", "reading_date"))
set$timestep<-as.factor(set$timestep)
View(set)

####Analysis####

#Goal 1 & 2. Rate of Cumulative elevation change by SET and Site
### This calculates CUMULATIVE changes.  If you want interval changes, all you need to do is change the columns
### In the select functions below


pin_tibble<- set %>%
  group_by(pinid) %>% 
  arrange(pinid, timestep) %>% 
  nest(timestep, measurement) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$measurement, 2)[2,]- combn(.x$measurement, 2)[1,])))%>% 
  unnest() %>%  
  spread(key, value)
View(pin_tibble)## You may get a warning here about binding character and factor vectors.
#####This appears when you have more timesteps in one set/site than others.  It wants you to check to make sure
#####it didn't do a bad thing by treating factors as characters

###join with some of the columns from the set dataframe  
set_join<- set%>%
  select(site, set_name, position, pin_number, pinid) %>% 
  right_join(pin_tibble, by="pinid")
View(set_join)

###pull out just measurements that you need
#####This part needs a little changing each time you run the code because You'll have to add one more 
#####column for the newest date in the mutate function, however, by clicking tab inside the 
#####mutate function, it should call up the different column headings, making it easier to find the ones you want
levels(set$timestep)###This tells you how many timesteps you have and that you need to adjust to in the next step

set_join<-set_join %>% 
  select(site, set_name, position, pin_number, pinid, t0_t1,t0_t2, t0_t3, t0_t4, t0_t5, t0_t6) %>% 
  distinct()
View(set_join)
###average by set (if you want to average by set and position, you can add position into the group_by function)
set_average<-set_join %>% 
  group_by(set_name)%>%
  summarise_if(is.numeric, mean, na.rm=TRUE)
  
groan()
View(set_average)



set_se<-set_join %>% 
  group_by(set_name)%>%
  summarise_if(is.numeric, std.error, na.rm=TRUE)
View(set_se)
### average by site
site_average<-set_join %>% 
  group_by(site) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE)
View(site_average)
site_se<-set_join %>% 
  group_by(site)%>%
  summarise_if(is.numeric, std.error, na.rm=TRUE)
View(site_se)

#### Calculating time between timesteps####
##### As you add timesteps you will need to change the code in the select function to reflect that
time_tibble<- set %>%
  group_by(set_name) %>% 
  arrange(set_name, timestep) %>%
  nest(timestep, reading_date) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$reading_date, 2)[2,]- combn(.x$reading_date, 2)[1,]))) %>% 
  unnest() %>% 
  distinct() %>% 
  spread(key, value) %>% 
  select(set_name, t0_t1,t0_t2, t0_t3, t0_t4, t0_t5, t0_t6)
### you may get warnings again if your timesteps are of different lengths.
View(time_tibble)

time_site<- set %>%
  group_by(site) %>% 
  arrange(site, timestep) %>%
  nest(timestep, reading_date) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$reading_date, 2)[2,]- combn(.x$reading_date, 2)[1,]))) %>% 
  unnest() %>% 
  distinct() %>% 
  spread(key, value) %>% 
  select(site, t0_t1,t0_t2, t0_t3, t0_t4, t0_t5, t0_t6)
View(time_site)

#### Getting the data ready for plotting and linear regressions for rate calculations####
###by SET
set_graph<-set_average %>%
  gather(variable, value, -set_name) %>% 
  left_join(., set_se %>% gather(variable, value, -set_name), by=c('set_name', 'variable'))%>%
  left_join(., time_tibble %>% gather(variable, value, -set_name), by=c('set_name', 'variable')) %>% 
  transmute(set_name, variable, cum_elev=value.x, std_err=value.y, days= value)

####Make Baseline data####
baseline<-data.frame(levels(set_graph$set_name))
View(baseline)
baseline<-baseline %>% 
  rename(set_name = levels.set_graph.set_name.)

baseline$variable<-"t0"
baseline$cum_elev<-0
baseline$std_err<-0
baseline$days<-0
View(baseline)
####add baseline to rest of data
set_graph<-set_graph %>% 
  bind_rows(baseline)
View(set_graph)
set_graph<-set %>%
  select(site, set_name) %>% 
  left_join(., set_graph, by="set_name") %>% 
  na.omit() %>% 
  distinct()
View(set_graph)
groan()
write.csv(set_graph, file = "set_av_timesteps.csv")

### by Site
site_graph<-site_average %>%
  gather(variable, value, -site) %>% 
  left_join(., site_se %>% gather(variable, value, -site), by=c('site', 'variable'))%>%
  left_join(., time_site %>% gather(variable, value, -site), by=c('site', 'variable')) %>% 
  transmute(site, variable, cum_elev=value.x, std_err=value.y, days= value) %>% 
  na.omit()
View(site_graph)

sitebase<-data.frame(levels(site_graph$site))
sitebase<- sitebase %>% 
  rename(site = levels.site_graph.site.)

sitebase$variable<-"t0"
sitebase$cum_elev<-0
sitebase$std_err<-0
sitebase$days<-0

site_graph<-site_graph %>% 
  bind_rows(sitebase)
View(site_graph)
write.csv(site_graph, file = "site_av_timesteps.csv")

#### Plot Cumulative Change####
### Graphical elements like specific colors or text styles that might be called multiple times
bold<-element_text(face = "bold")
my.formula<-y~x

###Subset by site then plot by Set
g<-set_graph %>%
  filter(site== "CovePoint") %>%
  ggplot()
### by SET
h<-g+geom_errorbar(aes(x=days, ymax=(cum_elev+std_err), ymin=(cum_elev-std_err), color=set_name),
                   width=20, color="grey56")+
  geom_line(aes(x=days, y=cum_elev, color=set_name), size=1)+
  geom_point(aes(x=days, y=cum_elev, color=set_name), size=3)+
  geom_smooth(method=lm, aes(x=days, y=cum_elev, linetype=set_name), fill=NA, color="black")+
  stat_poly_eq(formula = my.formula, aes(x=days, y=cum_elev, color=set_name, label = 
                                           paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE, label.y.npc = 1.1)+
  scale_color_ptol()+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color="grey80"),
        axis.text = bold, 
        axis.title = bold)+
  labs(bold)+
  guides(color=guide_legend(title="SET"), linetype=guide_legend(title="SET"))+
  ylab("Cumulative Elevation Change (mm)")+xlab("Days since Installation")+
  ggtitle("Cove Point Set Data (2014 - 2017)")
h
ggsave("cp_set.png")
groan()
#Linear Regression and stats
cp<-set_graph %>% 
  filter(site=="CovePoint")
cp_lm<-lm(cum_elev~days, data=cp)
summary(cp_lm)

### by Site
f<-site_graph %>% 
  filter(site=="CovePoint") %>% 
  ggplot()

e<-f+geom_errorbar(aes(x=days, ymax=(cum_elev+std_err), ymin=(cum_elev-std_err)), color="black")+
  geom_line(aes(x=days, y=cum_elev), size=1, color="darkturquoise")+
  geom_point(aes(x=days, y=cum_elev), size=3, color="darkturquoise")+
  geom_smooth(method=lm, aes(x=days, y=cum_elev), fill=NA, color="black", linetype= "dashed")+
  stat_poly_eq(formula = my.formula, aes(x=days, y=cum_elev, label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color="grey76"),
        axis.text = bold, 
        axis.title = bold)+
  ylab("Cumulative Elevation Change (mm)")+xlab("Days since Installation")+
  ggtitle("Cove Point Average Set Data (2014-2017)")
e
ggsave("cp_site.png")
