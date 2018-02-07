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

####load data####
mh<-read.csv("data/CBL_MH.csv")


####format data####
mh$date<-mdy(mh$date)
View(mh)

mh$id<-as.factor(paste(mh$set_name, mh$plot, sep = "_"))

#create a timestep Value
mh_time<-mh %>%
  select(set_name, date) %>% 
  distinct()
groan()
mh_time<-mh_time %>% 
  arrange(date) %>% 
  group_by(set_name) %>% 
  mutate(timestep = dplyr::row_number(date))%>% 
  mutate(timestep = timestep-1) %>% 
  mutate(timestep = as.factor(paste("t", timestep, sep = "")))
mh<-mh %>% 
  left_join(mh_time, by = c("set_name", "date"))
mh$timestep<-as.factor(mh$timestep)
View(mh)

levels(mh$timestep)

#####Analsis####
####Calculating Means and Standard Error for each Plot####
##You may get an error if your t0 timestep only has one 0 measurement because the standard error calc 
##pops out a NA for std error of one number
mh_av<-mh %>% 
  group_by(site, id, date) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
  rename(av_cum_acr=measurement)
mh_se<-mh %>% 
  group_by(site,id, date) %>% 
  summarise_if(is.numeric, std.error, na.rm=TRUE) %>% 
  rename(standard_error=measurement)
mh_join<-mh_av %>% 
  left_join(., mh_se)
View(mh_join)
#reattaching variables
mh2<- mh %>% 
  select(site, set_name, id, date, timestep) %>% 
  distinct()%>% 
  left_join(., mh_join)
View(mh2)
#calculating cumulative changes
mh3<-mh2 %>%
  group_by(site, set_name, id) %>% 
  arrange(id, timestep) %>% 
  nest(timestep, av_cum_acr) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$av_cum_acr, 2)[2,]- combn(.x$av_cum_acr, 2)[1,])))%>% 
  unnest() %>%  
  spread(key, value) %>% 
  select(id, t0_t1, t0_t2, t0_t3, t0_t4, t0_t5)
View(mh3)

time_mh<- mh2 %>%
  group_by(site, set_name, id) %>% 
  arrange(id, timestep) %>%
  nest(timestep, date) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$date, 2)[2,]- combn(.x$date, 2)[1,]))) %>% 
  unnest() %>% 
  distinct() %>% 
  spread(key, value) %>% 
  select(id, t0_t1,t0_t2, t0_t3, t0_t4, t0_t5)
View(time_mh)

mh_graph<-mh3 %>%
  gather(variable, value, -id) %>% 
  left_join(., time_mh %>% gather(variable, value, -id), by=c('id', 'variable'))%>% 
  transmute(id, time=variable, av_cum_acr=value.x, days=value.y)

mh_graph2<-mh2 %>% 
  select(site, set_name, id, av_cum_acr, standard_error)%>% 
  inner_join(., mh_graph, by =c('id', 'av_cum_acr')) 
  

mh_base<-mh %>% 
  filter(timestep=="t0") %>% 
  select(site, set_name, id, measurement, timestep) %>% 
  rename(av_cum_acr=measurement, time=timestep)
mh_base$days<-0


mh_graph2<-mh_graph2 %>% 
  bind_rows(mh_base)

####Graph by plot####
### Graphical elements like specific colors or text styles that might be called multiple times
bold<-element_text(face = "bold")
my.formula<-y~x

a<-mh_graph2 %>% 
  filter(site=="CovePoint") %>% 
  ggplot()
b<-a+
  geom_point(aes(x=days, y=av_cum_acr, color=id), size=2)+
  geom_smooth(method=lm, aes(x=days, y=av_cum_acr, color=id), fill=NA)+
  stat_poly_eq(formula = my.formula, aes(x=days, y=av_cum_acr, color=id, label = 
                                           paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE,
               label.y.npc = 1)+
  scale_color_ptol()+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color="grey80"),
        axis.text = bold, 
        axis.title = bold)+
  labs(bold)+
  guides(color=guide_legend(title="SET"), linetype=guide_legend(title="SET"))+
  ylab("Cumulative accretion (mm)")+xlab("Days since Installation")+
  ggtitle("Cove Point North Set Marker Horizon Data")
b

#### To look at each SET####

mh_av_b<-mh %>% 
  group_by(site, set_name, date) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
  rename(av_cum_acr=measurement)
mh_se_b<-mh %>% 
  group_by(site,set_name, date) %>% 
  summarise_if(is.numeric, std.error, na.rm=TRUE) %>% 
  rename(standard_error=measurement)
mh_join_b<-mh_av_b %>% 
  left_join(., mh_se_b)
View(mh_join_b)
#reattaching variables
mh2_b<- mh %>% 
  select(site, set_name, date, timestep) %>% 
  distinct()%>% 
  left_join(., mh_join_b)
View(mh2_b)
#calculating cumulative changes
mh3_b<-mh2_b %>%
  group_by(site, set_name) %>% 
  arrange(set_name, timestep) %>% 
  nest(timestep, av_cum_acr) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$av_cum_acr, 2)[2,]- combn(.x$av_cum_acr, 2)[1,])))%>% 
  unnest() %>%  
  spread(key, value) %>% 
  select(set_name, t0_t1, t0_t2, t0_t3, t0_t4, t0_t5)
View(mh3_b)

time_mh_b<- mh2_b %>%
  group_by(site, set_name) %>% 
  arrange(set_name, timestep) %>%
  nest(timestep, date) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$date, 2)[2,]- combn(.x$date, 2)[1,]))) %>% 
  unnest() %>% 
  distinct() %>% 
  spread(key, value) %>% 
  select(set_name, t0_t1,t0_t2, t0_t3, t0_t4, t0_t5)
View(time_mh_b)

mh_graph_b<-mh3_b %>%
  gather(variable, value, -set_name) %>% 
  left_join(., time_mh_b %>% gather(variable, value, -set_name), by=c('set_name', 'variable'))%>% 
  transmute(set_name, time=variable, av_cum_acr=value.x, days=value.y)

mh_graph2_b<-mh2_b %>% 
  select(site, set_name, av_cum_acr, standard_error)%>% 
  inner_join(., mh_graph_b, by =c('set_name', 'av_cum_acr')) 


mh_base_b<-mh %>% 
  filter(timestep=="t0") %>%
  select(site, set_name, measurement, timestep) %>% 
  distinct() %>%
  rename(av_cum_acr=measurement, time=timestep)
mh_base_b$days<-0


mh_graph2_b<-mh_graph2_b %>% 
  bind_rows(mh_base_b)

####Graph by Set####
a_b<-mh_graph2_b %>% 
  filter(site=="CovePoint") %>% 
  ggplot()
b_b<-a_b+
  geom_point(aes(x=days, y=av_cum_acr, color=set_name), size=2)+
  geom_smooth(method=lm, aes(x=days, y=av_cum_acr, color=set_name), fill=NA)+
  stat_poly_eq(formula = my.formula, aes(x=days, y=av_cum_acr, color=set_name, label = 
                                           paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE,
               label.y.npc = 1)+
  scale_color_ptol()+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color="grey80"),
        axis.text = bold, 
        axis.title = bold)+
  labs(bold)+
  guides(color=guide_legend(title="SET"), linetype=guide_legend(title="SET"))+
  ylab("Cumulative accretion (mm)")+xlab("Days since Installation")+
  ggtitle("Cove Point Marker Horizon Data")
b_b
ggsave("CP_MH.png")
####By Site####
mh_av_c<-mh %>% 
  group_by(site, date) %>% 
  summarise_if(is.numeric, mean, na.rm=TRUE) %>% 
  rename(av_cum_acr=measurement)
mh_se_c<-mh %>% 
  group_by(site, date) %>% 
  summarise_if(is.numeric, std.error, na.rm=TRUE) %>% 
  rename(standard_error=measurement)
mh_join_c<-mh_av_c %>% 
  left_join(., mh_se_c)
View(mh_join_c)
#reattaching variables
mh2_c<- mh %>% 
  select(site, date, timestep) %>% 
  distinct()%>% 
  left_join(., mh_join_c)
View(mh2_c)
#calculating cumulative changes
mh3_c<-mh2_c %>%
  group_by(site) %>% 
  arrange(site, timestep) %>% 
  nest(timestep, av_cum_acr) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$av_cum_acr, 2)[2,]- combn(.x$av_cum_acr, 2)[1,])))%>% 
  unnest() %>%  
  spread(key, value) %>% 
  select(site, t0_t1, t0_t2, t0_t3, t0_t4, t0_t5)
View(mh3_c)

time_mh_c<- mh2_c %>%
  group_by(site) %>% 
  arrange(site, timestep) %>%
  nest(timestep, date) %>% 
  mutate(data= map(data, ~data.frame(key=paste(combn(.x$timestep, 2)[1,], combn(.x$timestep,2)[2,], sep = "_"), 
                                     value = combn(.x$date, 2)[2,]- combn(.x$date, 2)[1,]))) %>% 
  unnest() %>% 
  distinct() %>% 
  spread(key, value) %>% 
  select(site, t0_t1,t0_t2, t0_t3, t0_t4, t0_t5)
View(time_mh_c)

mh_graph_c<-mh3_c %>%
  gather(variable, value, -site) %>% 
  left_join(., time_mh_c %>% gather(variable, value, -site), by=c('site', 'variable'))%>% 
  transmute(site, time=variable, av_cum_acr=value.x, days=value.y)

mh_graph2_c<-mh2_c %>% 
  select(site, av_cum_acr, standard_error)%>% 
  inner_join(., mh_graph_c, by =c('site', 'av_cum_acr')) 

mh_base_c<-mh %>% 
  filter(timestep=="t0") %>%
  select(site, measurement, timestep) %>% 
  distinct() %>%
  rename(av_cum_acr=measurement, time=timestep)
mh_base_c$days<-0


mh_graph2_c<-mh_graph2_c %>% 
  bind_rows(mh_base_c)

####Graph by Site####
a_c<-mh_graph2_c %>% 
  filter(site=="CovePoint") %>% 
  ggplot()
b_c<-a_c+
  geom_point(aes(x=days, y=av_cum_acr), size=2)+
  geom_smooth(method=lm, aes(x=days, y=av_cum_acr), fill=NA)+
  stat_poly_eq(formula = my.formula, aes(x=days, y=av_cum_acr, label = 
                                           paste(..eq.label.., ..rr.label.., sep = "~~~")), parse = TRUE,
               label.y.npc = 1)+
  scale_color_ptol()+
  theme_classic()+
  theme(panel.grid.major.y = element_line(color="grey80"),
        axis.text = bold, 
        axis.title = bold)+
  labs(bold)+
  guides(color=guide_legend(title="SET"), linetype=guide_legend(title="SET"))+
  ylab("Cumulative accretion (mm)")+xlab("Days since Installation")+
  ggtitle("Cove Point Marker Horizon Data")
b_c
