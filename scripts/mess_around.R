asdf<-set2 %>%
  select(set_name, reading_date) %>% 
  distinct()
asdf$reading_date<-mdy(asdf$reading_date)

asdfg<-asdf %>% 
  group_by(set_name) %>% 
  mutate(timestep = dplyr::row_number(reading_date)) %>% 
  mutate(timestep = timestep-1) %>% 
  mutate(timestep = as.factor(paste("t", timestep, sep = "")))

View(asdfg)
set2$reading_date<-mdy(set2$reading_date)
set2<-set2 %>% 
  left_join(asdfg, by = c("set_name", "reading_date"))
View(set2)


seta<-read.csv(file = "comb_set_data.csv")
seta$site<-as.factor(seta$site)
seta$position<-as.factor(seta$position)
seta$pin_number<-as.factor(seta$pin_number)
seta$reading_date<-mdy(seta$reading_date) #if your date data is in a different format look at the lubridate package to figure out what code you need
seta$pinid<-as.factor(paste(seta$set_name, seta$position, seta$pin_number, sep = "_"))
seta <- seta [,c(1,2,3,4,5,6,7,9,8)]



all.equal(set, seta)


a<- ggplot (set, aes(x=reading_date, y=measurement) )+
  geom_line(aes(color=pinid))+
  geom_smooth(method = lm, color="black")
a

S = length(unique(set$pinid))     
dat$t0_t1 = as.numeric(factor(set$pinid, labels = 1:S))     
mat = matrix(nrow = S, ncol = 2) # empty matrix 

for (s in 1:S) { # for each subject 
  measurement = set[set$pinid == s, 'Score'] # get scores for participant s 
  measurement = na.omit(measurement) 
  avCumDiff = sum(diff(scores))/length(scores) # average cumulative difference 
  mat[s,] = c(s, avCumDiff) # add to matrix
}     
colnames(mat) <- c('ppt', 'AvDiff') 


test<-set %>%
  group_by(pinid) %>%
  summarize(t0t1 = (measurement-measurement), t1t2=(measurement-measurement))

#
pin_dif <- function(x) setNames(
  data.frame(pinid = x$pinid, as.list(- combn(x$measurement, 2, diff))),
  c("pinid", combn(x$timestep, 2, paste, collapse = "_"))
)

mydata$timestep<-as.character(mydata$timestep)
head(by(set, set$pinid, pin_dif))

time_dif <- function(x) setNames(
  data.frame(pinid = x$pinid, as.list(- combn(x$reading_date, 2, diff))),
  c("pinid", combn(x$timestep, 2, paste, collapse = "_"))
)

head(by(set, set$pinid, time_dif))

test3<-set %>%
  filter(.$set_name=="CP_South", .$position=="1"|.$position=="3")%>%
  filter(.$pin_number=="1"|.$pin_number=="2"|.$pin_number=="3")%>%
  select(pinid,reading_date,timestep,measurement)
View(test3)


mydata<-data.frame(pinid=c("CP_South_1_1", "CP_South_1_2","CP_South_1_1", 
    "CP_South_1_2","CP_South_1_1", "CP_South_1_2"), reading_date=c("2014-08-26","2014-08-26","2015-04-06", "2015-04-06","2015-10-20", "2015-10-20"), 
      timestep=c("t0", "t0", "t1","t1","t2","t2"), measurement = c(189, 186, 187, 185, 184, 181))
mydata$reading_date<-ymd(mydata$reading_date)
View(mydata)
#






means <-
  read.table(text =
               " rat   gene gene_category timepoint1 timepoint2
             1   Rat1  gene1  experimental   23.36667   23.49667
             2   Rat1  gene2  experimental   18.26000   18.38000
             3   Rat1  gene3       control   42.05500   41.45000
             4   Rat1  gene4       control   40.08667   39.89500
             5   Rat2  gene1  experimental   25.29333   22.83000
             6   Rat2  gene2  experimental   19.72667   19.19333
             7   Rat2  gene3       control   42.05500   41.45000
             8   Rat2  gene4       control   40.08667   39.89500")
View(means)


geneLists <-
  means %>%
  {split(.$gene, .$`gene_category`)} %>%
  lapply(unique) %>%
  lapply(as.character) %>%
  lapply(function(x){paste0("`", x, "`")})
geneLists


timeLists <-
  set %>%
  {split(.$timestep)} %>%
  lapply(unique) %>%
  lapply(as.character) %>%
  lapply(function(x){paste0("`", x, "`")})


#trying another way

pintime<- set %>%
  select(pinid, measurement, timestep) %>%
  group_by(pinid) %>%
  spread(timestep, measurement)
View(pintime)

pintrend<-pintime %>%
  group_by(pinid) %>%
  mutate(t1t0 = t1 - t0,
         t1t2 = t2 - t1,
         t2t3 = t3 - t2,
         t3t4 = t4 - t3,
         t4t5 = t5 - t4,
         t5t6 = t6 - t5)
View(pintrend)  






#Trying one way # if add data need to change the 7
f <- function(x) setNames(
  data.frame(pintrend = x$pinid[1:2], as.list(- combn(x$measurement, 7, diff))),
  c("pinid", combn(x$timestep, 2, paste, collapse = "_"))
)

by(set,set$pinid, f)


#Stuff
longForm <-
  set %>%
  select(-timestep) %>%
  gather("timestep", "measurement") %>%
  spread(pinid, measurement) %>%
  mutate_(.dots = colsToCreate) %>%
  select_(.dots = paste0("-",unlist(geneLists))) %>%
  gather(Comparison, Difference, -rat, -timepoint) %>%
  mutate(time = parse_number(timepoint)) %>%
  separate(Comparison, c("exp_Gene", "cont_Gene"), " - ")

head(longForm)






a<-for(i in seq(1, length(time_list)+1, 1)) {
  x[[i-1]]<-time_list[[i]]$measurement - time_list[[i-1]]$measurement
}




colsToCreate <-
  outer(geneLists[["experimental"]]
        , geneLists[["control"]]
        , paste, sep = " - ") %>%
  as.character()

longForm <-
  set %>%
  select(-gene_category) %>%
  gather("timepoint", "value", starts_with("timepoint")) %>%
  spread(gene, value) %>%
  mutate_(.dots = colsToCreate) %>%
  select_(.dots = paste0("-",unlist(geneLists))) %>%
  gather(Comparison, Difference, -rat, -timepoint) %>%
  mutate(time = parse_number(timepoint)) %>%
  separate(Comparison, c("exp_Gene", "cont_Gene"), " - ")





a_01<- set %>%
  group_by(set_name, position, pin_number) %>%
  mutate(measurement-measurement)
a_01
###
setDT(set_average)
setDT(time_tibble)
xcols=names(set_average)[-1]
icols=paste0("i.", xcols)
set_rate<-set_average[time_tibble, on = 'set_name', Map(`/`, mget(xcols), mget(icols)), by=.EACHI]
View(set_rate)
set_rate<- set_rate %>% 
  mutate_if(is.numeric, funs(.*365.25))
###by Site
setDT(site_average)
setDT(time_site)
ycols=names(site_average)[-1]
jcols=paste0("i.", xcols)
site_rate<-site_average[time_site, on = 'site', Map(`/`, mget(ycols), mget(jcols)), by=.EACHI] %>% 
  mutate_if(is.numeric, funs(.*365.25))
View(site_rate)

