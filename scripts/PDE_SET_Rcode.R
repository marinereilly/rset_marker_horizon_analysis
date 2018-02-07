##From LeeAnn Haaf @PDE

##needed packages: plyr, dplyr, ggplot2, lubridate (install in this order)
##----------------------------------SURFACE ELEVATION TABLES----------------------------------------------
HP<-read.csv(file.choose()) ##must be csv. raw measurements need to be calculated into change intervals (current reading-previous reading [final-initial]) and cumulative changes (previous intervals + current interval); SETs only!
HP$date<-mdy(HP$date)
HPsum<-ddply(HP, c("set","day","date"), summarize, mean=mean(cumulat_mm), se=sd(cumulat_mm)/sqrt(length(day)))
HPplot<-ggplot(HPsum, aes(x=date, y=mean, color=factor(set), shape=factor(set)))+geom_hline(yintercept=0, color="black")+geom_line(size=1)+geom_point(size=3)+geom_errorbar(aes(ymax=mean+se, ymin=mean-se), width=10, linetype=1)+labs(y="Cumulative Elevation Change (mm)", x="Date")+theme_bw()+theme(legend.key.width=unit(3.5,"line"))+scale_color_manual(values=c("#44aa99","#f39c12","#332288"), name="SET Number")+scale_shape_manual(values=c(17,15,16), name="SET Number")+scale_x_date(date_labels="%m/%Y",date_minor_breaks = "1 month", date_breaks="4 months")
HPplot+theme(legend.justification=c(1,-0.2),
  legend.position=c(0.2,0.7), ##adjust these to move legend around
  legend.text=element_text(size=10), 
  legend.title=element_text(size=10), 
  axis.title.x=element_text(size=10),
  axis.title.y=element_text(size = 10), 
  legend.key = element_blank(), 
  legend.background = element_rect(color="black",size = 0.1))
HPsetlm <- lm(mean~day*set, data=HPsum) ##are SETs significantly different? check interaction term
par(mfrow=c(2,2))
plot(HPsetlm)
lapply(split(HPsum,HPsum$set),function(HPsum){summary(lm( HPsum$mean ~ HPsum$day ))}) ##run individual regressions within one dataframe
###------------------------------------------------MARKER HORIZONS----------------------------------------------
HPmh<-read.csv(file.choose())##should already be in date (mm/dd/yyyy), day, mean, sd, se format; MH data are sometimes missing and QA is important before uploading to R. Best to do these outside to make sure zeros are truly zeros and NAs are handled correctly; MHs are already 'cumulative'
HPmh$date<-mdy(HPmh$date)
HPplotmh<-ggplot(HPmh, aes(x=date, y=mean_acc, color=factor(set), shape=factor(set)))+geom_hline(yintercept=0, color="black")+geom_line(size=1)+geom_point(size=3)+geom_errorbar(aes(ymax=mean_acc+se, ymin=mean_acc-se), width=10, linetype=1)+labs(y="Accretion (mm)", x="Date")+theme_bw()+theme(legend.key.width=unit(3.5,"line"))+scale_color_manual(values=c("#006666","#cc6633","#000033"), name="SET Number")+scale_shape_manual(values=c(17,15,16), name="SET Number")+scale_x_date(date_labels="%m/%Y",date_minor_breaks = "1 month", date_breaks="4 months")
HPplotmh+theme(legend.justification=c(1,-0.2),
             legend.position=c(0.2,0.7), ##adjust these to move legend around
             legend.text=element_text(size=10), 
             legend.title=element_text(size=10), 
             axis.title.x=element_text(size=10),
             axis.title.y=element_text(size = 10), 
             legend.key = element_blank(), 
             legend.background = element_rect(color="black",size = 0.1))
HPmhlm <- lm(mean_acc~day*set, data=HPmh)
par(mfrow=c(2,2))
plot(HPmhlm)
lapply(split(HPmh,HPmh$set),function(HPmh){summary(lm( HPmh$mean_acc ~ HPmh$day ))})
