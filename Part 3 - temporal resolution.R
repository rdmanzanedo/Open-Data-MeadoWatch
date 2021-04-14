############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

###################PART 3######################

##Figure 3: Temporal resolution of the plots
#load report data
MW_PhenoDat_2013_2019 <- read.csv("MW_PhenoDat_2013_2019.csv")
head(MW_PhenoDat_2013_2019)

#graphical parameters
par(bty="o")
par(lwd=2)
par(tcl=-0.2)
par(las=1)
par(cex.lab=1.2)

###temporal resolution of the database####
#first turn date to DOY for calculations
MW_PhenoDat_2013_2019$DOY = strftime(as.Date(MW_PhenoDat_2013_2019$Date, format='%m/%d/%Y'), format = "%j")

##calculate for each hike the number of days in between reports (to get the temporal resolution)
####first without considering different years#####

#select hikes
gb.dates = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$Transect=='Glacier Basin')
rl.dates  = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$Transect=='Reflection Lakes')

##GLACIER BASIN#

par(mfrow=c(4,3))
for (i in 2015:2019){
  #subset for the each year
  aa = subset(gb.dates, gb.dates$Year==i)
  #make sure DOY is numeric
  aa.gb.days.with.rep = as.numeric(names(table(aa$DOY)))
  #calculate difference in dates with previous report
  aa.gb.bet.days = diff(aa.gb.days.with.rep, lag=1)
  #plot it
  plot(aa.gb.bet.days~aa.gb.days.with.rep[-1], 
       main=paste(i),
       pch=16, 
       col=rgb(.8,.65,.4,.8),
       cex=2,
       xlim=c(0,365),
       ylim=c(0,20),
       xlab='Day of the year',
       ylab='Temporal frequency of Meadowatch reports (time since last report)')
  #add reference lines and indications of daily and weekly resolutions
  abline(h=1, lty=2, lwd=0.5)
  text(30,2.5,'Daily')
  abline(h=7, lty=2, lwd=0.5)
  text(30,9,'Weekly')
  
}

##REFLECTION LAKE#
for (i in 2013:2019){
  #subset for the each year
  aa = subset(rl.dates, rl.dates$Year==i)
  #make sure DOY is numeric
  aa.gb.days.with.rep = as.numeric(names(table(aa$DOY)))
  #calculate difference in dates with previous report
  aa.gb.bet.days = diff(aa.gb.days.with.rep, lag=1)
  #plot it
  plot(aa.gb.bet.days ~aa.gb.days.with.rep[-1], 
       main=paste(i),
       pch=16, 
       col=rgb(.18,.34,.47,.8),
       cex=2,
       xlim=c(0,365),
       ylim=c(0,20),
       xlab='Day of the year',
       ylab='Temporal frequency of Meadowatch reports (time since last report)')
  #add reference lines and indications of daily and weekly resolutions
  abline(h=1, lty=2, lwd=0.5)
  text(30,2.5,'Daily')
  abline(h=7, lty=2, lwd=0.5)
  text(30,9,'Weekly')
  
}

##END OF CODE