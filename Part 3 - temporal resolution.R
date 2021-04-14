############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

###################PART 3######################

##Figure 2: Species composition of the plots
#load report data
MW_PhenoDat_2013_2019 <- read.csv("MW_PhenoDat_2013_2019.csv")
head(MW_PhenoDat_2013_2019)

#graphical parameters
par(bty="o")
par(lwd=2)
par(tcl=-0.2)
par(las=1)
par(cex.lab=1.2)

###temporal resolution of the database####♥
#first turn date to DOY for calculations
MW_PhenoDat_2013_2019$DOY = strftime(as.Date(MW_PhenoDat_2013_2019$Date, format='%m/%d/%Y'), format = "%j")

gb.dates = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$Transect=='Glacier Basin')
rf.dates  = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$Transect=='Reflection Lakes')
gb.days.with.rep = as.numeric(names(table(gb.dates$DOY)))
rf.days.with.rep = as.numeric(names(table(rf.dates$DOY)))
gb.bet.days = diff(gb.days.with.rep, lag=1)
rf.bet.days = diff(rf.days.with.rep, lag=1)

##gb
par(mfcol=c(2,3))
for (i in 2015:2019){
  aa = subset(gb.dates, gb.dates$Year==i)
  aa.gb.days.with.rep = as.numeric(names(table(aa$DOY)))
  aa.gb.bet.days = diff(aa.gb.days.with.rep, lag=1)
  plot(aa.gb.bet.days ~ aa.gb.days.with.rep[-1], 
       main=paste(i),
       pch=21, 
       col='orange2',
       bg=rgb(.8,.6,.4,.6),
       cex=1.5,
       xlim=c(100,300),
       ylim=c(0,20),
       xlab='Day of the year',
       ylab='Temporal frequency of Meadowatch reports (time since last report)')
  abline(h=1, lty=2)
  text(120,2,'Daily')
  abline(h=7, lty=2)
  text(120,8,'Weekly')
  
}

##reflection
par(mfcol=c(2,3))
for (i in 2015:2019){
  aa = subset(gb.dates, gb.dates$Year==i)
  aa.gb.days.with.rep = as.numeric(names(table(aa$DOY)))
  aa.gb.bet.days = diff(aa.gb.days.with.rep, lag=1)
  plot(aa.gb.bet.days ~ aa.gb.days.with.rep[-1], 
       main=paste(i),
       pch=21, 
       col='orange2',
       bg=rgb(.8,.6,.4,.6),
       cex=1.5,
       xlim=c(100,300),
       ylim=c(0,20),
       xlab='Day of the year',
       ylab='Temporal frequency of Meadowatch reports (time since last report)')
  abline(h=1, lty=2)
  text(120,2,'Daily')
  abline(h=7, lty=2)
  text(120,8,'Weekly')
  
}


plot(gb.bet.days ~ gb.days.with.rep[-1], 
     pch=21, 
     col='orange2',
     bg=rgb(.8,.6,.4,.6),
     cex=1.5,
     xlim=c(0,366),
     xlab='Day of the year',
     ylab='Temporal frequency of Meadowatch reports (time since last report)')
abline(h=1, lty=2)
text(20,2,'Daily')
abline(h=7, lty=2)
text(20,8,'Weekly')
abline(h=30, lty=2)
text(20,31,'Monthly')





#change date to DOY 



#budding graph
budding.graph = data.frame('year' = MW_PhenoDat_2013_2019$Year,
                           'transect' = MW_PhenoDat_2013_2019$Transect,
                           'DOY' = MW_PhenoDat_2013_2019$DOY,
                           'observer' = MW_PhenoDat_2013_2019$Observer,
                           'species' = MW_PhenoDat_2013_2019$Species,
                           'site' = MW_PhenoDat_2013_2019$Site_Code,
                           'budding' = MW_PhenoDat_2013_2019$Bud,
                           'flowering' = MW_PhenoDat_2013_2019$Flower,
                           'fruiting' = MW_PhenoDat_2013_2019$Fruit,
                           'dispersing' = MW_PhenoDat_2013_2019$Disperse)

head(budding.graph)
budding2 = unique(budding.graph)
head(budding2)



##species - doy - site - n.years.with.budyes
budding.3.sp1 = data.frame('DOY' = 1:366,
                           'species' = rep(NA,366),
                           'site' = rep(NA,366),
                           'n.years.bud' = rep(NA,366))


for
sitenames=as.character(levels(budding.graph$site))



species1 = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$Species=='LICA')
buds.lica = data.frame('DOY' = 1:366,
                       'n.budding.lica' = rep(NA, 366))

species1.1 = data.frame(species1$)
?unique

for (i in 1:366){
  aa = subset(species1, species1$DOY == i)
  buds.lica$n.budding.lica[i] = dim(aa)[1]
}
plot(buds.lica)
