############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL-JULY 2021#########

###################PART 1######################

##Figure 1: Location of the plots and temporal distribution

#######plot location####

##load the plots coordinates and elevation
MW_SiteInfo_2013_2020 = read.csv("MW_SiteInfo_2013_2020.csv")
head(MW_SiteInfo_2013_2020)

#mapping libraries and some graphical parameters
library(ggmap)
library(ggplot2)

par(bty="o")
par(lwd=2)
par(tcl=-0.2)
par(las=1)
par(cex.lab=1.2)

#define the edges of the map for the satellite image
bounds = c(left = -121.85, 
            bottom = 46.74,
            right = -121.5,
            top = 46.95)

#download terrain tiles
mtrain = get_stamenmap(bbox = bounds, maptype = "terrain", zoom = 13)

#plot the satellite and locations, color according to elevation value
map = ggmap(mtrain) +  
  geom_point(aes(y = Latitude, 
                 x = Longitude, 
                 colour = Elevation,
                 size=2), 
             data = MW_SiteInfo_2013_2020)
map+scale_color_gradient(low="blue",high="indianred")
map+scale_color_gradientn(colours = rainbow(5))

###reports per year in both hikes#######

#load report data
MW_PhenoDat_2013_2019 <- read.csv("MW_PhenoDat_2013_2019.csv")
head(MW_PhenoDat_2013_2019)
#there are multiple reports per year (and potentially per day), subset to observer to make table
reports.peryear = data.frame('year' = MW_PhenoDat_2013_2019$Year,
                             'observer' = MW_PhenoDat_2013_2019$Observer,
                             'transect' = MW_PhenoDat_2013_2019$Transect)

#remove all other duplicates
unique.reports=unique(reports.peryear)

#number formatting of table results and adding the year numbers
rep.year = data.frame(table(unique.reports[,-2]))
rep.year$Freq = as.numeric(rep.year$Freq)
rep.year$year = 2013:2019

#remove zeros for plotting
rep.year$Freq[rep.year$Freq==0] = NA

#plot
par(mfcol=c(1,1))
plot(Freq~year, data=subset(rep.year, transect=='Glacier Basin'), 
     pch=20,
     type='b', 
     col='orange',
     xlim=c(2013,2020),
     ylim=c(0,100),
     ylab='Number of reports per year',
     xlab='Year',
     lwd=4,
     cex=3)
lines(Freq~year, data=subset(rep.year, transect=='Reflection Lakes'), 
      pch=20,
      type='b',
      col= 'dodgerblue4',
      lwd=4,
      cex=3)
text(2019.7,70, 'Glacier Basin', col='orange')
text(2019.7,85, 'Reflection Lakes', col='dodgerblue3')

######Records per month######

#there multiple reports per year (and potentially per day), subset to observer to make table
reports.permonth = data.frame('month' = MW_PhenoDat_2013_2019$Month,
                              'observer' = MW_PhenoDat_2013_2019$Observer,
                              'transect' = MW_PhenoDat_2013_2019$Transect,
                              'year' = MW_PhenoDat_2013_2019$Year)

#remove all other duplicates
head(reports.permonth)
unique.reports2=unique(reports.permonth)

#number formatting of table results and adding the month labels
rep.month = data.frame(table(unique.reports2[,-2]))
rep.month$Freq = as.numeric(rep.month$Freq)
rep.month$month = as.numeric(rep.month$month)


#plot GB number of reports per year and month
#generate a ramp palette for colors and define line width
colfunc <- colorRampPalette(c("red", "blue"), interpolate="spline")
coloring = colfunc(8)
coloring.GB = c(rgb(108/255,69/255,0),
                rgb(151/255,96/255,0),
                rgb(179/255,114/255,0),
                rgb(212/255,135/255,0),
                rgb(254/255,162/255,0))

coloring.RL = c(rgb(3/255,21/255,39/255),
                rgb(4/255,40/255,76/255),
                rgb(5/255,53/255,101/255),
                rgb(6/255,68/255,130/255),
                rgb(7/255,71/255,156/255),
                rgb(8/255,95/255,182/255),
                rgb(9/255,112/255,217/255))
lining = 1

#plot Glacier basin as lines 'b'
par(mfcol=c(1,2))
plot(Freq~month, 
     data=subset(rep.month, transect=='Glacier Basin' & year == '2015'),
     type='b',
     ylim=c(0,60),
     col=coloring.GB[1],
     ylab='Number of reports per month',
     xlab='Month',
     axes=F,
     main='Glacier Basin',
     lwd=lining,
     pch=22)
#define x labels
axis(1, at= 1:6, labels=c('May',
             'June',
             'July',
             'August',
             'September',
             'October'))
axis(2)
box()
#add lines per years
lines (Freq~month,
       subset(rep.month, transect=='Glacier Basin' & year == '2016'),
       col = coloring.GB[2],
       bg = coloring.GB[2],
       lwd=lining,
       type='b',
       pch=23)
lines (Freq~month,
       subset(rep.month, transect=='Glacier Basin' & year == '2017'),
       col = coloring.GB[3],
       bg = coloring.GB[3],
       lwd=lining,
       type='b',
       pch=24)
lines (Freq~month,
       subset(rep.month, transect=='Glacier Basin' & year == '2018'),
       col = coloring.GB[4],
       bg= coloring.GB[4],
       lwd=lining,
       type='b',
       pch=25)
lines (Freq~month,
       subset(rep.month, transect=='Glacier Basin' & year == '2019'),
       col = coloring.GB[5],
       bg = coloring.GB[5],
       lwd=lining,
       pch=26)
#add the legend
legend('topleft',
       legend=as.character(2015:2019),
       col=c(coloring.GB[1:5]),
       pch=22:26,
       lwd=2,
       box.lwd=0)


###Plot reflection lakes
plot(Freq~month, 
     data=subset(rep.month, transect=='Reflection Lakes' & year == '2013'),
     type='b',
     ylim=c(0,60),
     col=coloring.RL[1],
     ylab='Number of reports per month',
     xlab='Month',
     axes=F,
     main='Reflection Lakes',
     lwd=lining,
     pch=20)
axis(1, at= 1:6, labels=c('May',
                          'June',
                          'July',
                          'August',
                          'September',
                          'October'))
axis(2)
box()
lines (Freq~month,
       subset(rep.month, transect=='Reflection Lakes' & year == '2014'),
       col = coloring.RL[2],
       bg=coloring.RL[2],
       lwd=lining,
       type='b',
       pch=21)
lines (Freq~month,
       subset(rep.month, transect=='Reflection Lakes' & year == '2015'),
       col = coloring.RL[3],
       bg = coloring.RL[3],
       lwd=lining,
       type='b',
       pch=22)
lines (Freq~month,
       subset(rep.month, transect=='Reflection Lakes' & year == '2016'),
       col = coloring.RL[4],
       bg = coloring.RL[4],
       lwd=lining,
       type='b',
       pch=23)
lines (Freq~month,
       subset(rep.month, transect=='Reflection Lakes' & year == '2017'),
       col = coloring.RL[5],
       bg= coloring.RL[5],
       lwd=lining,
       type='b',
       pch=24)
lines (Freq~month,
       subset(rep.month, transect=='Reflection Lakes' & year == '2018'),
       col = coloring.RL[6],
       bg = coloring.RL[6],
       lwd=lining,
       type='b',
       pch=25)
lines (Freq~month,
       subset(rep.month, transect=='Reflection Lakes' & year == '2019'),
       col = coloring.RL[7],
       lwd=lining,
       pch=26)
legend('topleft',
       legend=as.character(2013:2019),
       col=c(coloring.RL[1:7]),
       pch=20:26,
       lwd=2,
       box.lwd=0)

###END OF CODE
