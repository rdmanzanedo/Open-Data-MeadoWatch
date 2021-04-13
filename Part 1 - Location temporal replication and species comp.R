############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

##Figure 1: Location of the plots, temporal distribution, species compositions

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

#define the edges of the map for the satelite image
bounds = c(left = -121.85, 
            bottom = 46.74,
            right = -121.5,
            top = 46.95)

#download terrain tiles
mtrain = get_stamenmap(bbox = bounds, maptype = "terrain", zoom = 13)

#plot the satelite and locations, color according to elevation value
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



