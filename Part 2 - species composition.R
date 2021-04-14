############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

###################PART 2######################

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


#keep the reports
species.dist = data.frame('species' = MW_PhenoDat_2013_2019$Species,
                          'observer' = MW_PhenoDat_2013_2019$Observer,
                          'transect' = MW_PhenoDat_2013_2019$Transect)

#remove all other duplicates
unique.species.dist= unique(species.dist)

#calculate the proportion of species per trail
rep.species = table(unique.species.dist[,-2])
rep.species2 = prop.table(rep.species,2)

#plot as a vector plot
barplot(rep.species2,
        col = rainbow(dim(rep.species2)[1]),
        ylab='Proportion of reports per species (%)')

#define the x position and the color coding to add labels on top
acc.pos.gb = cumsum(rep.species2[,1])
acc.pos.rl = cumsum(rep.species2[,2])
acc.lab.gb = rep.species2[,1] != 0
acc.lab.rl = rep.species2[,2] != 0

#labels appear on top (will me adjusted in inkscape, zero pressence are writen white)
text(rownames(rep.species2), 
     x=rep(1, length(rownames(rep.species2))),
     y=acc.pos.gb,
     col=acc.lab.gb)
text(rownames(rep.species2), 
     x=rep(2, length(rownames(rep.species2))),
     y=acc.pos.rl,
     col=acc.lab.rl)


#####END OF CODE#########

