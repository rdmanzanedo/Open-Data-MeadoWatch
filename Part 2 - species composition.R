############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL-JULY 2021#########

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

#labels appear on top (will me adjusted in inkscape, zero presence are wriTten white)
text(rownames(rep.species2), 
     x=rep(1, length(rownames(rep.species2))),
     y=acc.pos.gb,
     col=acc.lab.gb)
text(rownames(rep.species2), 
     x=rep(2, length(rownames(rep.species2))),
     y=acc.pos.rl,
     col=acc.lab.rl)

#calculate species per plot and then replace the elevations

MW_PhenoDat_2013_2019$Site_Code = as.factor(MW_PhenoDat_2013_2019$Site_Code)
levels(MW_PhenoDat_2013_2019$Site_Code) = c("GB01","GB10","GB11","GB12","GB13","GB14",
                                            "GB15","GB16","GB17","GB02","GB03","GB04",
                                            "GB05","GB06","GB07","GB08","GB09","RL01","RL10","RL11",
                                            "RL02","RL03","RL03a","RL03a","RL03b","RL04","RL05","RL05a",
                                            "RL05b","RL05b","RL06","RL07","RL08","RL09")

species.dist2 = data.frame('species' = MW_PhenoDat_2013_2019$Species,
                          'observer' = MW_PhenoDat_2013_2019$Observer,
                          'transect' = MW_PhenoDat_2013_2019$Transect,
                          'site_Code' = MW_PhenoDat_2013_2019$Site_Code)

#remove duplicates
unique.species.dist2= unique(species.dist2)

#calculate the proportion of species per plot and by proportion
alt.species = table(unique.species.dist2[,-c(2,3)])

#plots RL08 and RL09 have no records?
alt.species = alt.species[,-c(31,32)]
alt.species.prop = prop.table(alt.species,2)

#divide RL and GB
GB.species = alt.species[,1:17]
RL.species = alt.species[,18:30]

#sort them in increasing value fo the plots
GB.ordered = GB.species[,order(colnames(GB.species))]
RL.ordered = RL.species[,order(colnames(RL.species))]

#prepare color palette

GB.ordereddf = as.matrix(GB.ordered)
pressenceGB = GB.ordered
str(GB.ordereddf)
heatmap(GB.ordereddf,col = coloring, Colv = NA, Rowv=NA, scale='row')


?heatmap

#####END OF CODE#########

