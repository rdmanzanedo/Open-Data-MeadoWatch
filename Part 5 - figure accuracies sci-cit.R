############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

###################PART 5######################

##Continuation of Figure 4: Data validation comparison of scientist vs. citizen scientists

#load accuracies data per phenostate and species
phenostates = read.csv('Accuracy per phenostate-trail.csv')
head(phenostates)

#graphical parameters
par(bty="o")
par(lwd=2)
par(tcl=-0.2)
par(las=1)
par(cex.lab=1.2)

#Figure 4a: metrics per phenostate
#set the x-axes values for plotting and a small offset for the different hikes
phenostates$Y = 1:4
phenostates$Y2 = phenostates$Y+0.2
#plot each phenostate
plot(accuracy.rl~Y, phenostates,
     pch=21,
     bg='orange',
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     xlim=c(0.5,4.5),
     main='Assessment scientists vs. citizens',
     axes=F,
     xlab='Phenostates',
     ylab='Metric value')
axis(1, labels=c('Budding','Flowering','Fruiting','Seeding'),
     at=c(1:4))
axis(2)
box()
points(accuracy.gb~Y2, phenostates,
       pch=21,
       bg='steelblue3',
       cex=2,
       lwd=0.5)

points(sensitivity.rl~Y, phenostates,
     pch=24,
     bg='orange',
     lwd=0.5,
     cex=2,
     ylim=c(0,1))
points(sensitivity.gb~Y2, phenostates,
       pch=24,
       bg='steelblue3',
       cex=2,
       lwd=0.5)

points(specificity.rl~Y, phenostates,
       pch=23,
       bg='orange',
       lwd=0.5,
       cex=2,
       ylim=c(0,1))
points(specificity.gb~Y2, phenostates,
       pch=23,
       bg='steelblue3',
       cex=2,
       lwd=0.5)
##add the legends
legend('bottomleft',
       legend=c('Accuracy', 'Sensitivity', 'Specificity'),
       pch=c(21,24,23),
       lwd=1,
       box.col='white',
       cex=1.25)
legend('bottomright',
       legend=c('Glacier Basin', 'Reflection Lakes'),
       col=c('orange','steelblue3'),
       pch=16,
       lwd=1,
       box.col='white',
       cex=1.25)

###Figure 4b: metrics per species
#load species data
species = read.csv('Accuracy per species.csv')
head(species)
position.sp = as.numeric(as.factor(species$species))

#ACCURACIES
plot(accuracy.rl~position.sp, species,
     pch=16,
     col=rgb(0.8,0.2,0.0,0.4),
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     main='Accuracy per species',
     axes=F,
     xlab='Species',
     ylab='Metric value')
axis(1, labels=unique(species$species),
     at=c(1:length(unique(species$species))),
     las=2)
axis(2)
box()
points(accuracy.gb~position.sp, species,
       pch=16,
       col=rgb(0.0,0.0,0.5,0.4),
       cex=2,
       lwd=0.5)

#SENSITIVITIES
plot(sensitivity.rl~position.sp, species,
     pch=16,
     col=rgb(0.8,0.2,0.0,0.4),
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     main='Sensitivity per species',
     axes=F,
     xlab='Species',
     ylab='Metric value')
axis(1, labels=unique(species$species),
     at=c(1:length(unique(species$species))),
     las=2)
axis(2)
box()
points(sensitivity.gb~position.sp, species,
       pch=16,
       col=rgb(0.0,0.0,0.5,0.4),
       cex=2,
       lwd=0.5)

#SPECIFICITIES
plot(specificity.rl~position.sp, species,
     pch=16,
     col=rgb(0.8,0.2,0.0,0.4),
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     main='Specificity per species',
     axes=F,
     xlab='Species',
     ylab='Metric value')
axis(1, labels=unique(species$species),
     at=c(1:length(unique(species$species))),
     las=2)
axis(2)
box()
points(specificity.gb~position.sp, species,
       pch=16,
       col=rgb(0.0,0.0,0.5,0.4),
       cex=2,
       lwd=0.5)


####CONFLICT ON THE SPECIES NAMES!!! CORRECT!!!
##END OF CODE
