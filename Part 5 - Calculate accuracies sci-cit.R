############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL-JULY 2021#########

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
plot(Y~accuracy.rl, phenostates,
     pch=21,
     bg='orange',
     lwd=0.5,
     cex=2,
     xlim=c(0,1),
     ylim=c(0.5,4.5),
     main='Assessment scientists vs. citizens',
     axes=F,
     ylab='Phenostates',
     xlab='Value (%)',
     col=rgb(0,0,0,0))
axis(2, labels=c('Budding','Flowering','Fruiting','Seeding'),
     at=c(1:4))
axis(1)
box()
points(Y~accuracy.gb, phenostates,
       pch=21,
       bg='steelblue3',
       cex=2,
       lwd=0,
       col=rgb(0,0,0,0))
points(Y~sensitivity.rl, phenostates,
     pch=24,
     bg='orange',
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     col=rgb(0,0,0,0))
points(Y~sensitivity.gb, phenostates,
       pch=24,
       bg='steelblue3',
       cex=2,
       lwd=0.5,
       col=rgb(0,0,0,0))
points(Y~specificity.rl, phenostates,
       pch=23,
       bg='orange',
       lwd=0.5,
       cex=2,
       col=rgb(0,0,0,0))
points(Y~specificity.gb, phenostates,
       pch=23,
       bg='steelblue3',
       cex=2,
       lwd=0,
       col=rgb(0,0,0,0))

##means +- standard errors
head(phenostates)
phenostates$mean = apply(phenostates[,2:7],1, FUN='mean')
phenostates$sd = apply(phenostates[,2:7],1, FUN='sd')
phenostates$se = (phenostates$sd) / (sqrt(phenostates$mean))

#plot them
points(Y~mean, phenostates,
       pch=16,
       bg='black',
       cex=3,
       lwd=0)
for (i in 1:4){
  lines(x=c((phenostates$mean[i]-phenostates$se[i]),(phenostates$mean[i]+phenostates$se[i])), 
            y=c(i,i))

}


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
#add mean values
lines(x=c(0.9,1.1), y=c(rep(mean(as.numeric(phenostates[1,c(2,4,6)])),2)), 
       pch=8,
       col='orange',
      lwd=3)
lines(x=c(1.1,1.3), y=c(rep(mean(as.numeric(phenostates[1,c(3,5,7)])),2)), 
      pch=8,
      col='steelblue3',
      lwd=3)

lines(x=c(1.9,2.1), y=c(rep(mean(as.numeric(phenostates[2,c(2,4,6)])),2)), 
      pch=8,
      col='orange',
      lwd=3)
lines(x=c(2.1,2.3), y=c(rep(mean(as.numeric(phenostates[2,c(3,5,7)])),2)), 
      pch=8,
      col='steelblue3',
      lwd=3)

lines(x=c(2.9,3.1), y=c(rep(mean(as.numeric(phenostates[3,c(2,4,6)])),2)), 
      pch=8,
      col='orange',
      lwd=3)
lines(x=c(3.1,3.3), y=c(rep(mean(as.numeric(phenostates[3,c(3,5,7)])),2)), 
      pch=8,
      col='steelblue3',
      lwd=3)

lines(x=c(3.9,4.1), y=c(rep(mean(as.numeric(phenostates[4,c(2,4,6)])),2)), 
      pch=8,
      col='orange',
      lwd=3)
lines(x=c(4.1,4.3), y=c(rep(mean(as.numeric(phenostates[4,c(3,5,7)])),2)), 
      pch=8,
      col='steelblue3',
      lwd=3)


###Figure 4b: metrics per species
#load species data
species = read.csv('Accuracy per species.csv')
means = read.csv('Mean_metrics_per_species.csv')
head(species)
position.sp = as.numeric(as.factor(species$species))
pch.vector = length(species$phenostate)/4

color.orange = rgb(255/255,165/255,0/255,0.4)
color.blue = rgb(79/255,148/255,205/255,0.6)


#ACCURACIES
plot(accuracy.rl~position.sp, species,
     pch=rep(c(16,17,18,19), pch.vector),
     col=color.orange,
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     main='Accuracy per species',
     axes=F,
     xlab='Species',
     ylab='Metric value')
axis(1, labels=unique(species$species),
     at=unique(as.numeric(position.sp)),
     las=2)
axis(2)
box()
points(accuracy.gb~position.sp, species,
       pch=rep(c(16,17,18,19), pch.vector),
       col=color.blue,
       cex=2,
       lwd=0.5)

#add the mean values per species 
means = read.csv('Mean_metrics_per_species.csv')
accura = subset(means, means$mean.metric=='acc')
#sort in alphabetical order
accura = accura[order(accura$species),]
points(accura$value, 
      pch=8,
      col='red',
      lwd=3)



#SENSITIVITIES
plot(sensitivity.rl~position.sp, species,
     pch=rep(c(16,17,18,19), pch.vector),
     col=color.blue,
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     main='Sensitivity per species',
     axes=F,
     xlab='Species',
     ylab='Metric value')
axis(1, labels=unique(species$species),
     at=unique(as.numeric(position.sp)),
     las=2)
axis(2)
box()
points(sensitivity.gb~position.sp, species,
       pch=rep(c(16,17,18,19), pch.vector),
       col=color.orange,
       cex=2,
       lwd=0.5)

#add the mean values per species 
sen = subset(means, means$mean.metric=='sen')
#sort in alphabetical order
sen = sen[order(sen$species),]
points(sen$value, 
       pch=8,
       col='red',
       lwd=3)

#SPECIFICITIES
plot(specificity.rl~position.sp, species,
     pch=rep(c(16,17,18,19), pch.vector),
     col=color.blue,
     lwd=0.5,
     cex=2,
     ylim=c(0,1),
     main='Specificity per species',
     axes=F,
     xlab='Species',
     ylab='Metric value')
axis(1, labels=unique(species$species),
     at=unique(as.numeric(position.sp)),
     las=2)
axis(2)
box()
points(specificity.gb~position.sp, species,
       pch=rep(c(16,17,18,19), pch.vector),
       col=color.orange,
       cex=2,
       lwd=0.5)
legend('bottomleft',
       legend=c('Budding', 'Flowering','Fruiting','Seeding'),
       pch=c(16,17,18,19),
       lwd=1,
       box.col='white',
       cex=1.25)

#add the mean values per species 
spe = subset(means, means$mean.metric=='spe')
#sort in alphabetical order
spe = spe[order(spe$species),]
points(spe$value, 
       pch=8,
       col='red',
       lwd=3)


##END OF CODE
