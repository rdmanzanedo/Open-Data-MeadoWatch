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

#add reference line of mean
abline(v=mean(phenostates$mean),
       lty=2,
       lwd=0.51)

#plot them
points(Y~mean, phenostates,
       pch=16,
       col='grey30',
       cex=3,
       lwd=0)
for (i in 1:4){
  lines(x=c((phenostates$mean[i]-phenostates$se[i]),(phenostates$mean[i]+phenostates$se[i])), 
            y=c(i,i),
        col='grey30')
}


###Figure 4b: metrics per species
#load species data
species = read.csv('Accuracy per species.csv')
means = read.csv('Mean_metrics_per_species.csv')
head(species)
position.sp = as.numeric(as.factor(species$species))
pch.vector = length(species$phenostate)/4

color.orange = rgb(255/255,165/255,0/255,0.4)
color.blue = rgb(79/255,148/255,205/255,0.6)

##before anything, calculate means +- standard errors per species
head(species)
means.errs = data.frame('species' = rep(NA, length(levels(as.factor(species$species)))),
                        'mean.acc' = rep(NA, length(levels(as.factor(species$species)))),
                        'se.acc' = rep(NA, length(levels(as.factor(species$species)))),
                        'mean.sen' = rep(NA, length(levels(as.factor(species$species)))),
                        'se.sen' = rep(NA, length(levels(as.factor(species$species)))),
                        'mean.spe' = rep(NA, length(levels(as.factor(species$species)))),
                        'se.spe' = rep(NA, length(levels(as.factor(species$species)))))
##subset per species and then calculate basic metrics, save on the dataframe means.errs
for (i in 1:length(levels(as.factor(species$species)))){
  naming = levels(as.factor(species$species))[i]
  spec = subset(species,species==as.character(naming))
  accuracy = mean(c(spec$accuracy.gb,spec$accuracy.rl), na.rm=T)
  sensitiv = mean(c(spec$sensitivity.gb,spec$sensitivity.rl), na.rm=T)
  specific = mean(c(spec$specificity.gb,spec$specificity.rl), na.rm=T)
  
  accuracy.sd = sd(c(spec$accuracy.gb,spec$accuracy.rl), na.rm=T)
  sensitiv.sd = sd(c(spec$sensitivity.gb,spec$sensitivity.rl), na.rm=T)
  specific.sd = sd(c(spec$specificity.gb,spec$specificity.rl), na.rm=T)
  
  accuracy.n = length(na.omit(c(spec$accuracy.gb,spec$accuracy.rl)))
  sensitiv.n = length(na.omit(c(spec$sensitivity.gb,spec$sensitivity.rl)))
  specific.n = length(na.omit(c(spec$specificity.gb,spec$specificity.rl)))
  
  accuracy.se = accuracy.sd / sqrt(accuracy.n)
  sensitiv.se = sensitiv.sd / sqrt(sensitiv.n)
  specific.se = specific.sd / sqrt(specific.n)
  
  means.errs[i,1] = paste (levels(as.factor(species$species))[i])
  means.errs[i,2] = accuracy
  means.errs[i,3] = accuracy.se
  means.errs[i,4] = sensitiv
  means.errs[i,5] = sensitiv.se
  means.errs[i,6] = specific
  means.errs[i,7] = specific.se
  print(means.errs)
}

means.errs  


#plot ACCURACIES
plot(position.sp~accuracy.rl, species,
     pch=rep(c(16,17,18,19), pch.vector),
     col=color.orange,
     lwd=0.5,
     cex=1.5,
     xlim=c(0,1),
     main='Accuracy per species',
     axes=F,
     ylab='Species',
     xlab='Value (%)')
axis(2, labels=unique(species$species),
     at=unique(as.numeric(position.sp)),
     las=2)
axis(1)
box()
points(position.sp~accuracy.gb, species,
       pch=rep(c(16,17,18,19), pch.vector),
       col=color.blue,
       cex=1.5,
       lwd=0.5)

#add reference line of mean
abline(v=mean(means.errs$mean.acc),
       lty=2,
       lwd=0.51)

#plot them
points(1:17~means.errs$mean.acc,
       pch=16,
       col='grey30',
       cex=2,
       lwd=0)
for (i in 1:17){
  lines(x=c((means.errs$mean.acc[i]-means.errs$se.acc[i]), (means.errs$mean.acc[i]+means.errs$se.acc[i])), 
        y=c(i,i),
        col='grey30')
}




#plot SENSITIVITY
plot(position.sp~sensitivity.rl, species,
     pch=rep(c(16,17,18,19), pch.vector),
     col=color.orange,
     lwd=0.5,
     cex=1.5,
     xlim=c(0,1),
     main='Sensitivity per species',
     axes=F,
     ylab='Species',
     xlab='Value (%)')
axis(2, labels=unique(species$species),
     at=unique(as.numeric(position.sp)),
     las=2)
axis(1)
box()
points(position.sp~sensitivity.gb, species,
       pch=rep(c(16,17,18,19), pch.vector),
       col=color.blue,
       cex=1.5,
       lwd=0.5)

#add reference line of mean
abline(v=mean(means.errs$mean.sen),
       lty=2,
       lwd=0.51)

#plot them
points(1:17~means.errs$mean.sen,
       pch=16,
       col='grey30',
       cex=2,
       lwd=0)
for (i in 1:17){
  lines(x=c((means.errs$mean.sen[i]-means.errs$se.sen[i]), (means.errs$mean.sen[i]+means.errs$se.sen[i])), 
        y=c(i,i),
        col='grey30')
}


#plot SPECIFICITY
plot(position.sp~specificity.rl, species,
     pch=rep(c(16,17,18,19), pch.vector),
     col=color.orange,
     lwd=0.5,
     cex=1.5,
     xlim=c(0,1),
     main='Specificity per species',
     axes=F,
     ylab='Species',
     xlab='Value (%)')
axis(2, labels=unique(species$species),
     at=unique(as.numeric(position.sp)),
     las=2)
axis(1)
box()
points(position.sp~specificity.gb, species,
       pch=rep(c(16,17,18,19), pch.vector),
       col=color.blue,
       cex=1.5,
       lwd=0.5)

#add reference line of mean
abline(v=mean(means.errs$mean.spe),
       lty=2,
       lwd=0.51)

#plot them
points(1:17~means.errs$mean.spe,
       pch=16,
       col='grey30',
       cex=2,
       lwd=0)
for (i in 1:17){
  lines(x=c((means.errs$mean.spe[i]-means.errs$se.spe[i]), (means.errs$mean.spe[i]+means.errs$se.spe[i])), 
        y=c(i,i),
        col='grey30')
}


##END OF CODE

