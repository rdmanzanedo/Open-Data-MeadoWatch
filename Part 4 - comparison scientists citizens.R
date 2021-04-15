############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

###################PART 4######################

##Figure 4: Data validation comparison of scientist vs. citizen scientists

#load report data
MW_PhenoDat_2013_2019 <- read.csv("MW_PhenoDat_2013_2019.csv")
head(MW_PhenoDat_2013_2019)

#graphical parameters
par(bty="o")
par(lwd=2)
par(tcl=-0.2)
par(las=1)
par(cex.lab=1.2)

#here we have to first pair each scientist obvservation with observations on the very same day (more flexible?)

#split in scientists and citizens
scientists = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$QA.QC ==1)
citizens = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$QA.QC == 0)

#and also in tracks, to pair identical situations
sci.gb = subset(scientists, scientists$Transect == "Glacier Basin")
cit.gb = subset(citizens, citizens$Transect == "Glacier Basin")

sci.rl = subset(scientists, scientists$Transect == "Reflection Lakes")
cit.rl = subset(citizens, citizens$Transect == "Reflection Lakes")


##GLACIER BASIN

#let's keep the variables of importance for each
sci.assessment = data.frame('Date' = sci.gb$Date,
                            'Species' = sci.gb$Species,
                            'Plot' = sci.gb$Site_Code,
                            'sci.Budding' = sci.gb$Bud,
                            'sci.Flowering' = sci.gb$Flower,
                            'sci.Fruiting' = sci.gb$Fruit,
                            'sci.Seeding' = sci.gb$Disperse)
non.sci.assessment = data.frame('Date' = cit.gb$Date,
                            'Species' = cit.gb$Species,
                            'Plot' = cit.gb$Site_Code,
                            'cit.Budding' = cit.gb$Bud,
                            'cit.Flowering' = cit.gb$Flower,
                            'cit.Fruiting' = cit.gb$Fruit,
                            'cit.Seeding' = cit.gb$Disperse)

#merge removing the incomparables (date, species, and plot need to be identical)
togetherness = merge(sci.assessment, non.sci.assessment,all = F)
head(togetherness)

print('hey')

#let's define a function to calculate some main agreement matrices 
#we can also introduce kappa, if that is more informative
agreement.sci.cit = function(dataset, phenostate){
  #define the phenostate to study
  if(phenostate=='budding') {a=0}
  if(phenostate=='flowering') {a=1}
  if(phenostate=='fruiting') {a=2}
  if(phenostate=='seeding') {a=3}
  #the a value determines the columns we are checking, as they go in that order
  #calculate confusion matrix elements and print them
  SY.CY = dim(subset(dataset, dataset[,4+a] == 1 & dataset[8+a] == 1))[1]
  SY.CN = dim(subset(dataset, dataset[,4+a] == 1 & dataset[8+a] == 0))[1]
  SN.CY = dim(subset(dataset, dataset[,4+a] == 0 & dataset[8+a] == 1))[1]
  SN.CN = dim(subset(dataset, dataset[,4+a] == 0 & dataset[8+a] == 0))[1]
  print(c('true pos =', SY.CY))
  print(c('sci.pos.cit.neg = ',SY.CN))
  print(c('sci.neg.cit.pos = ',SN.CY))
  print(c('true neg =', SN.CN))
  
  #calculate main metrics and print it, here we assume that
  #scientists define the 'true value'
  acc = ((SY.CY + SN.CN) / (SY.CY+SY.CN+SN.CY+SN.CN))
  sen = (SY.CY / (SY.CY+SN.CY))
  spe = (SN.CN / (SN.CN+SY.CN))
  print(c('accuracy =', acc))
  print(c('sensitivity =', sen))
  print(c('specificity =', spe))
}


agreement.sci.cit(togetherness,'budding')
agreement.sci.cit(togetherness,'flowering')
agreement.sci.cit(togetherness,'fruiting')
agreement.sci.cit(togetherness,'seeding')


#we can quickly check the differences between species, for example in flowering id

ls.sp = names(table(togetherness$Species))
for (i in 1:11){
  sps = subset(togetherness, togetherness$Species == ls.sp[i])
  print(ls.sp[i])
  agreement.sci.cit(sps, 'budding')
}

