############################DATA PAPER MEADOWATCH###################
###DATA COMPILED BY AJI JOHN
###FIGURES AND THIS ANALYSIS: RUBÉN D. MANZANEDO. APRIL 2021#########

###################PART 4######################

##Figure 4: Data validation comparison of scientist vs. citizen scientists

#load report data
MW_PhenoDat_2013_2019 <- read.csv("MW_PhenoDat_2013_2019.csv")
head(MW_PhenoDat_2013_2019)

#basic graphical parameters
par(bty="o")
par(lwd=2)
par(tcl=-0.2)
par(las=1)
par(cex.lab=1.2)

#here we have to first pair each scientist observation with observations on the very same day and track
#split in scientists and citizens
scientists = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$QA.QC ==1)
citizens = subset(MW_PhenoDat_2013_2019, MW_PhenoDat_2013_2019$QA.QC == 0)

#and also in tracks, to pair identical situations
sci.gb = subset(scientists, scientists$Transect == "Glacier Basin")
cit.gb = subset(citizens, citizens$Transect == "Glacier Basin")

sci.rl = subset(scientists, scientists$Transect == "Reflection Lakes")
cit.rl = subset(citizens, citizens$Transect == "Reflection Lakes")

######CONFUSION MATRICES AND ACCURACY PER TRAIL#######
#let's keep the variables of importance for each
sci.assessment.gb = data.frame('Date' = sci.gb$Date,
                            'Species' = sci.gb$Species,
                            'Plot' = sci.gb$Site_Code,
                            'sci.Budding' = sci.gb$Bud,
                            'sci.Flowering' = sci.gb$Flower,
                            'sci.Fruiting' = sci.gb$Fruit,
                            'sci.Seeding' = sci.gb$Disperse)
non.sci.assessment.gb = data.frame('Date' = cit.gb$Date,
                            'Species' = cit.gb$Species,
                            'Plot' = cit.gb$Site_Code,
                            'cit.Budding' = cit.gb$Bud,
                            'cit.Flowering' = cit.gb$Flower,
                            'cit.Fruiting' = cit.gb$Fruit,
                            'cit.Seeding' = cit.gb$Disperse)
#same for reflection lakes
sci.assessment.rl = data.frame('Date' = sci.rl$Date,
                               'Species' = sci.rl$Species,
                               'Plot' = sci.rl$Site_Code,
                               'sci.Budding' = sci.rl$Bud,
                               'sci.Flowering' = sci.rl$Flower,
                               'sci.Fruiting' = sci.rl$Fruit,
                               'sci.Seeding' = sci.rl$Disperse)
non.sci.assessment.rl = data.frame('Date' = cit.rl$Date,
                                   'Species' = cit.rl$Species,
                                   'Plot' = cit.rl$Site_Code,
                                   'cit.Budding' = cit.rl$Bud,
                                   'cit.Flowering' = cit.rl$Flower,
                                   'cit.Fruiting' = cit.rl$Fruit,
                                   'cit.Seeding' = cit.rl$Disperse)

#merge removing incomparables (date, species, and plot need to be identical match)
togetherness.gb = merge(sci.assessment.gb, non.sci.assessment.gb,all = F)
togetherness.rl = merge(sci.assessment.rl, non.sci.assessment.rl, all=F)


#FUNCTION to calculate confusion matrix and some main agreement metrics#

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
  #scientists define the 'true value' for the sake of the confusion matrix
  acc = ((SY.CY + SN.CN) / (SY.CY+SY.CN+SN.CY+SN.CN))
  sen = (SY.CY / (SY.CY+SY.CN))
  spe = (SN.CN / (SN.CN+SN.CY))
  print(c('accuracy =', acc))
  print(c('sensitivity =', sen))
  print(c('specificity =', spe))
}

#calculate for each state
agreement.sci.cit(togetherness.gb,'budding')
agreement.sci.cit(togetherness.gb,'flowering')
agreement.sci.cit(togetherness.gb,'fruiting')
agreement.sci.cit(togetherness.gb,'seeding')

agreement.sci.cit(togetherness.rl,'budding')
agreement.sci.cit(togetherness.rl,'flowering')
agreement.sci.cit(togetherness.rl,'fruiting')
agreement.sci.cit(togetherness.rl,'seeding')

#tables saved to 'accuracies and confusion matrices(from part 4 script).xlsx


#####ACCURACZ PER SPECIES#######
#differences between species, for example in flowering id but in all

ls.sp.gb = names(table(togetherness.gb$Species))
ls.sp.rl = names(table(togetherness.rl$Species))
#GB
for (i in 1:11){
  sps = subset(togetherness.gb, togetherness.gb$Species == ls.sp.gb[i])
  print(ls.sp.gb[i])
  agreement.sci.cit(sps, 'budding')
  agreement.sci.cit(sps, 'flowering')
  agreement.sci.cit(sps, 'fruiting')
  agreement.sci.cit(sps, 'seeding')
}

#RL
for(i in 1:11){
  sps = subset(togetherness.rl, togetherness.rl$Species == ls.sp.rl[i])
  print(ls.sp.rl[i])
  agreement.sci.cit(sps, 'budding')
  agreement.sci.cit(sps, 'flowering')
  agreement.sci.cit(sps, 'fruiting')
  agreement.sci.cit(sps, 'seeding')
}

#tables saved to 'accuracies and confusion matrices(from part 4 script).xlsx
##END OF CODE#####