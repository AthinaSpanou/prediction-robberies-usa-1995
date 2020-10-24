library(car)
library(caret)
library(corrplot)
library(dplyr)
library(forcats)
library(ggplot2)
library(ggrepel)
library(glmnet)
library(gvlma)
library(Hmisc)
library(lars)
library(lawstat)
library(MASS)
library(nortest)
library(psych)
library(summarytools)
library(scales)
library(usmap)
library(vioplot)

setwd("D:/MSc Business Analytics/Statistics I/Main_Assignment")

dataset <- read.table("crime_40.dat", sep=",", header=F)

#_________________________________________________________________________________________________________________
#### DATA CLEANING & DATA TRANSFORMATION ####

dataset <- setNames(dataset, c('communityname','state','countyCode','communityCode','fold','population','householdsize',
                               'racepctblack','racePctWhite','racePctAsian','racePctHisp','agePct12t21','agePct12t29',
                               'agePct16t24','agePct65up','numbUrban','pctUrban','medIncome','pctWWage','pctWFarmSelf',
                               'pctWInvInc','pctWSocSec','pctWPubAsst','pctWRetire','medFamInc','perCapInc','whitePerCap',
                               'blackPerCap','indianPerCap','AsianPerCap','OtherPerCap','HispPerCap','NumUnderPov',
                               'PctPopUnderPov','PctLess9thGrade','PctNotHSGrad','PctBSorMore','PctUnemployed',
                               'PctEmploy','PctEmplManu','PctEmplProfServ','PctOccupManu','PctOccupMgmtProf',
                               'MalePctDivorce','MalePctNevMarr','FemalePctDiv','TotalPctDiv','PersPerFam','PctFam2Par',
                               'PctKids2Par','PctYoungKids2Par','PctTeen2Par','PctWorkMomYoungKids','PctWorkMom',
                               'NumKidsBornNeverMar','PctKidsBornNeverMar','NumImmig','PctImmigRecent','PctImmigRec5',
                               'PctImmigRec8','PctImmigRec10','PctRecentImmig','PctRecImmig5','PctRecImmig8',
                               'PctRecImmig10','PctSpeakEnglOnly','PctNotSpeakEnglWell','PctLargHouseFam','PctLargHouseOccup',
                               'PersPerOccupHous','PersPerOwnOccHous','PersPerRentOccHous','PctPersOwnOccup','PctPersDenseHous',
                               'PctHousLess3BR','MedNumBR','HousVacant','PctHousOccup','PctHousOwnOcc','PctVacantBoarded',
                               'PctVacMore6Mos','MedYrHousBuilt','PctHousNoPhone','PctWOFullPlumb','OwnOccLowQuart',
                               'OwnOccMedVal','OwnOccHiQuart','OwnOccQrange','RentLowQ','RentMedian','RentHighQ',
                               'RentQrange','MedRent','MedRentPctHousInc','MedOwnCostPctInc','MedOwnCostPctIncNoMtg',
                               'NumInShelters','NumStreet','PctForeignBorn','PctBornSameState','PctSameHouse85',
                               'PctSameCity85','PctSameState85','LemasSwornFT','LemasSwFTPerPop','LemasSwFTFieldOps',
                               'LemasSwFTFieldPerPop','LemasTotalReq','LemasTotReqPerPop','PolicReqPerOffic',
                               'PolicPerPop','RacialMatchCommPol','PctPolicWhite','PctPolicBlack','PctPolicHisp',
                               'PctPolicAsian','PctPolicMinor','OfficAssgnDrugUnits','NumKindsDrugsSeiz','PolicAveOTWorked',
                               'LandArea','PopDens','PctUsePubTrans','PolicCars','PolicOperBudg','LemasPctPolicOnPatr',
                               'LemasGangUnitDeploy','LemasPctOfficDrugUn','PolicBudgPerPop','murders','murdPerPop',
                               'rapes','rapesPerPop','robberies','robbbPerPop','assaults','assaultPerPop','burglaries',
                               'burglPerPop','larcenies','larcPerPop','autoTheft','autoTheftPerPop','arsons',
                               'arsonsPerPop','ViolentCrimesPerPop','nonViolPerPop'))

dataset[dataset=="?"] <- NA

str(dataset)

colSums(sapply(dataset, is.na))

dataset <- subset(dataset, select=-c(LemasSwornFT, LemasSwFTPerPop, LemasSwFTFieldOps, LemasSwFTFieldPerPop, LemasTotalReq, 
                                      LemasTotReqPerPop, PolicReqPerOffic, PolicPerPop, RacialMatchCommPol, PctPolicWhite, 
                                      PctPolicBlack, PctPolicHisp, PctPolicAsian, PctPolicMinor,OfficAssgnDrugUnits, 
                                      NumKindsDrugsSeiz, PolicAveOTWorked, PolicCars, PolicOperBudg, LemasPctPolicOnPatr,  
                                      LemasGangUnitDeploy, PolicBudgPerPop))

colSums(sapply(dataset, is.na))
datasetall <- dataset

# Removing communityname, countyCode, communityCode, fold since they are non-predictive

dataset <- subset(dataset, select=-c(communityname,countyCode,communityCode,fold))

#Transform factor variables to numeric

sapply(dataset,class)
dataset$rapes <- as.numeric(as.character(dataset$rapes))
dataset$rapesPerPop <- as.numeric(as.character(dataset$rapesPerPop))
dataset$assaults <- as.numeric(as.character(dataset$assaults))
dataset$assaultPerPop <- as.numeric(as.character(dataset$assaultPerPop))
dataset$arsons <- as.numeric(as.character(dataset$arsons))
dataset$arsonsPerPop <- as.numeric(as.character(dataset$arsonsPerPop))
dataset$ViolentCrimesPerPop <- as.numeric(as.character(dataset$ViolentCrimesPerPop))
dataset$nonViolPerPop <- as.numeric(as.character(dataset$nonViolPerPop))

# Transform integer variables to numeric
columns <-c("population","numbUrban","medIncome","blackPerCap","whitePerCap","perCapInc","medFamInc",
            "indianPerCap", "AsianPerCap", "OtherPerCap", "HispPerCap", "NumUnderPov","NumKidsBornNeverMar",
            "NumImmig","MedNumBR","HousVacant","MedYrHousBuilt", "OwnOccLowQuart","OwnOccMedVal",
            "OwnOccHiQuart", "OwnOccQrange","RentLowQ", "RentMedian", "RentHighQ",
            "RentQrange", "MedRent","NumInShelters","NumStreet")
dataset[, columns] <- lapply(columns, function(x) as.numeric(dataset[[x]]))
sapply(dataset,class)

# Removing the rest violent and non-violent crimes as potential dependent variables

violent <- subset(dataset, select=c(murders, murdPerPop, rapes, rapesPerPop, robberies, robbbPerPop,
                                     assaults, assaultPerPop, ViolentCrimesPerPop))
nonViolent <- subset(dataset, select=c(burglaries, burglPerPop, larcenies, larcPerPop, autoTheft, autoTheftPerPop,
                                        arsons, arsonsPerPop,nonViolPerPop ))

dataset <- subset(dataset, select =-c(murders,murdPerPop,rapes,rapesPerPop, robberies,assaults, assaultPerPop, 
                                      ViolentCrimesPerPop,burglaries, burglPerPop, larcenies, larcPerPop, autoTheft, 
                                      autoTheftPerPop,arsons, arsonsPerPop,nonViolPerPop ))
colSums(sapply(dataset, is.na))

#_________________________________________________________________________________________________________________
#### DESCRIPTIVE ANALYSIS FOR CATEGORICAL VARIABLES ####

plot_usmap(data = datasetall, values = "robbbPerPop", color = "black",labels = TRUE ) + 
  scale_fill_continuous(name = "Robberies per 100k (1995)", 
                        low = "mistyrose",
                        high = "darkmagenta", labels = c("0", "250", "500", "750", "1000"),
                        breaks = c(0, 250, 500, 750, 1000)
  )+ labs(title = "Number of Robberies per 100k in 1995", caption="States in Grey were not found in the dataset") + 
  theme(plot.title = element_text(face = "bold",hjust = 0.5,size=12), legend.position = "right",
        plot.caption = element_text(color = "blue", face = "italic",hjust=0.5,size=10))

#_________________________________________________________________________________________________________________
#### DESCRIPTIVE ANALYSIS FOR NUMERICS VARIABLES ####

numerics <- sapply(dataset, class) == "numeric"
numerics <- dataset[,numerics]
round(t(describe(numerics)),2)

# Summary Statistics for numeric variables
Hmisc::describe(numerics)
# Summary Statistics for robberies per 100k
summarytools::descr(numerics$robbbPerPop, transpose = TRUE)

# Barplot
violent <- violent[!is.na(violent$ViolentCrimesPerPop),]
colSums(sapply(violent, is.na))
round((sum(violent$murdPerPop,na.rm=TRUE)/sum(violent$ViolentCrimesPerPop,na.rm=TRUE)),2)*100
round((sum(violent$rapesPerPop,na.rm=TRUE)/sum(violent$ViolentCrimesPerPop,na.rm=TRUE)),2)*100
round((sum(violent$robbbPerPop,na.rm=TRUE)/sum(violent$ViolentCrimesPerPop,na.rm=TRUE)),2)*100
round((sum(violent$assaultPerPop,na.rm=TRUE)/sum(violent$ViolentCrimesPerPop,na.rm=TRUE)),2)*100

df <- data.frame(
  group = c("Murder", "Rape", "Robbery","Assault"),
  value = c(1, 6, 27,66)
)
head(df)

df %>%
  arrange(desc(value)) %>%
  mutate(prop = percent(value / sum(value))) -> df 

pie <- ggplot(df, aes(x = "", y = value, fill = fct_inorder(group))) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette="PuRd")+
  theme(axis.text.x=element_blank())+
  geom_label_repel(aes(label = prop), size=5, show.legend = F, nudge_x = 1) +
  guides(fill = guide_legend(title = "Crimes per 100k"))
pie

#---------------Plot 1---------------------

length1<-(seq(1,12))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length1){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length1){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 2---------------------

length2<-(seq(13,24))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length2){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length2){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 3---------------------

length3<-(seq(25,36))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length3){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length3){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 4---------------------

length4<-(seq(37,48))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length4){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length4){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 5---------------------

length5<-(seq(49,60))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length5){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length5){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 6---------------------

length6<-(seq(61,72))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length6){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length6){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 7---------------------

length7<-(seq(73,84))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length7){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length7){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 8---------------------

length8<-(seq(85,96))
par(mfrow=c(3,4));n <- nrow(numerics)
for (i in length8){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,4))
for( i in length8){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}

#---------------Plot 9---------------------

length9<-(seq(97,103))
par(mfrow=c(3,3));n <- nrow(numerics)
for (i in length9){
  hist(numerics[,i], main=names(numerics)[i], probability=TRUE, xlab=colnames(numerics[i]))
  lines(density(numerics[,i]),col = "deeppink", lwd=2 )
  index <- seq( min(numerics[,i]), max(numerics[,i]),
                length.out=100)
  ynorm <- dnorm( index, mean=mean(numerics[,i]),
                  sd(numerics[,i]) )
  lines( index, ynorm, col="blue", lty=3, lwd=2 )
}

par(mfrow=c(3,3))
for( i in length9){
  qqnorm(numerics[,i], main=paste('QQPlot of', colnames(numerics[i])), 
         xlab=colnames(numerics[i]))
  qqline(numerics[,i], col='deeppink',lwd=2)
}


#_________________________________________________________________________________________________________________
#### OUTLIERS ####

outliers<-dataset[2:104]
for (i in 2:104){
  datatest<-dataset[,i]
  out <- boxplot( datatest, plot=FALSE )$out
  if(length(out)!=0){
    print('-------------------------------------------------------')
    print( paste('Outliers for variable', names(dataset)[i] ) )
    print( paste(length(out), 'outliers') )
    print( paste(round(100*length(out)/sum(!is.na(dataset)),1),
                 '% outliers', sep='' ) )
    print(which( datatest %in% out ))
  }
}

# No variable has outliers more than 0.1% of data 

#_________________________________________________________________________________________________________________
#### NORMALITY TEST ####

# Shapiro-Wilk Test
lengthi<-(1:length(numerics));lengthi
count1<-0
for (i in lengthi){ 
  if (shapiro.test(numerics[,i])$p.value<0.05){
    print(paste('For variable', colnames(numerics[i]),'P-value',shapiro.test(numerics[,i])$p.value, 'is less than a=0.05, thus the nuss hypothesis of normally distributed data is rejected'))}
  else{
    print(paste('For variable', colnames(numerics[i]),'P-value',shapiro.test(numerics[,i])$p.value, 'is greater than a=0.05, thus the nuss hypothesis of normally distributed data is not rejected'))
    count1<-count1+1}
}

# Kolmogorov-Smirnov Test
lengthi<-(1:length(numerics));lengthi
count2<-0
for (i in lengthi){ 
    if (lillie.test(numerics[,i])$p.value<0.05){
    print(paste('For variable', colnames(numerics[i]),'P-value',lillie.test(numerics[,i])$p.value, 'is less than a=0.05, thus the nuss hypothesis of normally distributed data is rejected'))}
  else{
    print(paste('For variable', colnames(numerics[i]),'P-value',lillie.test(numerics[,i])$p.value, 'is greater than a=0.05, thus the nuss hypothesis of normally distributed data is not rejected'))
    count2<-count2+1}
}

# Symmetry Test
lengthi<-(1:length(numerics));length_i
count3 <- 0 
for (i in length_i){ 
  if (symmetry.test(numerics[,i])$p.value<0.05){
    print(paste('In column', colnames(numerics[i]),'P-value',symmetry.test(numerics[,i])$p.value, 'is less than a=0.05, thus I reject the null hypothesis that the distribution is assymmetric'))}
  else{
    print(paste('In column', colnames(numerics[i]),'P-value',symmetry.test(numerics[,i])$p.value, 'is greater than a=0.05, thus I do not reject the null hypothesis that the distribution is assymmetric'))
    count3 <- count3 +1 }
}

#### Two sample Hypothesis testing (1 continous and 1 categorical variable) ####

# Question: Is the number of robberies per 100k equal in states California (CA) and Texas (TX)?

ca<-datasetall$robbbPerPop[which(datasetall$state=="CA")];length(ca)
tx<-datasetall$robbbPerPop[which(datasetall$state=="TX")];length(tx)

#Normality, n<50
shapiro.test(ca)
shapiro.test(tx)

par(mfrow=c(1,2))
hist(ca, main="Robberries per 100k of California state")
qqnorm(ca, main="Robberries per 100k of California state")
qqline(ca)

hist(tx, main="Robberries per 100k of Texas state")
qqnorm(tx, main="Robberries per 100k of Texas state")
qqline(tx)

symmetry.test(ca, boot=F)
symmetry.test(tx, boot=F)

wilcox.test(ca,tx)

par(mfrow=c(1,1))
boxplot(ca, tx,  names=c("CA","TX"), ylab='Robberies per 100k on two Sates', ylim=c(0,900))

#We do not accept H0: M1 = M2 => Significant difference is found about the 
#median of robberies per 100k between California and Texas.

#_________________________________________________________________________________________________________________
#### HIGH CORRELATED VARIABLES ####

# Is any variable correlated to robperpop??
# keep only attributes with correlation to price greater/less than 0.25/-0.25 

names(numerics)
colSums(sapply(numerics, is.na))

correlations <- cor(numerics)
match("robbbPerPop",names(numerics))
cor <- correlations[,103] #robperpop
summary(cor)
cor<- data.frame(as.list(cor))
highcor<-cor[,colSums(cor > 0.25 | cor < -0.25) >= 1]
names(highcor)
highcor<-subset(numerics, select=c( racepctblack, racePctWhite, pctWFarmSelf, pctWInvInc, pctWPubAsst, PctPopUnderPov,
                                           PctNotHSGrad, PctBSorMore, PctUnemployed, PctOccupManu, PctOccupMgmtProf,
                                           MalePctDivorce, FemalePctDiv, TotalPctDiv, PctFam2Par, PctKids2Par, 
                                           PctYoungKids2Par,PctTeen2Par, NumKidsBornNeverMar, PctKidsBornNeverMar, 
                                           PctImmigRec5, PctImmigRec8,PctImmigRec10, PctRecImmig5, PctRecImmig8, 
                                           PctRecImmig10, PctLargHouseFam, PersPerRentOccHous, PctPersOwnOccup, 
                                           PctPersDenseHous, PctHousOwnOcc, PctVacantBoarded, PctHousNoPhone, 
                                           MedRentPctHousInc, NumStreet, LemasPctOfficDrugUn, robbbPerPop))

#_________________________________________________________________________________________________________________
#### PAIRWISE ASSOCIATIONS ####

library(corrplot)

#Correlation applying the Pearson's Correlation test: Identify Linear dependent variables
par(mfrow=c(1,1))
#group of high correlated variables, method: ellipse
corrplot(cor(numerics[1:11]), type = "upper", tl.pos = "td",
         method = "ellipse", tl.cex = 0.55, tl.col = 'black',
         order = "hclust", diag = T)

#________________________________________________________________________________________________
#### LASSO ####

# Lars lasso
tolasso <-highcor
mfull <- lm(robbbPerPop~.,data=tolasso)
X<-model.matrix(mfull)[,-1]
lasso1 <- lars( X, tolasso$robbbPerPop ) 
plot(lasso1, xvar='n')
plot(lasso1, xvar='n', breaks=F)
plot(lasso1, xvar='n', breaks=F, xlim=c(0.5,1), ylim=c(-20,15) )
plot(lasso1, xvar='df')
plot(lasso1, xvar='arc')
plot(lasso1, xvar='step')
res.cv <- cv.lars( X, tolasso$robbbPerPop ) # default model='fraction'
lambda<-res.cv$index
cv    <-res.cv$cv
mincv.s <- lambda[cv==min(cv)]
coef( lasso1, s=mincv.s, mode='fraction' )

rescp<-summary(lasso1)
coef(lasso1, s=which.min(rescp$Cp), mode="step")
plot(lasso1, xvar='n', plottype='Cp')

#________________________________________________________________________________________________
# Glmnet lasso
to_lasso<-highcor
colSums(sapply(to_lasso, is.na))
to_lasso[, 1:37] <- sapply(to_lasso[,1:37],as.numeric)
str(to_lasso)
mfull <- lm(robbbPerPop~.,data=to_lasso)
X <- model.matrix(mfull)[,-1]
lasso2 <- glmnet(X, to_lasso$robbbPerPop)
par(mfrow=c(1,1))
plot(lasso2, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda
lasso2 <- cv.glmnet(X, to_lasso$robbbPerPop, alpha = 1)
lasso2$lambda
lasso2$lambda.min
lasso2$lambda.1se
plot(lasso2)

coef(lasso2, s = "lambda.min")
coef(lasso2, s = "lambda.1se")

plot(lasso2$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso2$lambda.min, lasso2$lambda.1se)), lty =2)

#________________________________________________________________________________________________
#### FITTING THE REGRESSION MODEL ####

regressionmodel <- subset(highcor, select=c(racepctblack,racePctWhite,PctKidsBornNeverMar,robbbPerPop))

# Examine correlations
corrplot(cor(regressionmodel[1:4]), type = "upper", tl.pos = "td",
         method = "ellipse", tl.cex = 0.80, tl.col = 'black',
         order = "hclust", diag = T)

# Exploratory graphs for the selected attributes
eda.plots <- function(data, ask=F){
  graphics.off()
  numeric.only <- sapply(data,class)=='numeric'
  y <- data[,numeric.only]
  n<-ncol(y)
  for (i in 1:n){
    if (!ask) win.graph()
    par(mfrow=c(2,2), ask=ask)
    y1 <- y[,i]
    vioplot(y1, col="deeppink",main=names(y)[i])
    hist(y1, probability=TRUE, main=names(y)[i])
    lines(density(y1), col="deeppink",lwd=2)
    qqnorm(y1, main=names(y)[i])
    qqline(y1)
    boxplot(y1, main=names(y)[i], horizontal=TRUE, col="lightgrey")
  }
}
graphics.off()
eda.plots(regressionmodel, ask=T)

# Initial Model
initial_model <- lm(robbbPerPop~.,data=regressionmodel)
summary(initial_model)
par(mfrow=c(2,2))
plot(initial_model,ask=F)
spreadLevelPlot(initial_model)

round(vif(initial_model),2)

# Assessing Outliers
outlierTest(initial_model) # Bonferonni p-value for most extreme obs

# We can use a quantile comparison plots to compare the distribution of thestudentized residuals from our regression model 
# to thet-distribution. Observations that stray outside of the 95% confidence envelope are statistically significant outliers
qqPlot(initial_model, main="QQ Plot", col.line="deeppink",simulate=T) #qq plot for studentized resid

# added variable plots 
avPlots(initial_model, col.lines="deeppink") # leverage plots 

#________________________________________________________________________________________________
#### TEST ASSUMPTIONS OF THE INITIAL FULL MODEL ####

# ___________________
# Global test of model assumptions
# ___________________
gvmodel <- gvlma(initial_model) 
summary(gvmodel)

# ___________________
# Non Linearity
# ___________________
par(mfrow=c(1,1))
residualPlot(initial_model, type='rstudent', col.quad="deeppink") 
residualPlots(initial_model, plot=F, type = "rstudent") #initial model passes
crPlots(initial_model)

ggscatter(regressionmodel, x = "robbbPerPop", y = "racepctblack", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "robbbPerPop", ylab = "racepctblack",
          add.params = list(color = "deeppink",
                            fill = "lightgray"))

ggscatter(regressionmodel, x = "robbbPerPop", y = "racePctWhite", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "robbbPerPop", ylab = "racePctWhite",
          add.params = list(color = "deeppink",
                            fill = "lightgray"))

ggscatter(regressionmodel, x = "robbbPerPop", y = "PctKidsBornNeverMar", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "robbbPerPop", ylab = "PctKidsBornNeverMar",
          add.params = list(color = "deeppink",
                            fill = "lightgray"))

# ___________________
# Independence 
# ___________________
plot(rstudent(initial_model), type='l', col='deeppink', lwd=2.5)
library(randtests); runs.test(initial_model$res) 
library(lmtest);dwtest(initial_model) 
#D-W statistic is between the desirable margins (1.4-2.6) while the p-value is very high.
library(car); durbinWatsonTest(initial_model)  #initial model passes

# ___________________
# Equality of variances (Homoscedasticity) 
# ___________________
yhat <- fitted(initial_model)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(initial_model)~yhat.quantiles) # model does not pass

#Equlaity of variance is not met
boxplot(rstudent(initial_model)~yhat.quantiles)

# ___________________
# Normality Assumption
# ___________________

plot(initial_model, which = 2) #Normality of the residuals (step 1)
shapiro.test(initial_model$residuals) # model does not pass 
lillie.test(initial_model$residuals) # model does not pass
ad.test(initial_model$residuals) # model does not pass
Stud.residuals <- rstudent(initial_model)
yhat <- fitted(initial_model)
par(mfrow=c(1,1))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

#Summary of the final 4 plots
par(mfrow=c(2,2))
plot( initial_model, 2, col="deeppink")
plot( initial_model, 3)
residualPlot(initial_model, type='rstudent',col.quad="deeppink",lwd=2 )
plot(rstudent(initial_model), type='l', col="deeppink",lwd=2)

#________________________________________________________________________________________________
#### STEPWISE REGRESSION ####

# Stepwise Model
aicmodel <- lm(robbbPerPop~.,data=regressionmodel)
summary(step(aicmodel), direction="both")
aicmodel <- lm(robbbPerPop~+racePctWhite+PctKidsBornNeverMar,data=regressionmodel)

round(vif(aicmodel),2)
summary(aicmodel)
par(mfrow=c(2,2))
plot(aicmodel,ask=F)
par(mfrow=c(1,1))
spreadLevelPlot(aicmodel)


# Assessing Outliers
outlierTest(aicmodel) # Bonferonni p-value for most extreme obs

# We can use a quantile comparison plots to compare the distribution of thestudentized residuals from our regression model 
# to thet-distribution. Observations that stray outside of the 95% confidence envelope are statistically significant outliers
qqPlot(aicmodel, main="QQ Plot", col.line="deeppink",simulate=T) #qq plot for studentized resid

# added variable plots 
avPlots(aicmodel, col.lines="deeppink") # leverage plots 

#________________________________________________________________________________________________
#### TEST ASSUMPTIONS OF THE STEPWISE FULL MODEL ####

# ___________________
# Global test of model assumptions
# ___________________
gvmodel <- gvlma(aicmodel) 
summary(gvmodel)

# ___________________
# Non Linearity
# ___________________
par(mfrow=c(1,1))
residualPlot(aicmodel, type='rstudent', col.quad="deeppink") 
residualPlots(aicmodel, plot=F, type = "rstudent") #initial model passes
crPlots(aicmodel)

# ___________________
# Independence 
# ___________________
plot(rstudent(aicmodel), type='l', col='deeppink', lwd=2.5)
library(randtests); runs.test(aicmodel$res) 
library(lmtest);dwtest(aicmodel) 
#D-W statistic is between the desirable margins (1.4-2.6) while the p-value is very high.
library(car); durbinWatsonTest(aicmodel)  # model passes

# ___________________
# Equality of variances (Homoscedasticity) 
# ___________________
yhat <- fitted(aicmodel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(aicmodel)~yhat.quantiles) # model does not pass

#Equlaity of variance is not met
boxplot(rstudent(aicmodel)~yhat.quantiles)

# ___________________
# Normality Assumption
# ___________________

plot(aicmodel, which = 2) #Normality of the residuals (step 1)
shapiro.test(aicmodel$residuals) # model does not pass 
lillie.test(aicmodel$residuals) # model does not pass
ad.test(aicmodel$residuals) # model does not pass
Stud.residuals <- rstudent(aicmodel)
yhat <- fitted(aicmodel)
par(mfrow=c(1,1))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

#Summary of the final 4 plots
par(mfrow=c(2,2))
plot( aicmodel, 2, col="deeppink")
plot( aicmodel, 3)
residualPlot(aicmodel, type='rstudent',col.quad="deeppink",lwd=2 )
plot(rstudent(aicmodel), type='l', col="deeppink",lwd=2)

#________________________________________________________________________________________________
#### LOG TRANSFORMATION ####

# lOG Model
logmodel <- lm(log(robbbPerPop+1)~.,data=regressionmodel)
summary(step(logmodel), direction="both")
logmodel <- lm(log(robbbPerPop+1)~+racePctWhite+log(PctKidsBornNeverMar),data=regressionmodel)
summary(logmodel)

round(vif(logmodel),2)

par(mfrow=c(2,2))
plot(logmodel,ask=F)
par(mfrow=c(1,1))
spreadLevelPlot(logmodel)

# Assessing Outliers
outlierTest(logmodel) # Bonferonni p-value for most extreme obs

# We can use a quantile comparison plots to compare the distribution of thestudentized residuals from our regression model 
# to thet-distribution. Observations that stray outside of the 95% confidence envelope are statistically significant outliers
qqPlot(logmodel, main="QQ Plot", col.line="deeppink",simulate=T) #qq plot for studentized resid

# added variable plots 
avPlots(logmodel, col.lines="deeppink") # leverage plots 

#________________________________________________________________________________________________
#### TEST ASSUMPTIONS OF THE LOG MODEL ####

# ___________________
# Global test of model assumptions
# ___________________
gvmodel <- gvlma(logmodel) 
summary(gvmodel)

# ___________________
# Non Linearity
# ___________________
par(mfrow=c(1,1))
residualPlot(logmodel, type='rstudent', col.quad="deeppink",ask=F) 
residualPlots(logmodel, plot=F, type = "rstudent")
crPlots(logmodel)

# ___________________
# Independence 
# ___________________

plot(rstudent(logmodel), type='l', col='deeppink', lwd=2.5)
library(randtests); runs.test(logmodel$res) 
library(lmtest);dwtest(logmodel) 
#D-W statistic is between the desirable margins (1.4-2.6) while the p-value is very high.
library(car); durbinWatsonTest(logmodel)  # model passes

# ___________________
# Equality of variances (Homoscedasticity) 
# ___________________

yhat <- fitted(logmodel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(logmodel)~yhat.quantiles) # model passes

#Equlaity of variance is not met
boxplot(rstudent(logmodel)~yhat.quantiles)

# ___________________
# Normality Assumption
# ___________________

plot(logmodel, which = 2) #Normality of the residuals (step 1)
shapiro.test(logmodel$residuals) # model does not pass 
lillie.test(logmodel$residuals) # model passes
ad.test(logmodel$residuals) # model passes
Stud.residuals <- rstudent(logmodel)
yhat <- fitted(logmodel)
par(mfrow=c(1,1))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

#Summary of the final 4 plots
par(mfrow=c(2,2))
plot( logmodel, 2, col="deeppink")
plot( logmodel, 3)
residualPlot(logmodel, type='rstudent',col.quad="deeppink",lwd=2 )
plot(rstudent(logmodel), type='l', col="deeppink",lwd=2)

#________________________________________________________________________________________________
#### POLYNOMIAL TRANSFORMATION #### 

polymodel <- lm(log(robbbPerPop+1)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=regressionmodel)
summary(step(polymodel, direction='both'))
round(vif(polymodel),2)

par(mfrow=c(2,2))
plot(polymodel,ask=F)
par(mfrow=c(1,1))
spreadLevelPlot(polymodel)

# Assessing Outliers
outlierTest(polymodel) # Bonferonni p-value for most extreme obs

# We can use a quantile comparison plots to compare the distribution of thestudentized residuals from our regression model 
# to thet-distribution. Observations that stray outside of the 95% confidence envelope are statistically significant outliers
qqPlot(polymodel, main="QQ Plot", col.line="deeppink",simulate=T) #qq plot for studentized resid

# added variable plots 
avPlots(polymodel, col.lines="deeppink") # leverage plots 

#________________________________________________________________________________________________
#### TEST ASSUMPTIONS OF THE POLYNOMIAL MODEL ####

# ___________________
# Global test of model assumptions
# ___________________
gvmodel <- gvlma(polymodel) 
summary(gvmodel)

# ___________________
# Non Linearity
# ___________________
par(mfrow=c(1,1))
residualPlot(polymodel, type='rstudent', col.quad="deeppink") 
residualPlots(polymodel, plot=F, type = "rstudent")
crPlots(polymodel)

# ___________________
# Independence 
# ___________________
plot(rstudent(polymodel), type='l', col='deeppink', lwd=2.5)
library(randtests); runs.test(polymodel$res) 
library(lmtest);dwtest(polymodel) 
#D-W statistic is between the desirable margins (1.4-2.6) while the p-value is very high.
library(car); durbinWatsonTest(polymodel)  # model passes

# ___________________
# Equality of variances (Homoscedasticity) 
# ___________________
yhat <- fitted(polymodel)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(polymodel)~yhat.quantiles) # model passes

#Equlaity of variance is not met
boxplot(rstudent(polymodel)~yhat.quantiles)

# ___________________
# Normality Assumption
# ___________________

plot(polymodel, which = 2) #Normality of the residuals (step 1)
shapiro.test(polymodel$residuals) # model passes 
lillie.test(polymodel$residuals) # model passes
ad.test(polymodel$residuals) # model passes
Stud.residuals <- rstudent(polymodel)
yhat <- fitted(polymodel)
par(mfrow=c(1,1))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

#Summary of the final 4 plots
par(mfrow=c(2,2))
plot( polymodel, 2, col="deeppink")
plot( polymodel, 3, col.line="deeppink")
residualPlot(polymodel, type='rstudent',col.quad="deeppink",lwd=2 )
plot(rstudent(polymodel), type='l', col="deeppink",lwd=2)

#________________________________________________________________________________________________
#### BOX COX TRANSFORMATION #### 

boxcox<-regressionmodel[(!regressionmodel$robbbPerPop==0),]
m <- lm(log(robbbPerPop+1)~+(racePctWhite)+(PctKidsBornNeverMar),data=regressionmodel)

# run the box-cox transformation
bc <- boxcox(log(robbbPerPop+1)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=boxcox)

(lambda <- bc$x[which.max(bc$y)])

powerTransform <- function(y, lambda1, lambda2 = NULL, method = "boxcox") {
  
  boxcoxTrans <- function(x, lam1, lam2 = NULL) {
    
    # if we set lambda2 to zero, it becomes the one parameter transformation
    lam2 <- ifelse(is.null(lam2), 0, lam2)
    
    if (lam1 == 0L) {
      log(y + lam2)
    } else {
      (((y + lam2)^lam1) - 1) / lam1
    }
  }
  
  switch(method
         , boxcox = boxcoxTrans(y, lambda1, lambda2)
         , tukey = y^lambda1
  )
}

boxcox_model <- lm(powerTransform(log(robbbPerPop),lambda)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=boxcox)
summary(step(boxcox_model, direction='both'))
round(vif(boxcox_model),2)
par(mfrow=c(1,1))
spreadLevelPlot(boxcox_model)
qqnorm(boxcox_model$residuals); qqline(boxcox_model$residuals)
par(op)

#________________________________________________________________________________________________
#### TEST ASSUMPTIONS OF THE BOX-COX MODEL ####

# ___________________
# Global test of model assumptions
# ___________________
gvmodel <- gvlma(boxcox_model) 
summary(gvmodel)

# ___________________
# Non Linearity
# ___________________
par(mfrow=c(1,1))
residualPlot(boxcox_model, type='rstudent', col.quad="deeppink") 
residualPlots(boxcox_model, plot=F, type = "rstudent")
crPlots(boxcox_model)

# ___________________
# Independence 
# ___________________
plot(rstudent(boxcox_model), type='l', col='deeppink', lwd=2.5)
library(randtests); runs.test(boxcox_model$res) 
library(lmtest);dwtest(boxcox_model) 
#D-W statistic is between the desirable margins (1.4-2.6) while the p-value is very high.
library(car); durbinWatsonTest(boxcox_model)  # model passes

# ___________________
# Equality of variances (Homoscedasticity) 
# ___________________
yhat <- fitted(boxcox_model)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(boxcox_model)~yhat.quantiles) # model passes

#Equlaity of variance is not met
boxplot(rstudent(boxcox_model)~yhat.quantiles)

# ___________________
# Normality Assumption
# ___________________

plot(boxcox_model, which = 2) #Normality of the residuals (step 1)
shapiro.test(boxcox_model$residuals) # model passes 
lillie.test(boxcox_model$residuals) # model passes
ad.test(boxcox_model$residuals) # model passes
Stud.residuals <- rstudent(boxcox_model)
yhat <- fitted(boxcox_model)
par(mfrow=c(1,1))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

#Summary of the final 4 plots
par(mfrow=c(2,2))
plot( boxcox_model, 2, col="deeppink")
plot( boxcox_model, 3, col.line="deeppink")
residualPlot(boxcox_model, type='rstudent',col.quad="deeppink",lwd=2 )
plot(rstudent(boxcox_model), type='l', col="deeppink",lwd=2)

#________________________________________________________________________________________________
#### COOK'S DISTANCE ####

sample_size<-nrow(regressionmodel)
mod <-  lm(log(robbbPerPop+1)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=regressionmodel)
cooksd <- cooks.distance(mod)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels

influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
cooks <- regressionmodel[-influential, ]

#________________________________________________________________________________-
#### MODEL AFTER APPLYING COOKS DISTANCE ####

model <- lm(powerTransform(log(robbbPerPop),lambda)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=cooks)
summary(step(model,direction="both"))
round(vif(model),2)
par(mfrow=c(1,1))
spreadLevelPlot(model)

#________________________________________________________________________________________________
#### TEST ASSUMPTIONS AFTER COOKS DISTANCE ####

# ___________________
# Global test of model assumptions
# ___________________
gvmodel <- gvlma(model) 
summary(gvmodel)

# ___________________
# Non Linearity
# ___________________
par(mfrow=c(1,1))
residualPlot(model, type='rstudent', col.quad="deeppink") 
residualPlots(model, plot=F, type = "rstudent")
crPlots(model)

# ___________________
# Independence 
# ___________________
plot(rstudent(model), type='l', col='deeppink', lwd=2.5)
library(randtests); runs.test(model$res) 
library(lmtest);dwtest(model) 
#D-W statistic is between the desirable margins (1.4-2.6) while the p-value is very high.
library(car); durbinWatsonTest(model)  # model passes

# ___________________
# Equality of variances (Homoscedasticity) 
# ___________________
yhat <- fitted(model)
yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
table(yhat.quantiles)

leveneTest(rstudent(model)~yhat.quantiles) # model passes

#Equlaity of variance is not met
boxplot(rstudent(model)~yhat.quantiles)

# ___________________
# Normality Assumption
# ___________________

plot(model, which = 2) #Normality of the residuals (step 1)
shapiro.test(model$residuals) # model does not pass 
lillie.test(model$residuals) # model passes
ad.test(model$residuals) # model passes
Stud.residuals <- rstudent(model)
yhat <- fitted(model)
par(mfrow=c(1,1))
plot(yhat, Stud.residuals)
abline(h=c(-2,2), col=2, lty=2)

#Summary of the final 4 plots
par(mfrow=c(2,2))
plot( model, 2, col="deeppink")
plot( model, 3)
residualPlot(model, type='rstudent',col.quad="deeppink",lwd=2 )
plot(rstudent(model), type='l', col="deeppink",lwd=2)

#_____________________________________________________________________________________________
#### FINAL MODELS FOR 10 FOLD CROSS VALIDATION ####


# LOG MODEL
logmodel <- lm(log(robbbPerPop)~+racePctWhite+log(PctKidsBornNeverMar),data=cooks)

log_cross<-train(log(robbbPerPop)~+racePctWhite+PctKidsBornNeverMar,data=cooks, na.action=na.exclude, method="lm",
                    trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
print(log_cross)

# POLYNOMIAL MODEL AFTER COOK'S DISTANCE 
poly_model <- lm(log(robbbPerPop)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=cooks)
model_cross<-train(log(robbbPerPop)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=cooks, na.action=na.exclude, method="lm",
                 trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
print(model_cross)

# BOX-COX MODEL
boxcox_model <- lm(powerTransform(log(robbbPerPop),lambda)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=boxcox)
box_cross<-train(log(robbbPerPop)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar),data=cooks, na.action=na.exclude, method="lm",
                   trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
print(box_cross)

#_____________________________________________________________________________________________
#### OUT OF SAMPLE PREDICTIVE ABILITY  OF THE MODELS ####

test<-read.table("crime_test.dat", sep=",", header=F)
test <- setNames(test, c('communityname','state','countyCode','communityCode','fold','population','householdsize',
                         'racepctblack','racePctWhite','racePctAsian','racePctHisp','agePct12t21','agePct12t29',
                         'agePct16t24','agePct65up','numbUrban','pctUrban','medIncome','pctWWage','pctWFarmSelf',
                         'pctWInvInc','pctWSocSec','pctWPubAsst','pctWRetire','medFamInc','perCapInc','whitePerCap',
                         'blackPerCap','indianPerCap','AsianPerCap','OtherPerCap','HispPerCap','NumUnderPov',
                         'PctPopUnderPov','PctLess9thGrade','PctNotHSGrad','PctBSorMore','PctUnemployed',
                         'PctEmploy','PctEmplManu','PctEmplProfServ','PctOccupManu','PctOccupMgmtProf',
                         'MalePctDivorce','MalePctNevMarr','FemalePctDiv','TotalPctDiv','PersPerFam','PctFam2Par',
                         'PctKids2Par','PctYoungKids2Par','PctTeen2Par','PctWorkMomYoungKids','PctWorkMom',
                         'NumKidsBornNeverMar','PctKidsBornNeverMar','NumImmig','PctImmigRecent','PctImmigRec5',
                         'PctImmigRec8','PctImmigRec10','PctRecentImmig','PctRecImmig5','PctRecImmig8',
                         'PctRecImmig10','PctSpeakEnglOnly','PctNotSpeakEnglWell','PctLargHouseFam','PctLargHouseOccup',
                         'PersPerOccupHous','PersPerOwnOccHous','PersPerRentOccHous','PctPersOwnOccup','PctPersDenseHous',
                         'PctHousLess3BR','MedNumBR','HousVacant','PctHousOccup','PctHousOwnOcc','PctVacantBoarded',
                         'PctVacMore6Mos','MedYrHousBuilt','PctHousNoPhone','PctWOFullPlumb','OwnOccLowQuart',
                         'OwnOccMedVal','OwnOccHiQuart','OwnOccQrange','RentLowQ','RentMedian','RentHighQ',
                         'RentQrange','MedRent','MedRentPctHousInc','MedOwnCostPctInc','MedOwnCostPctIncNoMtg',
                         'NumInShelters','NumStreet','PctForeignBorn','PctBornSameState','PctSameHouse85',
                         'PctSameCity85','PctSameState85','LemasSwornFT','LemasSwFTPerPop','LemasSwFTFieldOps',
                         'LemasSwFTFieldPerPop','LemasTotalReq','LemasTotReqPerPop','PolicReqPerOffic',
                         'PolicPerPop','RacialMatchCommPol','PctPolicWhite','PctPolicBlack','PctPolicHisp',
                         'PctPolicAsian','PctPolicMinor','OfficAssgnDrugUnits','NumKindsDrugsSeiz','PolicAveOTWorked',
                         'LandArea','PopDens','PctUsePubTrans','PolicCars','PolicOperBudg','LemasPctPolicOnPatr',
                         'LemasGangUnitDeploy','LemasPctOfficDrugUn','PolicBudgPerPop','murders','murdPerPop',
                         'rapes','rapesPerPop','robberies','robbbPerPop','assaults','assaultPerPop','burglaries',
                         'burglPerPop','larcenies','larcPerPop','autoTheft','autoTheftPerPop','arsons',
                         'arsonsPerPop','ViolentCrimesPerPop','nonViolPerPop'))

test<-subset(test,select=c(racePctWhite,PctKidsBornNeverMar,robbbPerPop ))
test$robbbPerPop<-as.numeric(as.factor(test$robbbPerPop))
test[,1:3] <-sapply(test[,1:3],as.numeric)
str(test)

# LOG MODEL
train.control<-trainControl(method="cv", number=10)
log_test<-train(log(robbbPerPop)~+racePctWhite+log(PctKidsBornNeverMar) 
                     ,data=test, na.action=na.exclude, method="lm",
                     trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
print(log_test)

# POLYNOMIAL MODEL
train.control<-trainControl(method="cv", number=10)
poly_test<-train(log(robbbPerPop)~+poly(racePctWhite,3)+log(PctKidsBornNeverMar)
                ,data=test, na.action=na.exclude, method="lm",
                trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
print(poly_test)

# BOX-COX MODEL
test$robbbPerPop <- powerTransform(log(test$robbbPerPop), lambda)
train.control<-trainControl(method="cv", number=10)
boxcox_test<-train(robbbPerPop~+poly(racePctWhite,3)+log(PctKidsBornNeverMar)
                   ,data=test, na.action=na.exclude, method="lm",
                   trControl = trainControl(method = "cv", number = 10,verboseIter = TRUE))
print(boxcox_test)

#________________________________________________________________________________-
#### CENTER THE MEDIANS ####

#Center the means
centered_model<-as.data.frame(scale(cooks, center=T, scale=F))
centered_model$robbbPerPop<-cooks$robbbPerPop
sapply(centered_model,median)
sapply(centered_model,sd)
round(sapply(centered_model,median),5)
round(sapply(centered_model,median),2)
#centered_model<-centered_model[(centered_model$PctKidsBornNeverMar>0),]
model2<-lm(log(robbbPerPop+1)~+racePctWhite+PctKidsBornNeverMar, data=centered_model)
summary(step(model2, direction='both'))

confint(model2)

cooks$racePctWhite[median(cooks$robbbPerPop)] #70.5
cooks$PctKidsBornNeverMar[median(cooks$PctKidsBornNeverMar)] #3.43

sum(exp(logmodel$fitted.values)) #overall estimate of robberies 

#________________________________________________________________________________-
#### FURTHER ANALYSIS - ROBUST REGRESSION ####

library(MASS)
rlmmodel <- rlm(log(robbbPerPop)~+(racePctWhite)+log(PctKidsBornNeverMar),
                data=cooks, psi = psi.bisquare)  # robust reg model
summary(rlmmodel)

library(DMwR)
round(DMwR::regr.eval(cooks$robbbPerPop, rlmmodel$fitted.values),3)

library(robustbase)
library(robust)
robust_model<-lmrob((log(robbbPerPop))~+(racePctWhite)+log(PctKidsBornNeverMar),
                    data=cooks, method='MM', fast.s.large.n = Inf, cov = ".vcov.w" )
summary(robust_model, setting = "KS2014")
round(vif(robust_model),1)

##################################### END OF CODE ##################################### 