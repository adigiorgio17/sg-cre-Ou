##A DiGiorgio, J McCann, D Naumenko, E Vogel
#For Specific Gravity & Creatinine Project
#Feb 2021

#List of all packages used
packages <-
  c(
    "readxl",
    "dplyr",
    "tibble",
    "ggplot2",
    "lme4",
    "lmerTest",
    "patternplot",
    "tidyverse",
    "reshape2",
    "emmeans",
    "MuMIn",
    "rsq",
    "wesanderson",
    "gamm4",
    "RColorBrewer",
    "grid",
    "gtable",
    "gridExtra",
    "Hmisc",
    "lattice",
    "MASS",
    "pgirmess",
    "rgl",  ##Need XQuartz to run this, xquartz.org
    "ggpubr",
    "gratia",
    "mgcv",
    "mgcViz", #requires rgl
    "AICcmodavg",
    "paletteer"    
  )


#Installs app packages if missing, and loads
check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)


################## DATASET w Cre/(Sg-1)+(Sg-1)^2 W/ too low SG removed outliers= ########################
##Added new dataset, SGCrCl3 (from Excel SG_Cr_Data_2_21MKC.xlsx)

RawData <- read_excel("SG_Cr_Data_2_21MKC.xlsx")
SGCrCl3 <- tbl_df(RawData)


##Remove SG and Cre NA rows##
SGCrCl3 <- SGCrCl3[which(SGCrCl3$SGmeter3 != "NA"), ]
SGCrCl3 <- SGCrCl3[which(SGCrCl3$Creatinine_Result3 != "NA"), ]


##Assign data types
OuID3 <- as.factor(SGCrCl3$Orangutan_ID3)
FirstUr3 <- as.factor(SGCrCl3$First_Urine_Day3)
AgeSex3 <- as.factor(SGCrCl3$Age_Class3)
AgeSexAbb3 <- as.factor(SGCrCl3$AgeClassCond3)
Cre3 <- as.vector(SGCrCl3$Creatinine_Result3)
CV3 <- as.vector(SGCrCl3$CV3)
SG3 <- as.vector(SGCrCl3$SGmeter3)
SGadj3 <- as.vector(SGCrCl3$'SG-13')
Sex3 <- as.factor(SGCrCl3$Sex3)
FAI3 <- as.factor(SGCrCl3$HI_Low_FAI_Quartiles3)
Slope3 <- as.vector(SGCrCl3$'Cr/(SG-1)3')
FAINu3 <- as.vector(SGCrCl3$'Habitat_Wide3')
SGadj23 <- as.vector(SGCrCl3$'Sg-1^23')
CrSgQuad <- as.vector(SGCrCl3$'Cr/[(SG-1)+(SG-1)^2]3')

str(SGCrCl3)
print(SGCrCl3)

#check column names
names(SGCrCl3)


################# LMM to get residuals: NO fixed effects###########################
###LINEAR REGRESSION Cre ~ SGadj and SG-1^2
LmmCreSgSG2 <- lm(Cre3 ~ 0 + SGadj3 + SGadj23)
summary(LmmCreSgSG2)


###### PREDICTED VALUES #####
SGCrCl3$PredCre <-
  predict (LmmCreSgSG2) # create a new variable with the predicted values based on LmmCreSgSG2
print(SGCrCl3)
##Can round values with SGCrCl3$PredCre <- round(SGCrCl3$PredCre, digits = 2)


###### RESIDUALS #######
### http://rstudio-pubs-static.s3.amazonaws.com/310380_07eebbdd4fac48aba8399c738ae0c31d.html ###
## Get residuals for next model...
SGCrCl3$ResidCre <- residuals(LmmCreSgSG2)
print(SGCrCl3)


################# LMM to get residuals: NO fixed effects###########################
###LINEAR REGRESSION Cre ~ SGadj and SG-1^2
LmmCreSgSG2 <- lm(Cre3 ~ 0 + SGadj3 + SGadj23)
summary(LmmCreSgSG2)


###### PREDICTED VALUES #####
SGCrCl3$PredCre <-
  predict (LmmCreSgSG2) # create a new variable with the predicted values based on LmmCreSgSG2
print(SGCrCl3)
##Can round values with SGCrCl3$PredCre <- round(SGCrCl3$PredCre, digits = 2)


###### RESIDUALS #######
### http://rstudio-pubs-static.s3.amazonaws.com/310380_07eebbdd4fac48aba8399c738ae0c31d.html ###
## Get residuals for next model...
SGCrCl3$ResidCre <- residuals(LmmCreSgSG2)
print(SGCrCl3)


##summary stats for ResidCre by age
group_by(SGCrCl3, AgeClassCond3) %>%
  summarise(
    count = n(),
    mean = mean(ResidCre, na.rm = TRUE),
    sd = sd(ResidCre, na.rm = TRUE),
    median = median(ResidCre, na.rm = TRUE),
    IQR = IQR(ResidCre, na.rm = TRUE)
  )


######################### GAMMs on Residuals #############################
###below is the basic GAMM - you can only smooth continuous variables; we include a random effect of orangutan ID

##GAMM on Residual Creatinine w/  Age Sex condensed and continuous FAI, OU ID is random
GAMMResCreXASxFAnum<-gamm4(ResidCre~s(FAINu3)+AgeSexAbb3,data=SGCrCl3,random=~(1|OuID3)+(FAINu3+0|OuID3))
summary(GAMMResCreXASxFAnum$gam)

#below gives you a quick way to view your results of the model - these should not be used in the thesis but give you an idea of data
#FAI is very wiggly - so we may want to reduce k; I changed it to 7 and it is much less wiggly now - still non-linear
plotmodelGAMMResCreXASxFAnum <- getViz(GAMMResCreXASxFAnum$gam)
print(plot(plotmodelGAMMResCreXASxFAnum, allTerms = T), pages = 1)

#below (getViz) is a way to check your model
#basically you want the black line to match up with the red diagonal line  √ 
#you want a more of less normal distribution of residuals √
#you want you residuals to be pretty evenly spread out  √
b <- getViz(GAMMResCreXASxFAnum$gam)
check(b)

##provides a AICc to compare models]
AICc(GAMMResCreXASxFAnum$mer)


###Reordering the Age Sex Class to provide more pair-wise data
SGCrCl3$AgeClassCond3 <- relevel(SGCrCl3$AgeClassCond3, "adult flanged male")
#run model again to find other pairwise comparisons
CreSG2<-gamm4(ResidCre~s(FAINu3)+AgeClassCond3,data=SGCrCl3,random=~(1|OuID3)+(FAINu3+0|OuID3))
summary(CreSG2$gam)

##relevel again
SGCrCl3$AgeClassCond3 <- relevel(SGCrCl3$AgeClassCond3,"adult unflanged male")
#run model again to find other pairwise comparisons
CreSG3<-gamm4(ResidCre~s(FAINu3)+AgeClassCond3,data=SGCrCl3,random=~(1|OuID3)+(FAINu3+0|OuID3))
summary(CreSG2$gam)

##relevel again
SGCrCl3$AgeClassCond3 <- relevel(SGCrCl3$AgeClassCond3,"dependent")
#run model again to find other pairwise comparisons
CreSG2<-gamm4(ResidCre~s(FAINu3)+AgeClassCond3,data=SGCrCl3,random=~(1|OuID3)+(FAINu3+0|OuID3))
summary(CreSG2$gam)



##############GLMM ResidCre for FAI(H/L) and Age-Sex CONDENSED, random = OuID ###############
##USED IN SUPPLEMENT, data behind High Low Facette graph
##RESIDUAL CRE w AS Condensed
GlmmResFAIASc <-lmer(ResidCre ~ 0 + AgeSexAbb3 + FAI3 + (1 | OuID3), data = SGCrCl3)
summary(GlmmResFAIASc)
print(GlmmResFAIAsc)

anova(GlmmResFAIASc)

#R2 for model (3 ways to find, diff R2 values)
r.squaredLR(GlmmResFAIASc)
r.squaredGLMM(GlmmResFAIASc) #requires MuMIn package
rsq(GlmmResFAIASc)  #requires rsq package

####Posthoc Tukey T-test w/ Age-Sex Condensed
Posthoc1c <-
  emmeans(GlmmResFAIASc, list(pairwise ~ AgeSexAbb3), adjust = "tukey")
print(Posthoc1c)

## Make Predicted Values from GLMM w/ Age-Sex and FAI for plotting
SGCrCl3$GLMMCreASc <-
  predict (GlmmResFAIASc) # create a new variable with the predicted values based on model1
print(SGCrCl3)


write.csv(SGCrCl3, file = "SGCrDataNEW.csv")
