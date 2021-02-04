##A DiGiorgio, J McCann
#For Specific Gravity & Creatinine Project
#Oct 2020

#List of all packages used
packages <-
  c(
    "readxl",
    "dplyr",
    "tibble",
    "lme4",
    "lmerTest",
    "tidyverse",
    "reshape2",
    "emmeans",
    "MuMIn",
    "rsq",
  )

#Installs app packages if missing, and loads
check.packages <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
check.packages(packages)

################## DATASET w Cre/(Sg-1)+(Sg-1)^2 W/ too low SG removed  ########################
##Dataset, SGCrCl3 (from Excel SG_Cr_Cl_3Use)
RawData <- read_excel("SG_Cr_Cl_3Use.xlsx")
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


############################ GLMMs on residuals###########################

##############GLMM ResidCre for FAI CONTINUOUS and Age-Sex CONDENSED, random = OuID ###############
##USED IN PAPER TEXT
##RESIDUAL CRE w AS Condensed
GlmmResFAIASN <-lmer(ResidCre ~ 0 + AgeSexAbb3 + FAINu3 + (1 | OuID3), data = SGCrCl3)
summary(GlmmResFAIASN)

#to get F values
anova(GlmmResFAIASN)

#R2 for model (3 ways to find, diff R2 values)
r.squaredLR(GlmmResFAIASN)
r.squaredGLMM(GlmmResFAIASN) #requires MuMIn package
rsq(GlmmResFAIASN)  #requires rsq package

####Posthoc Tukey T-test w/ AS Condensed
Posthoc1n <-
  emmeans(GlmmResFAIASN, list(pairwise ~ AgeSexAbb3), adjust = "tukey")
print(Posthoc1n)

## Make Predicted Values from GLMM w/ Age-Sex and FAI for plotting
SGCrCl3$GLMMCreASN <-
  predict (GlmmResFAIASN) # create a new variable with the predicted values based on model1
print(SGCrCl3)




##############GLMM ResidCre for FAI(H/L) and Age-Sex CONDENSED (4 groups), random = OuID ###############
##USED IN SUPPLEMENT, data behind High Low Facet grap
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



