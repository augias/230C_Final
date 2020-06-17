################################################################################
##
## [ PROJ ] < 230C Final Project >
## [ FILE ] < 230C_Final_Project.R >
## [ AUTH ] < Fernando Mora & Nadia Sabat-Bass >
## [ INIT ] < May 6, 2020 >
##
################################################################################

## libraries

library(tidyverse)
library(moments)
library(psych) #if needed, use install.packages("psych")
library(car)
library(ggpubr)
library(effects)
library(multcomp)

## Dataset load ####

ehs <- read.table(file = "EHS_delimited_ICPSR_03804-V5/03804-0001-Data.tsv", sep = '\t', header = TRUE)

## Covariate values ####
#PROGRAM - 0 comparison and 1 treatment groups
#B4PINCOM - PreK income per month
#B3P_PD - Parenting stress index short form at 36 months (OUTCOME)
#B1P_PD - Parenting stress index at 14 months (possible CV/pretest)
#HGCG - 1: Completed Less than 12 yrs 2: Compreted 12 or GED 3: More than 12 yrs
#RACE - Primary caregiver's race 1 white, 2 aa, 3, hisp, 4 other, -6 -5 missing
#TEEN_MOM - 1= yes Binary mom < 20 at child birthyear
#ENGLISH - 1= yes Binary primary language is english
#SUPPORT - 1=inadequate inadequate support from family and friends
#ADULTS_G - number of adults in household 0=0, 1=1 2=2 3=3

# Create subset dataframe ####
filtered <- ehs %>% dplyr::select(IDNUM, PROGRAM, B3P_PD, B1P_PD, B4PINCOM, HGCG, RACE, TEEN_MOM, ENGLISH, SUPPORT, ADULTS_G)

filtered <- filtered %>% mutate_all(funs(replace(., .<0, NA_real_))) #Turn any values below 0 into an NA value

factors <- c("PROGRAM", "HGCG", "RACE", "TEEN_MOM", "ENGLISH", "SUPPORT") #This char vector will help factorize these variables

make_factors <- function(df) {
  for (i in 1:length(factors)) {
    col_name <- factors[i]
    print(str_c("For variable ", names(df[col_name]), str(df[col_name]), sep = "")) #What are the attributes first
    df[[col_name]] <- as_factor(df[[col_name]])
    print(str(df[col_name]))
  }
  return(df)
}
filtered <- make_factors(filtered)
str(filtered$PROGRAM) #Check to make sure your variable of choice became a factor

filtered %>% group_by(PROGRAM) %>% filter(is.na(RACE)) %>% count() #This filtered subset shows missingness. can use to compare if theres an imbalance

#Check independence ####
# Between-group: e.g., same students appear in more than one IV level?
# Within-group: e.g., are students nested?
filtered %>% group_by(IDNUM) %>%
  summarise(n_per_group=n()) %>%
  ungroup %>%
  count(n_per_group)

filtered %>% group_by(PROGRAM) %>%
  mutate(count = n()) %>%
  group_by(PROGRAM, count) %>%
  summarize_at(c("B4PINCOM"), list(mean = mean, sd = sd), na.rm=TRUE) %>%
  ungroup()
#If the output shows 2 or more n-per_group, then the same student would be  in more than ine level. We're good

# T-Tests and chi square tests for baseline stats ####
#Test B4PINCOM mean differences across program
filtered %>% t.test(B4PINCOM ~ PROGRAM, data = ., var.equal = TRUE) 
filtered %>% t.test(B3P_PD ~ PROGRAM, data = ., var.equal = TRUE) 

#Chi-squared tests for independence of CSEX, RACE, TEEN_MOM, ENGLISH, HGCG, SUPPORT, ADULTS_G

chi <- c("RACE", "TEEN_MOM", "ENGLISH", "HGCG", "SUPPORT", "ADULTS_G")

for (i in 1:length(chi)) { #loop through 'i' where i=1 at CSEX and i=7 at ADULTS_G
  col_name <- chi[i]
  tbl <- table(filtered[[col_name]], filtered$PROGRAM)
  writeLines(str_c("Chi suqared table and test for ", col_name))
  print(tbl)
  print(chisq.test(tbl))
}

#ASSUMPTIONS CHECKS for INCOME ####

#Checks for normality of income and B3P_PD distributions ####
# Find and replace to switch between B4PINCOM and B3P_PD
for (i in 0:1) {
hist(filtered$B3P_PD[filtered$PROGRAM==i], prob = TRUE, main = paste("Histogram of income in PROGRAM ", i))
curve(dnorm(x, mean=mean(filtered$B3P_PD[filtered$PROGRAM==i], na.rm = TRUE), sd=sd(filtered$B3P_PD[filtered$PROGRAM==i], na.rm = TRUE)), add=TRUE) #adds normal curve
} #both charts for program group 0 and 1 look identical

# Boxplots
library(moments)
boxplot(B3P_PD~PROGRAM, # Tells R to plot the commute time variable by section number
        data=filtered, # Identifies the incomeset to be plotted
        main="B3P_PD", # Title of the boxplot 
        xlab="Treatment Group", # X-axis label
        ylab="Income", # Y-axis label 
        boxwex = 0.3)  # A scaling factor that changes the width of the boxplots

# check for outliers using Z-scores by group (yardstick >|3| z-score) ####
filtered <- filtered %>%
  group_by(PROGRAM) %>% 
  mutate(B3P_PD_z = scale(B3P_PD)) %>% ungroup()

# Check for skewness and kurtosis ####
# Skew: skew >|1| = high, skew |1|-|.5| = moderate, skew <|.5| = symmetric
# Kurtosis: DescribeBy displays "excess kurtosis" so 0 = normal, <0 = platykurtic, >0 = leptokurtic
library(psych) #if needed, use install.packages("psych")
describeBy(subset(filtered, select=c("PROGRAM", "B4PINCOM")), group="PROGRAM", mat=FALSE, digits=3)
describeBy(filtered, group="PROGRAM", mat=FALSE, digits=3) #can also do this, but will get statistics for all variables in the data

# Alternate kurtosis calculation, where 3 = normal
kurtosis(filtered$B4PINCOM[which(filtered$PROGRAM == 0)], na.rm = TRUE) #Comparison group
kurtosis(filtered$B4PINCOM[which(filtered$PROGRAM == 1)], na.rm = TRUE) #Treatment group

check1 <- lm(B3P_PD ~ B4PINCOM+PROGRAM, data=filtered)
summary(check1)
anova(check1)
# 1. Calculate residuals and standardized residuals
residuals <- residuals(check1)
Zresiduals <- scale(residuals)

# 2. Calculate predicted values and standardized predicted values
predicted <- fitted(check1)
Zpredicted <- scale(predicted) 

#Add standardized predicted values and residuals to data frame  
filtered <- cbind(filtered, Zresiduals, Zpredicted)

# 3. Plots!
#Linearity and homoscedasticity ####
#using standardized residuals over standardized predicted value plots:
hist(Zresiduals, breaks=30, prob = TRUE)
curve(dnorm(x, mean = mean(Zresiduals), sd = sd(Zresiduals)), add = TRUE)
plot(Zpredicted, Zresiduals, main="Zresiduals vs Zpredicted")
abline(h = 0, col = "blue") 
#Normality using standardized residuals over normal scores probability plot.
qqnorm(Zresiduals,
       ylab="Standardized Residuals",
       xlab="Normal Scores")
  qqline(Zresiduals)


#Test correlaiotn of B4PINCOM and B3P_PD ####
cor(filtered$B4PINCOM, filtered$B3P_PD, use = "complete.obs") #pearson correlation = -0.09076674

#Test linearity ####
library(ggplot2)
ggplot(filtered, 
       aes(x=B4PINCOM, y=B3P_PD, color=PROGRAM, shape=PROGRAM))+
  geom_point()+
  geom_smooth(method=lm, fullrange=TRUE)+
  geom_jitter()

  # Independence of the covariate and treatment effect ####
library(car)
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
check1 <- aov(B4PINCOM ~ PROGRAM, data=filtered)
Anova(check1, type="III") # Note p-value is 0.54 so assumption that income is independent of assignment variable is reasonable.

# Homogeneity of slopes ####
# Note any non-significant p-value for the interaction term
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
ancova <- aov(B3P_PD ~ B4PINCOM*PROGRAM, data = filtered)
Anova(ancova, type="III") #Interaction term is nonsignificant. p=0.38, so we assume slopes will be homogeneous


#Assumption Checks with Factors ####

#Linearity
library(ggpubr)
covariates <- c("RACE", "TEEN_MOM", "ENGLISH", "HGCG", "SUPPORT", "ADULTS_G")
for (i in 1:length(covariates)) { #loop through 'i' where i=1 at CSEX and i=7 at ADULTS_G
  col_name <- covariates[i]
  print(ggscatter(
  filtered, x = "B4PINCOM", y = "B3P_PD",
  facet.by  = c(col_name, "PROGRAM"), 
  short.panel.labs = FALSE)+
  stat_smooth(method = "loess", span = 0.9))
}

# Homogeneity of slopes
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
test <- aov(B3P_PD ~ B4PINCOM + RACE + TEEN_MOM + ENGLISH + HGCG + SUPPORT + ADULTS_G + B4PINCOM*PROGRAM + B4PINCOM*RACE + B4PINCOM*TEEN_MOM + B4PINCOM*ENGLISH + B4PINCOM*HGCG + B4PINCOM*SUPPORT + B4PINCOM*ADULTS_G, data = filtered)
Anova(test, type="III") 
  # There were 0 significant interaction terms. The assumption is met.

library(rstatix)
filtered %>% anova_test(
  B3P_PD ~ B4PINCOM + RACE + TEEN_MOM + ENGLISH + HGCG + SUPPORT + ADULTS_G + B4PINCOM*PROGRAM + B4PINCOM*RACE + B4PINCOM*TEEN_MOM + B4PINCOM*ENGLISH + B4PINCOM*HGCG + B4PINCOM*SUPPORT + B4PINCOM*ADULTS_G, type = 3
)# There were 0 significant interaction terms. The assumption is met.


# Homogeneity of variance
library(car)
leveneTest(filtered$B4PINCOM, filtered$PROGRAM) #p=0.0.47

leveneTest(filtered$B3P_PD, filtered$PROGRAM) #p=0.66

leveneTest(filtered$B3P_PD, filtered$HGCG) #p=0.025
var(filtered$B3P_PD[filtered$HGCG==1], na.rm = TRUE)
var(filtered$B3P_PD[filtered$HGCG==2], na.rm = TRUE)
var(filtered$B3P_PD[filtered$HGCG==3], na.rm = TRUE) #OK, variance is not 4x for the largest group!

leveneTest(filtered$B3P_PD, filtered$RACE) #p=0.016
var(filtered$B3P_PD[filtered$RACE==1], na.rm = TRUE)
var(filtered$B3P_PD[filtered$RACE==2], na.rm = TRUE)
var(filtered$B3P_PD[filtered$RACE==3], na.rm = TRUE)
var(filtered$B3P_PD[filtered$RACE==4], na.rm = TRUE) #Variences in 1:4 = 82,99,100,74 

leveneTest(filtered$B3P_PD, filtered$TEEN_MOM) #p=0.7

leveneTest(filtered$B3P_PD, filtered$ENGLISH) #p=0.04
var(filtered$B3P_PD[filtered$ENGLISH==0], na.rm = TRUE)
var(filtered$B3P_PD[filtered$ENGLISH==1], na.rm = TRUE) #variances 0:1 = 105,89

leveneTest(filtered$B3P_PD, filtered$SUPPORT) #p=0.04
var(filtered$B3P_PD[filtered$SUPPORT==0], na.rm = TRUE)
var(filtered$B3P_PD[filtered$SUPPORT==1], na.rm = TRUE) #variances 0:1 = 88,112

leveneTest(filtered$B3P_PD, filtered$ADULTS_G) #p=0.1

# Ancova 1 ####
# Dependent Variable: B3P_PD
# Treatment IV: PROGRAM
# Covariate: B4PINCOM
# No factorial covariates
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
ancova1 <- aov(B3P_PD ~ B4PINCOM + PROGRAM, data = filtered)
Anova(ancova1, type="III")
adjustedMeans<-effect("PROGRAM", ancova1, se=TRUE)
summary(adjustedMeans)
plot(adjustedMeans)

library(apaTables)
apa.aov.table(ancova1)

# Ancova 2 Omnibus ####
# Dependent Variable: B3P_PD
# Treatment IV: PROGRAM
# Covariate: B4PINCOM
# Factorial covariates: HGCG, CSEX, RACE, TEEN_MOM, ENGLISH, SUPPORT, ADULTS_G
# No interactions, because assumptions check showed no significant interaction and slopes are homogenous
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
ancova2 <- aov(B3P_PD ~ B4PINCOM + PROGRAM + HGCG + RACE + TEEN_MOM + ENGLISH + SUPPORT + ADULTS_G, data = filtered)
Anova(ancova2, type="III")
summary.lm(ancova2) #woah? this does splits?

apa.aov.table(ancova2)

# Adjusted means plots for omnibus significant groups
omnibus_groups <- c("HGCG", "TEEN_MOM", "SUPPORT")
for (i in 1:length(omnibus_groups)) {
  col_name <- omnibus_groups[i]
  adjustedMeans<-effect(col_name, ancova2, se=TRUE)
  summary(adjustedMeans)
  print(plot(adjustedMeans))
}

# Ancova 3: Education Post-Hoc Contrasts ####
#Educaiton assumptions regarding the covariate
#Test linearity
library(ggplot2)
ggplot(filtered, 
       aes(x=B4PINCOM, y=B3P_PD, color=HGCG, shape=HGCG))+
  geom_point()+
  geom_smooth(method=lm, fullrange=TRUE)+
  geom_jitter()

# Check independence of the covariate and treatment effect
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
check1 <- aov(B4PINCOM ~ HGCG, data=filtered)
Anova(check1, type="III") # Note p-value is significant so this assumption isnt met

# Check homogeneity of slopes
# Note non-significant p-value for the interaction term
options(contrasts = c("contr.sum","contr.poly")) #run this code to get accurate type III SS
check2 <- aov(B3P_PD ~ B4PINCOM*HGCG, data = filtered)
Anova(check2, type="III") #Interaction was not significant, so we can assume slopes are homogeneous and look at main effects without an interaction

# HGCG Education contrasts ####
contrasts(filtered$HGCG) #Contrast before planned contrasts
table(filtered$HGCG)
#Planned Contrasts
c1 <- c(-2, 1, 1) #Contrast 1 No HS or GED to parents with HS/GED and more
c2 <- c(0, -1, 1) #Drop group with No HS/GED to contrast caretakers with minimum 12/GED and caretakers with more than 12 years of HS/GED
mat <- cbind(c1, c2)
contrasts(filtered$HGCG) <- mat
contrasts(filtered$HGCG) #Orthagonal? Yes

ancova_hgcg <- aov(B3P_PD ~ B4PINCOM + HGCG, data = filtered)
Anova(ancova_hgcg, type="III")

library(multcomp)
contanova_hgcg1 <- glht(ancova_hgcg, linfct=mcp(HGCG=c1))
summary(contanova_hgcg1) #Estimate = -4.387 p = .00000331
confint(contanova_hgcg1) #lwr = -6.231 upr = -2.5429

contanova_hgcg2 <- glht(ancova_hgcg, linfct=mcp(HGCG=c2))
summary(contanova_hgcg2) #Estimate = -1.85 p = 0.00355
confint(contanova_hgcg2) #lwr = -3.0927 upr = -0.6074

library(effects)
adjustedMeans<-effect("HGCG", ancova_hgcg, se=TRUE)
summary(adjustedMeans)
plot(adjustedMeans)

apa.aov.table(ancova_hgcg)

# Effect sizes
#Using cohen's D

#Cohen's D calculation for ANCOVA contrast 1 (No HS vs HS and +)
MSe <- 149530/1661
sqrtMSe <- sqrt(MSe)
d <- 4.387 / sqrtMSe  #Here the numerator is the effect size of the contrast seen in the summary(contanova1) command
d #d = 0.4623685

#Cohen's D calculation for ANCOVA contrast 2 (HS vs HS+)
MSe <- 149530/1661
sqrtMSe <- sqrt(MSe)
d <- 1.85 / sqrtMSe
d #d = 0.194981

# True Post-Hocs ####
library(stats)
postHocs<-glht(ancova2, linfct = mcp(HGCG = "Tukey"))
summary(postHocs)
confint(postHocs)