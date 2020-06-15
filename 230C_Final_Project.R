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

## Dataset load ####

ehs <- read.table(file = "EHS_delimited_ICPSR_03804-V5/03804-0001-Data.tsv", sep = '\t', header = TRUE)

count(ehs$HGCG)

## Covariate values ####
#PROGRAM - 0 comparison and 1 treatment groups
#B4PINCOM - PreK income per month
#B3P_PD - Parenting stress index short form at 36 months (OUTCOME)
#B1P_PD - Parenting stress index at 14 months (possible CV/pretest)
#HGCG - 1: Completed Less than 12 yrs 2: Compreted 12 or GED 3: More than 12 yrs
#CSEX - child gender F M U
#RACE - Primary caregiver's race 1 white, 2 aa, 3, hisp, 4 other, -6 -5 missing
#TEEN_MOM - 1= yes Binary mom < 20 at child birthyear
#ENGLISH - 1= yes Binary primary language is english
#SUPPORT - 1=inadequate inadequate support from family and friends
#ADULTS_G - number of adults in household 0=0, 1=1 2=2 3=3

# Create subset dataframe ####
filtered <- ehs %>% select(IDNUM, PROGRAM, B3P_PD, B1P_PD, B4PINCOM, HGCG, CSEX, RACE, TEEN_MOM, ENGLISH, SUPPORT, ADULTS_G)

filtered <- filtered %>% mutate_all(funs(replace(., .<0, NA_real_))) #Turn any values below 0 into an NA value

factors <- c("PROGRAM", "HGCG", "CSEX", "RACE", "TEEN_MOM", "ENGLISH", "SUPPORT") #This char vector will help factorize these variables

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
filtered %>% t.test(B1P_PD ~ PROGRAM, data = ., var.equal = TRUE) 

#Chi-squared tests for independence of CSEX, RACE, TEEN_MOM, ENGLISH, HGCG, SUPPORT, ADULTS_G

chi <- c("CSEX", "RACE", "TEEN_MOM", "ENGLISH", "HGCG", "SUPPORT", "ADULTS_G")

for (i in 1:length(chi)) { #loop through 'i' where i=1 at CSEX and i=7 at ADULTS_G
  col_name <- chi[i]
  tbl <- table(filtered[[col_name]], filtered$PROGRAM)
  writeLines(str_c("Chi suqared table and test for ", col_name))
  print(tbl)
  print(chisq.test(tbl))
}

#ASSUMPTIONS CHECKS for INCOME ####

#Checks for normality of income distributions ####
for (i in 0:1) {
hist(filtered$B4PINCOM[filtered$PROGRAM==i], prob = TRUE, main = paste("Histogram of income in PROGRAM ", i))
curve(dnorm(x, mean=mean(filtered$B4PINCOM[filtered$PROGRAM==i], na.rm = TRUE), sd=sd(filtered$B4PINCOM[filtered$PROGRAM==i], na.rm = TRUE)), add=TRUE) #adds normal curve
} #both charts for program group 0 and 1 look identical

# Boxplots
library(moments)
boxplot(B4PINCOM~PROGRAM, # Tells R to plot the commute time variable by section number
        data=filtered, # Identifies the incomeset to be plotted
        main="Monthly income", # Title of the boxplot 
        xlab="Treatment Group", # X-axis label
        ylab="Income", # Y-axis label 
        boxwex = 0.3)  # A scaling factor that changes the width of the boxplots

# check for outliers using Z-scores by group (yardstick >|3| z-score) ####
library(dplyr)
filtered <- filtered %>%
  group_by(PROGRAM) %>% 
  mutate(B4PINCOM_z = scale(B4PINCOM)) %>% ungroup()

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

# Plot residuals vs locus
plot(hsb1$locus, hsb1$Zresiduals, main="Zresiduals vs locus", xlab = "locus", ylab = "Standardized Residuals")
abline(h = 0, col = "blue") #adds a horizontal reference line

#Test correlaiotn of B4PINCOM and B3P_PD ####
cor(filtered$B4PINCOM, filtered$B3P_PD, use = "complete.obs") #pearson correlatoin = -0.09076674

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
covariates <- c("CSEX", "RACE", "TEEN_MOM", "ENGLISH", "HGCG", "SUPPORT", "ADULTS_G")
for (i in 1:length(covariates)) { #loop through 'i' where i=1 at CSEX and i=7 at ADULTS_G
  col_name <- covariates[i]
print(ggscatter(
  filtered, x = "B4PINCOM", y = "B3P_PD",
  facet.by  = c(col_name, "PROGRAM"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9))
}

# Homogeneity of slopes

# Ancova ####
# Dependent Variable: B3P_PD
# Treatment IV: PROGRAM
# Covariates B4PINCOM, HGCG, CSEX, RACE, TEEN_MOM, ENGLISH, SUPPORT, ADULTS_G
