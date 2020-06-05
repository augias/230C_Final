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
library(haven)

## directory paths ####

## Dataset load ####

ehs <- read_sav(file = "EHS_spss_ICPSR_03804-V5/ICPSR_03804/DS0001/03804-0001-Data.sav")

## Create an object with all the variables we would like to test for distribution normality, inorder to create a for loop with these names
#B1P_EEMO : Emotionality scale from 14 month interview
#B5CB_AGR -CBCL Aggressive Behavior Raw Score
#B5CB_APR - CBCL Attention Problems Raw Score
#B5CB_AXR - CBCL Anxious/Depressed Raw Score
#B5CB_EXR - CBCL Externalizing Raw Score
#B5CB_INR - CBCL Internalizing Raw Score
#B5CB_RBR CBCL Rule-Breaking Behavior Raw Score
#B5CB_SCR CBCL Somatic Complaints Raw Score
#B5CB_SPR CBCL Social Problems Raw Score
#B5CB_TPR CBCL Thought Problems Raw Score
#B5CB_WDR CBCL Withdrawn/Depressed Raw Score

  
interest_outcomes <- c("B5CB_AGR", "B5CB_APR", "B5CB_AXR", "B5CB_EXR", "B5CB_INR", "B5CB_RBR", "B5CB_SCR", "B5CB_SPR", "B5CB_TPR", "B5CB_WDR")

filtered <- ehs[interest_outcomes]

for (i in 1:length(interest_outcomes)) {
    x <- filtered[[i]]
  plot <- ggplot(ehs, aes_string(x=x)) +
    geom_histogram(binwidth=1) +
    labs(title=interest_outcomes[i])
  print(plot)
  ggsave(filename = str_c(interest_outcomes[i], ".png"), path = "plots/", scale = 2)
}
