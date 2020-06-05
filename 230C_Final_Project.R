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

## BB5CB_* outcome variables to plot for distribution densities ####
#B5CB_AGR CBCL Aggressive Behavior Raw Score
#B5CB_APR CBCL Attention Problems Raw Score
#B5CB_AXR CBCL Anxious/Depressed Raw Score
#B5CB_EXR CBCL Externalizing Raw Score
#B5CB_INR CBCL Internalizing Raw Score
#B5CB_RBR CBCL Rule-Breaking Behavior Raw Score
#B5CB_SCR CBCL Somatic Complaints Raw Score
#B5CB_SPR CBCL Social Problems Raw Score
#B5CB_TPR CBCL Thought Problems Raw Score
#B5CB_WDR CBCL Withdrawn/Depressed Raw Score
  
interest_outcomes <- c("B5CB_AGR", "B5CB_APR", "B5CB_AXR", "B5CB_EXR", "B5CB_INR", "B5CB_RBR", "B5CB_SCR", "B5CB_SPR", "B5CB_TPR", "B5CB_WDR")

filtered <- ehs %>% select(IDNUM, all_of(interest_outcomes))

#The number of missing outcome variables is uniform, but also nearly half of all observations!
filtered %>% select("B5CB_AGR", "B5CB_APR", "B5CB_AXR", "B5CB_EXR", "B5CB_INR", "B5CB_RBR", "B5CB_SCR", "B5CB_SPR", "B5CB_TPR", "B5CB_WDR") %>% summarise_all(funs(sum(is.na(.))))

#Histograms saves in b5cd_plots directory
for (i in 1:length(interest_outcomes)) {
    x <- filtered[[i+1]]
  plot <- ggplot(ehs, aes_string(x=x)) +
    geom_histogram(binwidth=1) +
    labs(title=interest_outcomes[i])
  print(plot)
  #ggsave(filename = str_c(interest_outcomes[i], ".png"), path = "plots/B5CB_plots/", scale = 2)
}

#density plots of b5cd plots
for (i in 1:length(interest_outcomes)) {
  x <- filtered[[i+1]]
  plot <- ggplot(ehs, aes(x=x)) +
    geom_density(na.rm=TRUE, aes(y=stat(count)), fill="#69b3a2", alpha=0.8)+
    labs(title=interest_outcomes[i])
  print(plot)
  ggsave(filename = str_c(interest_outcomes[i], ".png"), path = "plots/B5CB_plots/", scale = 2)
}

# Create clean dataset with variables of interest ####
ehs_clean <- ehs %>% select(IDNUM, PROGRAM, PROGTYPE, all_of(interest_outcomes))

#Compare missingness by groups:
ehs_clean %>% group_by(PROGTYPE) %>% select("B5CB_AGR", "B5CB_APR", "B5CB_AXR", "B5CB_EXR", "B5CB_INR", "B5CB_RBR", "B5CB_SCR", "B5CB_SPR", "B5CB_TPR", "B5CB_WDR") %>% summarise_all(funs(sum(is.na(.)))) 
#More cases missing in group 2, then group 3, then group 1.

ehs_clean %>% group_by(PROGRAM) %>% select("B5CB_AGR", "B5CB_APR", "B5CB_AXR", "B5CB_EXR", "B5CB_INR", "B5CB_RBR", "B5CB_SCR", "B5CB_SPR", "B5CB_TPR", "B5CB_WDR") %>% summarise_all(funs(sum(is.na(.)))) 
#Even-ish split of missing cases: Comparison group NA = 687, treatment group NA = 687

#Non missing sums for progtype within PROGRAM to check for even distributions of the three types.
ehs_clean %>% group_by(PROGRAM) %>% select("PROGTYPE") %>% summarise_all(funs(sum(!is.na(.)))) 
#1474 non-missing comparison, 1503 non-missing program

ehs_clean %>% group_by(PROGTYPE) %>% summarise_all(funs(sum(!is.na(.))))
#610 center based, 1368 home based, 999 mixed

ehs_clean %>% filter(!is.na(PROGTYPE)) %>% group_by(PROGTYPE) %>% count(PROGRAM)
#Center based: Tr=305, Cont=305; Home-based: Tr=668, Cont=700; Mixed: Tr=501, Cont=498
