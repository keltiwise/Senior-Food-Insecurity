

library(tidyverse) # includes ggplot2 and dplyer
library(ggthemes) 
library(logistf) # firth's penalized
library(glmnet) # for fitting lasso, ridge regressions (GLM)
library(haven)
library(knitr)

# read in cps data as cps
cps <- read.csv('Data/cps_00006.csv', stringsAsFactors = TRUE)

# first few rows of select variables
head(cps[, c('CPSID', 'PERNUM', 'FSSTATUS', 'FSSTATUSMD', 'RACE', 'EDUC')]) %>% kable

# look at more descriptive labels for reference
# map_chr(cps, ~attr(.x, 'label')) %>%
#  bind_cols(names = names(cps), question = .) %>%
#  rownames_to_column(var = 'Variable Name') %>% kable

table(cps$STATEFIP)

# save this code into a src folder named clean_cps.R
# you'll also save code called clean_acs.R
# and hopefully multiple analysis files - one for each of the Y variables
# you choose to model
# your analysis files can source clean_cps.R using the source() function

# each row of cps is an individuals within a family
cps <- cps %>%
  mutate(
    SEX = SEX -1,
    CHILD = ifelse(AGE < 18, 1, 0),
    ELDERLY = ifelse(AGE > 59, 1, 0),
    BLACK = ifelse(RACE == 200, 1, 0),
    HISPANIC = ifelse(HISPAN > 0, 1, 0),
    EDUC = as.integer(EDUC %in% c(91, 92, 111, 123, 124, 125)),
    EMP = as.integer(EMPSTAT %in% c(1, 10, 12)),
    MARRIED = as.integer(MARST %in% c(1, 2)),
    DIFF = ifelse(DIFFANY == 2, 1, 0),
    COUNTY = as.factor(COUNTY))

# currently one row of cps = one individual
# however, we want to make prediction on the family level
# aggregate to the family level - this is where we choose family-level traits
# that we want to calculate. For example, household size is equal to the 
# number of rows for that family

cps_data <- cps %>%
  group_by(CPSID = as.factor(CPSID)) %>%
  summarise(COUNTY = first(COUNTY),
            # family level weight
            weight = first(HWTFINL),
            # household size
            hhsize = n(),
            # y variables 
            # see cps website for details
            # FSSTATUS, etc. is the same for each member - just take first value
            # for each family
            FSTOTXPNC_perpers = FSTOTXPNC/hhsize,
            FSSTATUS = first(FSSTATUS),
            FSSTATUSMD = first(FSSTATUSMD),
            FSFOODS = first(FSFOODS),
            FSWROUTY = first(FSWROUTY),
            FSBAL = first(FSBAL),
            FSRAWSCRA = first(FSRAWSCRA),
            FSTOTXPNC = first(FSTOTXPNC),
            FamInc = first(FAMINC),
            # count of family members in various categories
            female = sum(SEX),
            hispanic = sum(HISPANIC),
            black = sum(BLACK),
            kids = sum(CHILD),
            elderly = sum(ELDERLY),
            education = sum(EDUC),
            married = sum(MARRIED)) %>% ungroup()

# each row of cps_data is a family
# note we just calculated the number of people in each family that belong
# to the above groups. perhaps that isn't the best way? would proportions 
# be good in addition or instead of sums?!

# summary(cps_data) # see extremes for food security variables
# https://cps.ipums.org/cps-action/variables/search

cps_data <- cps_data %>%
  mutate(
    FSSTATUS = ifelse(FSSTATUS %in% c(98, 99), NA, FSSTATUS),
    FSSTATUSMD = ifelse(FSSTATUSMD %in% c(98, 99), NA, FSSTATUSMD),
    FSFOODS = ifelse(FSFOODS %in% c(98, 99), NA, FSFOODS),
    FSWROUTY = ifelse(FSWROUTY %in% c(96, 97, 98, 99), NA, FSWROUTY),
    FSBAL = ifelse(FSBAL %in% c(96, 97, 98, 99), NA, FSBAL),
    FSRAWSCRA = ifelse(FSRAWSCRA %in% c(98, 99), NA, FSRAWSCRA),
    FSTOTXPNC = ifelse(FSTOTXPNC %in% c(999), NA, FSTOTXPNC)) %>%
  mutate(
    FSSTATUS = ifelse(FSSTATUS > 1, 1, 0),
    FSSTATUSMD = ifelse(FSSTATUSMD > 1, 1, 0),
    FSFOODS = ifelse(FSFOODS > 1, 1, 0),
    FSWROUTY = ifelse(FSWROUTY > 1, 1, 0),
    FSBAL = ifelse(FSBAL > 1, 1, 0),
    FSRAWSCRA = ifelse(FSRAWSCRA > 1, 1, 0))


cps_data <- cps_data %>%
  mutate(
    FamInc = case_when(
      FamInc == 100 ~ "Under $5,000",
      FamInc == 110 ~ "Under $1,000",
      FamInc == 111 ~ "Under $500",
      FamInc == 112 ~ "$500 - 999",
      FamInc == 120 ~ "$1,000 - 1,999",
      FamInc == 121 ~ "$1,000 - 1,499",
      FamInc == 122 ~ "$1,500 - 1,999",
      FamInc == 130 ~ "$2,000 - 2,999",
      FamInc == 131 ~ "$2,000 - 2,499",
      FamInc == 132 ~ "$2,500 - 2,999",
      FamInc == 140 ~ "$3,000 - 3,999",
      FamInc == 141 ~ "$3,000 - 3,499",
      FamInc == 142 ~ "$3,500 - 3,999",
      FamInc == 150 ~ "$4,000 - 4,999",
      FamInc == 200 ~ "$5,000 - 7,999",
      FamInc == 210 ~ "$5,000 - 7,499",
      FamInc == 220 ~ "$5,000 - 5,999",
      FamInc == 230 ~ "$6,000 - 7,999",
      FamInc == 231 ~ "$6,000 - 7,499",
      FamInc == 232 ~ "$6,000 - 6,999",
      FamInc == 233 ~ "$7,000 - 7,499",
      FamInc == 234 ~ "$7,000 - 7,999",
      FamInc == 300 ~ "$7,500 - 9,999",
      FamInc == 310 ~ "$7,500 - 7,999",
      FamInc == 320 ~ "$8,000 - 8,499",
      FamInc == 330 ~ "$8,500 - 8,999",
      FamInc == 340 ~ "$8,000 - 8,999",
      FamInc == 350 ~ "$9,000 - 9,999",
      FamInc == 400 ~ "$10,000 - 14,999",
      FamInc == 410 ~ "$10,000 - 10,999",
      FamInc == 420 ~ "$11,000 - 11,999",
      FamInc == 430 ~ "$10,000 - 12,499",
      FamInc == 440 ~ "$10,000 - 11,999",
      FamInc == 450 ~ "$12,000 - 12,999",
      FamInc == 460 ~ "$12,000 - 14,999",
      FamInc == 470 ~ "$12,500 - 14,999",
      FamInc == 480 ~ "$13,000 - 13,999",
      FamInc == 490 ~ "$14,000 - 14,999",
      FamInc == 500 ~ "$15,000 - 19,999",
      FamInc == 510 ~ "$15,000 - 15,999",
      FamInc == 520 ~ "$16,000 - 16,999",
      FamInc == 530 ~ "$17,000 - 17,999",
      FamInc == 540 ~ "$15,000 - 17,499",
      FamInc == 550 ~ "$17,500 - 19,999",
      FamInc == 560 ~ "$18,000 - 19,999",
      FamInc == 600 ~ "$20,000 - 24,999",
      FamInc == 700 ~ "$25,000 - 49,999",
      FamInc == 710 ~ "$25,000 - 29,999",
      FamInc == 720 ~ "$30,000 - 34,999",
      FamInc == 730 ~ "$35,000 - 39,999",
      FamInc == 740 ~ "$40,000 - 49,999",
      FamInc == 800 ~ "$50,000 and over",
      FamInc == 810 ~ "$50,000 - 74,999",
      FamInc == 820 ~ "$50,000 - 59,999",
      FamInc == 830 ~ "$60,000 - 74,999",
      FamInc == 840 ~ "$75,000 and over",
      FamInc == 841 ~ "$75,000 - 99,999",
      FamInc == 842 ~ "$100,000 - 149,999",
      FamInc == 843 ~ "$150,000 and over",
      FamInc == 995 ~ "Missing",
      FamInc == 996 ~ "Refused",
      FamInc == 997 ~ "Don't know",
      FamInc == 999 ~ "Blank",
      TRUE ~ "Unknown"
    )
  )










