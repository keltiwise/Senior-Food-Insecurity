
# clear environment
rm(list=ls())

# source cleaning files
source('Code/clean_cps.R') # source for clean CPS
source("Code/clean_acs.R") # source for clean ACS

# ---------------

# load necessary packages
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(caret)
library(ggplot2)
library(reshape2)
library(randomForest)
library(RColorBrewer)
library(tidyverse) 
library(pROC)
library(glmnet)
library(lubridate)
library(rpart.plot)
library(scales)
library(viridis)
library(ggrepel)

# ---------------

# gather column names of cps data
colnames(cps_data)

# checking to see if any y variables have missing data
any(is.na(cps_data$FSSTATUS))
any(is.na(cps_data$FSFOODS))
any(is.na(cps_data$FSWROUTY))
any(is.na(cps_data$FSBAL))
any(is.na(cps_data$FSRAWSCRA))
any(is.na(cps_data$FSTOTXPNC))

# removing Y variable rows with NA values
cps_data <- cps_data %>%
  drop_na(FSSTATUS, FSFOODS, FSWROUTY, FSBAL, FSRAWSCRA, FSTOTXPNC)

# function to calculate mode to impute missing values in y columns
# get_mode <- function(v) {
#   uniqv <- unique(v[!is.na(v)]) # filter out NA values 
#   uniqv[which.max(tabulate(match(v, uniqv)))] # match returns position of each element and returns index
# }

# impute missing values for y variables 
# cps_data <- cps_data %>%
#   mutate(
#     # impute categorical/binary variables with mode
#     FSSTATUS = ifelse(is.na(FSSTATUS), get_mode(FSSTATUS), FSSTATUS),
#     FSSTATUSMD = ifelse(is.na(FSSTATUSMD), get_mode(FSSTATUSMD), FSSTATUSMD),
    
    #  impute numeric variables with median
#     FSFOODS = ifelse(is.na(FSFOODS), median(FSFOODS, na.rm = TRUE), FSFOODS),
#     FSWROUTY = ifelse(is.na(FSWROUTY), median(FSWROUTY, na.rm = TRUE), FSWROUTY),
#     FSBAL = ifelse(is.na(FSBAL), median(FSBAL, na.rm = TRUE), FSBAL),
#     FSRAWSCRA = ifelse(is.na(FSRAWSCRA), median(FSRAWSCRA, na.rm = TRUE), FSRAWSCRA),
#     FSTOTXPNC = ifelse(is.na(FSTOTXPNC), median(FSTOTXPNC, na.rm = TRUE), FSTOTXPNC)
#   )

# some column name descriptions: 
# CPSID is an IPUMS-CPS defined variable that uniquely identifies households across CPS 
# samples. The first six digits of CPSID index the four-digit year and two-digit month 
# that the household was first in the CPS.

# COUNTY gives the FIPS state and county codes for the respondent's county of residence

# weight is family level weight

# FSSTATUS identifies the 12-month food security status of the household. 
# Households are classified as food secure, low food secure or very low food secure.

# FSSTATUSMD identifies the detailed food security status of the household over the past 30 days. 
# Households are classified as high food secure, marginal food security, low food security, and very low food security.

# FSFOODS indicates whether the household had enough to eat or enough of the kinds of 
# foods they wanted to eat in the past twelve months.

# indication of column meanings in google docs

# ---------------------

# is extracting state from ID going to be valuable?
# let's try it: 

# creating state column from extracting first two digits from COUNTY
cps_data$STATE <- substr(cps_data$COUNTY, 1, 2)

# extracting year from CPSID
cps_data$YEAR <- substr(cps_data$CPSID, 1, 4)
table(cps_data$YEAR)

# changing state column ID to actual state using CPS website
cps_data <- cps_data %>%
  mutate(STATE = case_when(
    str_detect(STATE, "01") ~ "Alabama",
    str_detect(STATE, "02") ~ "Alaska",
    str_detect(STATE, "04") ~ "Arizona",
    str_detect(STATE, "05") ~ "Arkansas",
    str_detect(STATE, "06") ~ "California",
    str_detect(STATE, "08") ~ "Colorado",  
    str_detect(STATE, "09") ~ "Connecticut",
    str_detect(STATE, "10") ~ "Delaware",  
    str_detect(STATE, "11") ~ "District of Columbia",
    str_detect(STATE, "12") ~ "Florida",
    str_detect(STATE, "13") ~ "Georgia",
    str_detect(STATE, "15") ~ "Hawaii",
    str_detect(STATE, "16") ~ "Idaho",
    str_detect(STATE, "17") ~ "Illinois",
    str_detect(STATE, "18") ~ "Indiana",
    str_detect(STATE, "19") ~ "Iowa",
    str_detect(STATE, "20") ~ "Kansas",
    str_detect(STATE, "21") ~ "Kentucky",
    str_detect(STATE, "22") ~ "Louisiana",
    str_detect(STATE, "23") ~ "Maine",
    str_detect(STATE, "24") ~ "Maryland",
    str_detect(STATE, "25") ~ "Massachusetts",
    str_detect(STATE, "26") ~ "Michigan",
    str_detect(STATE, "27") ~ "Minnesota",
    str_detect(STATE, "28") ~ "Mississippi",
    str_detect(STATE, "29") ~ "Missouri",
    str_detect(STATE, "30") ~ "Montana",
    str_detect(STATE, "31") ~ "Nebraska",
    str_detect(STATE, "32") ~ "Nevada",
    str_detect(STATE, "33") ~ "New Hampshire",
    str_detect(STATE, "34") ~ "New Jersey",
    str_detect(STATE, "35") ~ "New Mexico",
    str_detect(STATE, "36") ~ "New York",
    str_detect(STATE, "37") ~ "North Carolina",
    str_detect(STATE, "38") ~ "North Dakota",
    str_detect(STATE, "39") ~ "Ohio",
    str_detect(STATE, "40") ~ "Oklahoma",
    str_detect(STATE, "41") ~ "Oregon",
    str_detect(STATE, "42") ~ "Pennsylvania",
    str_detect(STATE, "44") ~ "Rhode Island",
    str_detect(STATE, "45") ~ "South Carolina",
    str_detect(STATE, "47") ~ "Tennessee",
    str_detect(STATE, "48") ~ "Texas",
    str_detect(STATE, "49") ~ "Utah",
    str_detect(STATE, "50") ~ "Vermont",
    str_detect(STATE, "51") ~ "Virginia",
    str_detect(STATE, "53") ~ "Washington",
    str_detect(STATE, "54") ~ "West Virginia",
    str_detect(STATE, "55") ~ "Wisconsin"
  ))
# does not help much in exploratory analysis - many missing values and not enough information gain

# ----------------
####################
# Y variable: FSSTATUSMD
####################
# ----------------

# understanding data set

# looking at distribution of demographic columns to understand a background of families being looked at
# distribution of household size
ggplot(data = cps_data) + 
  geom_histogram(aes(x = hhsize), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(hhsize)), col = 'red', linetype = 'dashed') + 
  labs(x = "Household Size",
       y = "Total",
       title = "Distribution of Household Size")
# save image into results folder
ggsave("Results/Distribution_Household_Size.png", width = 12)

# distribution of education
ggplot(data = cps_data) + 
  geom_histogram(aes(x = education), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(education)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Individuals in Family that Received Education",
       y = "Total",
       title = "Distribution of Education in Each Family")
# save image into results folder
ggsave("Results/Distribution_Education.png", width = 12)

# distribution of elderly
ggplot(data = cps_data) + 
  geom_histogram(aes(x = elderly), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(elderly)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Elderly Individuals in Each Family",
       y = "Total",
       title = "Distribution of Elderly Individuals in Each Family")
# save image into results folder
ggsave("Results/Distribution_Eldery.png", width = 12)

# distribution of female
ggplot(data = cps_data) + 
  geom_histogram(aes(x = female), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(female)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Female Individuals in Each Family",
       y = "Total",
       title = "Distribution of Females in Each Family")
# save image into results folder
ggsave("Results/Distribution_Female.png", width = 12)

# distribution of black individuals
ggplot(data = cps_data) + 
  geom_histogram(aes(x = black), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(black)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Black Individuals in Each Family",
       y = "Total",
       title = "Distribution of Black Individuals in Each Family")
# save image into results folder
ggsave("Results/Distribution_Black.png", width = 12)

# distribution of married individuals
ggplot(data = cps_data) + 
  geom_histogram(aes(x = married), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(married)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Married Individuals in Each Family",
       y = "Total",
       title = "Distribution of Married Individuals in Each Family")
# save image into results folder
ggsave("Results/Distribution_Married.png", width = 12)

# distribution of kids
ggplot(data = cps_data) + 
  geom_histogram(aes(x = kids), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(kids)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Kids in Each Family",
       y = "Total",
       title = "Distribution of Kids in Each Family")
# save image into results folder
ggsave("Results/Distribution_Kids.png", width = 12)

# distribution of hispanic
ggplot(data = cps_data) + 
  geom_histogram(aes(x = hispanic), binwidth = 1, color = 'black', fill = 'skyblue') + 
  geom_vline(aes(xintercept = mean(hispanic)), col = 'red', linetype = 'dashed') + 
  labs(x = "Total Hispanic in Each Family",
       y = "Total",
       title = "Distribution of Hispanic in Each Family")
# save image into results folder
ggsave("Results/Distribution_Kids.png", width = 12)

# ---------------

# could looking at poverty level in comparison to families in the data set be valuable

# looking at poverty level
# convert FamInc to numeric using the lower bound of each income bracket
cps_data <- cps_data %>%
  mutate(FamInc_numeric = case_when(
    FamInc == "Under $5,000" ~ 5000,
    FamInc == "Under $1,000" ~ 1000,
    FamInc == "Under $500" ~ 500,
    FamInc == "$500 - 999" ~ 750,
    FamInc == "$1,000 - 1,999" ~ 1500,
    FamInc == "$1,000 - 1,499" ~ 1250,
    FamInc == "$1,500 - 1,999" ~ 1750,
    FamInc == "$2,000 - 2,999" ~ 2500,
    FamInc == "$2,000 - 2,499" ~ 2250,
    FamInc == "$2,500 - 2,999" ~ 2750,
    FamInc == "$3,000 - 3,999" ~ 3500,
    FamInc == "$3,000 - 3,499" ~ 3250,
    FamInc == "$3,500 - 3,999" ~ 3750,
    FamInc == "$4,000 - 4,999" ~ 4500,
    FamInc == "$5,000 - 7,999" ~ 6000,
    FamInc == "$5,000 - 7,499" ~ 6250,
    FamInc == "$5,000 - 5,999" ~ 5500,
    FamInc == "$6,000 - 7,999" ~ 7000,
    FamInc == "$6,000 - 7,499" ~ 7250,
    FamInc == "$6,000 - 6,999" ~ 6500,
    FamInc == "$7,000 - 7,499" ~ 7500,
    FamInc == "$7,000 - 7,999" ~ 8000,
    FamInc == "$7,500 - 9,999" ~ 8750,
    FamInc == "$7,500 - 7,999" ~ 7750,
    FamInc == "$8,000 - 8,499" ~ 8250,
    FamInc == "$8,500 - 8,999" ~ 8750,
    FamInc == "$8,000 - 8,999" ~ 8500,
    FamInc == "$9,000 - 9,999" ~ 9500,
    FamInc == "$10,000 - 14,999" ~ 12500,
    FamInc == "$10,000 - 10,999" ~ 10500,
    FamInc == "$11,000 - 11,999" ~ 11500,
    FamInc == "$10,000 - 12,499" ~ 11500,
    FamInc == "$10,000 - 11,999" ~ 11000,
    FamInc == "$12,000 - 12,999" ~ 12500,
    FamInc == "$12,000 - 14,999" ~ 13000,
    FamInc == "$12,500 - 14,999" ~ 13750,
    FamInc == "$13,000 - 13,999" ~ 13500,
    FamInc == "$14,000 - 14,999" ~ 14500,
    FamInc == "$15,000 - 19,999" ~ 17500,
    FamInc == "$15,000 - 15,999" ~ 15500,
    FamInc == "$16,000 - 16,999" ~ 16500,
    FamInc == "$17,000 - 17,999" ~ 17000,
    FamInc == "$15,000 - 17,499" ~ 16250,
    FamInc == "$17,500 - 19,999" ~ 17500,
    FamInc == "$18,000 - 19,999" ~ 18500,
    FamInc == "$20,000 - 24,999" ~ 22500,
    FamInc == "$25,000 - 49,999" ~ 37500,
    FamInc == "$25,000 - 29,999" ~ 27500,
    FamInc == "$30,000 - 34,999" ~ 32500,
    FamInc == "$35,000 - 39,999" ~ 37500,
    FamInc == "$40,000 - 49,999" ~ 45000,
    FamInc == "$50,000 and over" ~ 50000,
    FamInc == "$50,000 - 74,999" ~ 62500,
    FamInc == "$50,000 - 59,999" ~ 55000,
    FamInc == "$60,000 - 74,999" ~ 67500,
    FamInc == "$75,000 and over" ~ 75000,
    FamInc == "$75,000 - 99,999" ~ 87500,
    FamInc == "$100,000 - 149,999" ~ 125000,
    FamInc == "$150,000 and over" ~ 150000,
    TRUE ~ NA_real_
  ))

# subset data for households with size 1 in 2021
cps_2021_1 <- subset(cps_data, hhsize == 1 & YEAR == '2021' & !is.na(FamInc_numeric))

# plot family income distribution with poverty line
ggplot(data = cps_2021_1) + 
  geom_bar(aes(x = FamInc_numeric), stat = 'count', fill = 'skyblue', color = 'black') + 
  geom_vline(aes(xintercept = 12880), color = 'red', linetype = 'dashed') +
  labs(
    title = 'Family Income Distribution (Household Size = 1, Year = 2021)',
    x = 'Family Income (in dollars)',
    y = 'Number of Families'
  ) +
  theme_minimal() +
  annotate('text', x = 12880 + 500, y = 150, label = 'Poverty Line: $12,880', color = 'red', size = 4)
# save image into results folder
ggsave("Results/Povery_Line_HH_1.png", width = 12)

# lets look at 2022, for a family of 3
cps_2022_3 <- subset(cps_data, hhsize == 3 & YEAR == '2022' & !is.na(FamInc_numeric))

ggplot(data = cps_2022_3) + 
  geom_bar(aes(x = FamInc_numeric), stat = 'count', fill = 'skyblue', color = 'black') + 
  geom_vline(aes(xintercept = 23030), color = 'red', linetype = 'dashed') +
  labs(
    title = 'Family Income Distribution (Household Size = 3, Year = 2022)',
    x = 'Family Income (in dollars)',
    y = 'Number of Families'
  ) +
  theme_minimal() +
  annotate('text', x = 23030 + 500, y = 100, label = 'Poverty Line: $23,030', color = 'red', size = 4)
# save image into results folder
ggsave("Results/Poverty_Line_HH_3.png", width = 12)

# poverty line changes depending on size of household and year
# more families look to be above poverty line, but quite a bit of households are below 
# the poverty line
# not enough recent information to draw insights from

# ---------------

# looking into food spending efficiency across income levels
# is there a reoccurring pattern that shows 

# looking into proportion of income spent on food using family income numeric (lower bounds) 
# and FSTOTXPNC
cps_data <- cps_data %>%
  mutate(
    food_spending_efficiency = ifelse(!is.na(FamInc_numeric) & !is.na(FSTOTXPNC),
                                      FSTOTXPNC / FamInc_numeric, 
                                      NA) # efficiency computation
  )

# look into updated set
head(cps_data[, c("FamInc", "FamInc_numeric", "FSTOTXPNC", "food_spending_efficiency")])

# order income categories numerically - low to high
cps_data <- cps_data %>%
  mutate(FamInc = factor(FamInc, levels = unique(FamInc[order(FamInc_numeric)])))

# plotting
ggplot(cps_data, aes(x = FamInc, y = food_spending_efficiency)) +
  geom_boxplot(outlier.color = viridis(1, option = "D"), fill = viridis(1, option = "C"), alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, color = viridis(1, option = "E")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Income Levels",
    y = "Food Spending Efficiency (%)",
    title = "Food Spending Efficiency Across Income Levels",
    subtitle = "Proportion of Family Income Spent on Food",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )
# save image into results folder
ggsave("Results/Food_Spending_Efficiency_Untuned.png", width = 12)

# x-axis: display different income groups in order
# y-axis: represents the proportion of income spent on food (%)
# box plot median line: represents the median efficiency for 
# that income group
# box plot box edges: 25 and 75th percent quartile 
# box plot whiskers: extend to show range of of data points excluding outliers
# outliers: red data points that represent unusual values where families spent 
# an exceptionally high or low proportion of food based on income

# what do we find: 
# higher income houses typically have lower food spending efficiency values
# (spend a smaller percentage of income on food)
# lower income houses typically have a higher food spending efficiency on food
# (larger proportion of income goes towards food)

# some variation within each spending group is shown
# there is a significant drop in efficiency as income rises
# this might indicate a threshold of where food expenses 
# form a distinct share of household budget

# issue with this is FSTOTXPNC is on the weekly level and family income is yearly
# is there a way to compute FSTOTXPNC on the yearly level or compute family income 
# on the weekly level

# adjusting food spending to yearly as it originally is weekly
cps_data <- cps_data %>%
  mutate(
    yearly_food_spending = FSTOTXPNC * 52,  # convert weekly to yearly
    food_spending_efficiency = ifelse(
      !is.na(FamInc_numeric) & !is.na(FSTOTXPNC),
      ((FSTOTXPNC * 52) / FamInc_numeric),  # proportion of yearly income spent on food
      NA
    )
  )

# reordering income categories numerically - low to high
cps_data <- cps_data %>%
  mutate(FamInc = factor(FamInc, levels = unique(FamInc[order(FamInc_numeric)])))

# plotting
ggplot(cps_data, aes(x = FamInc, y = food_spending_efficiency)) +
  geom_boxplot(outlier.color = viridis(1, option = "D"), fill = viridis(1, option = "C"), alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, color = viridis(1, option = "E")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Income Levels",
    y = "Food Spending Efficiency (%)",
    title = "Food Spending Efficiency Across Income Levels",
    subtitle = "Proportion of Yearly Income Spent on Food"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )
# save image into results folder
ggsave("Results/Food_Spending_Efficiency_Tuned.png", width = 12)

# efficiency ranges from 0% to over 400%, this is not helpful
# can we cap it at 100% 
# starting with 150% first

# filter out cases where food spending efficiency is above 150%
cps_data <- cps_data %>%
  filter(food_spending_efficiency <= 1.5)  # keep only cases with greater than or equal to 150%

# plotting again
ggplot(cps_data, aes(x = FamInc, y = food_spending_efficiency)) +
  geom_boxplot(outlier.color = viridis(1, option = "D"), fill = viridis(1, option = "C"), alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, color = viridis(1, option = "E")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Income Levels",
    y = "Food Spending Efficiency (%)",
    title = "Food Spending Efficiency Across Income Levels",
    subtitle = "Proportion of Yearly Income Spent on Food (Capped at 150%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )
# save image into results folder
ggsave("Results/Food_Spending_Efficiency_Cap_150.png", width = 12)

# cap food spending efficiency at 100%
cps_data <- cps_data %>%
  mutate(food_spending_efficiency = pmin(food_spending_efficiency, 1))  # cap at 100%

# plotting again
ggplot(cps_data, aes(x = FamInc, y = food_spending_efficiency)) +
  geom_boxplot(outlier.color = viridis(1, option = "D"), fill = viridis(1, option = "C"), alpha = 0.6) +
  geom_jitter(width = 0.2, alpha = 0.5, color = viridis(1, option = "E")) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    x = "Income Levels",
    y = "Food Spending Efficiency (%)",
    title = "Food Spending Efficiency Across Income Levels",
    subtitle = "Proportion of Yearly Income Spent on Food (Capped at 100%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )
# save image into results folder
ggsave("Results/Food_Spending_Efficiency_Cap_100.png", width = 12)

# those that are spending more than 100% of their yearly income on food are likely getting
# additional support from food services, or loans, savings, etc.


# ------------------

# let's look into proportion of food security status in the past 30 days and family income 

ggplot(cps_data, aes(x = FamInc, fill = as.factor(FSSTATUSMD))) + 
  geom_bar(position = "fill") + 
  theme_minimal() + 
  scale_fill_discrete(
    labels = c("0" = "High food security", "1" = "Marginal / Low / Very \nlow food security")
  ) +
  scale_fill_brewer(palette = "Set2",  # colorblind palette
                    labels = c("0" = "High food security", "1" = "Marginal / Low / Very \nlow food security")) + 
  labs(
    x = "Family Income", # x label
    y = "Proportion", # y label
    fill = "Food Security Status", # fill based on food security status
    title = "Family Income in Comparison to Food Security Status"
  ) + 
  coord_flip() + 
  theme( # make labels stick out
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold")
  )
# save image into results
ggsave("Results/Proportion_Income_Food_Security.png", width = 12)

ggplot(cps_data, aes(x = FamInc, fill = as.factor(FSSTATUSMD))) + 
  geom_bar(position = "dodge") +  # create two separate bars on chart
  theme_minimal() + 
  scale_fill_discrete(
    labels = c("0" = "High food security", "1" = "Marginal / Low / Very \nlow food security")
  ) +
  scale_fill_brewer(palette = "Set2",  # colorblind palette
                    labels = c("0" = "High food security", "1" = "Marginal / Low / Very \nlow food security")) + 
  labs(
    x = "Family Income", # x label
    y = "Count of Households With Elderly Individuals",  # y label 
    fill = "Food Security Status", 
    title = "Count of Elderly in Comparison to Food Security Status Across Family Income"
  ) + 
  coord_flip() +  # flip axis for easy readability
  theme( # make labels stick out
    plot.title = element_text(face = "bold"), 
    axis.text.x = element_text(face = "bold"), 
    axis.text.y = element_text(face = "bold")
  )
# save image into results folder
ggsave("Results/Distribution_Elderly_Food_Security.png", width = 12)

# insights: 
# families that make above six figures look to have high food security status which is to be expected
# families that make $25,000 and under look to have a good amount of families that are low in food security
# $20,000 - $24,999 and under $5,000 have the highest proportion of families that are low in food security,
# with families that have income $10,000-13,000 also having a good proportion of families that are low in food security

# ---------------

# checking type of columns
str(cps_data)
# looking into variable types

# fitting lasso and ridge regression
# remove Y variables and ID 
cps_data_model <- cps_data %>% select(-c(CPSID, COUNTY, FSTOTXPNC_perpers, FSSTATUS, FSFOODS, FSWROUTY,
                                   FSBAL, FSRAWSCRA, FSTOTXPNC, STATE, YEAR, FamInc_numeric))

# if imputing: 
# make sure FSSTATUS MD contains no NA values
#mode <- as.integer(names(sort(table(cps_data_model$FSSTATUSMD), decreasing = TRUE))[1])
#cps_data_model$FSSTATUSMD[is.na(cps_data_model$FSSTATUSMD)] <- mode

# if FALSE is returned then there are no NA values
table(is.na(cps_data_model))

# converting FSSTATUSMD into a factor with 2 levels
cps_data$FSSTATUSMD <- as.factor(cps_data$FSSTATUSMD) 


# starting lasso and ridge regression
RNGkind(sample.kind = "default")
set.seed(12252024)
# creating training and testing split
train.idx <- sample(x = 1:nrow(cps_data_model), size = .7*nrow(cps_data_model))
train.df <- cps_data_model[train.idx, ]
test.df <- cps_data_model[-train.idx, ]

# model matrix creating for x train and testing sets
x.train <- model.matrix(FSSTATUSMD ~ hhsize + female + hispanic +
                          black + elderly + kids + education + married, 
                        data = train.df)[, -1]
x.test <- model.matrix(FSSTATUSMD ~ hhsize + female + hispanic +
                         black + elderly + kids + education + married, data = test.df)[, -1]

# creating vectors for y train and test sets
y.train <- as.vector(train.df$FSSTATUSMD)
y.test <- as.vector(test.df$FSSTATUSMD)


# insure all are correct form
class(y.train)
y.train <- as.numeric(y.train)

# fitting initial lasso before tuning
lr_lasso_cv <- cv.glmnet(x.train,
                         y.train,
                         family = binomial(link = 'logit'),
                         alpha = 1,
                         weights = as.integer(train.df$weight))

# fitting initial ridge before tuning
lr_ridge_cv <- cv.glmnet(x.train,
                         y.train,
                         family = binomial(link = 'logit'),
                         alpha = 0,
                         weights = as.integer(train.df$weight))

plot(lr_lasso_cv)
plot(lr_ridge_cv)

# creating variable to store best lambda for both lasso and ridge regression
best_lasso_lambda <- lr_lasso_cv$lambda.min
best_ridge_lambda <- lr_ridge_cv$lambda.min

# creating variable to store coef for lasso and ridge regression
lr_lasso_coef <- coef(lr_lasso_cv, s = "lambda.min") %>% as.matrix()
lr_ridge_coef <- coef(lr_ridge_cv, s = "lambda.min") %>% as.matrix()

# plotting coef
ggplot() + 
  geom_point(aes(lr_ridge_coef, lr_lasso_coef)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-10, 10)) + 
  ylim(c(-10, 10))

# final lasso model using the best lambda
final_lasso <- glmnet(x.train, y.train, 
                      family = binomial(link = 'logit'),
                      alpha = 1, # for lasso
                      weights = as.integer(train.df$weight),
                      lambda = best_lasso_lambda) # tune model based on cv

# final ridge model using the best lambda
final_ridge <- glmnet(x.train, y.train, 
                      family = binomial(link = 'logit'),
                      alpha = 0, # for ridge
                      weights = as.integer(train.df$weight),
                      lambda = best_ridge_lambda) # tune model based on cv

# creating prediction metric from the testing set of cps data
test.df.preds <- test.df %>%
  mutate(
    # note: lasso gets the matrix
    lasso_pred = predict(final_lasso, x.test, type = 'response')[,1],
    # note: ridge gets the matrix
    ridge_pred = predict(final_ridge, x.test, type = 'response')[,1]
    # note: all need type = 'response' so we don't use log odds
  )


# plotting ROC curves for both lasso and ridge regression
lasso_rocCurve <- roc(response = as.factor(test.df.preds$FSSTATUSMD), # truth
                      predictor = test.df.preds$lasso_pred, # predicted preds of MLE
                      levels = c('0', '1')) # positive event comes second

ridge_rocCurve <- roc(response = as.factor(test.df.preds$FSSTATUSMD), # truth
                      predictor = test.df.preds$ridge_pred, # predicted preds of MLE
                      levels = c('0', '1')) # positive event comes second

# make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
# make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)

# combine all the data frames
roc_data <- rbind(lasso_data, ridge_data)


# plot the data together
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data) +
  geom_text(data = roc_data %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()
# save image into results folder
ggsave("Results/ROC_Curve_Lasso_Ridge_1.png", width = 12)

# AUC (area under curve) : lasso: 0.61
# ridge: 0.609

# -------

# would using the proportion of each demographic x column be more valuable
# as there could be interaction terms

predictive_columns <- c("female", "hispanic", "black", "kids", "education", "married", "elderly")

# calculate proportion from hhsize
cps_data_proportions <- cps_data_model %>%
  mutate(across(
    all_of(predictive_columns), 
    ~ . / hhsize,  # divide each column by household size
    .names = "prop_{.col}" # create new proportional columns
  ))

# starting lasso and ridge regression
RNGkind(sample.kind = "default")
set.seed(1225202410)
# creating training and testing split
train.idx.p <- sample(x = 1:nrow(cps_data_proportions), size = .7*nrow(cps_data_proportions))
train.df.p <- cps_data_proportions[train.idx.p, ]
test.df.p <- cps_data_proportions[-train.idx.p, ]
train.df.p$FSSTATUSMD <- as.factor(train.df.p$FSSTATUSMD)
test.df.p$FSSTATUSMD <- as.factor(test.df.p$FSSTATUSMD)

# model matrix creating for x train and testing sets
# train model matrix
x.train.p <- model.matrix(FSSTATUSMD ~ hhsize + prop_female + prop_hispanic +
                          prop_black + prop_elderly + prop_kids + prop_education + prop_married, 
                        data = train.df.p)[, -1]
# test model matrix
x.test.p <- model.matrix(FSSTATUSMD ~ hhsize + prop_female + prop_hispanic +
                         prop_black + prop_elderly + prop_kids + prop_education + prop_married,
                       data = test.df.p)[, -1]

# creating vectors for y train and test sets
y.train.p <- as.vector(train.df.p$FSSTATUSMD)
y.test.p <- as.vector(test.df.p$FSSTATUSMD)

# insure all are correct form
class(y.train.p)
y.train.p <- as.numeric(y.train.p)

# fitting initial lasso before tuning
lr_lasso_cv.p <- cv.glmnet(x.train.p,
                         y.train.p,
                         family = binomial(link = 'logit'),
                         alpha = 1,
                         weights = as.integer(train.df.p$weight))

# fitting initial ridge before tuning
lr_ridge_cv.p <- cv.glmnet(x.train.p,
                         y.train.p,
                         family = binomial(link = 'logit'),
                         alpha = 0,
                         weights = as.integer(train.df.p$weight))

# plot intitial lasso's and ridge regression
plot(lr_lasso_cv.p)
plot(lr_ridge_cv.p)

# creating variable to store best lambda for both lasso and ridge regression
best_lasso_lambda.p <- lr_lasso_cv.p$lambda.min
best_ridge_lambda.p <- lr_ridge_cv.p$lambda.min

# creating variable to store coef for lasso and ridge regression
lr_lasso_coef.p <- coef(lr_lasso_cv.p, s = "lambda.min") %>% as.matrix()
lr_ridge_coef.p <- coef(lr_ridge_cv.p, s = "lambda.min") %>% as.matrix()

# plotting coefficients
ggplot() + 
  geom_point(aes(lr_ridge_coef.p, lr_lasso_coef.p)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-10, 10)) + 
  ylim(c(-10, 10))

# final lasso model using the best lambda
final_lasso.p <- glmnet(x.train.p, y.train.p, 
                      family = binomial(link = 'logit'),
                      alpha = 1, # for lasso
                      weights = as.integer(train.df.p$weight),
                      lambda = best_lasso_lambda.p) # tune model based on cv

# final ridge model using the best lambda
final_ridge.p <- glmnet(x.train.p, y.train.p, 
                      family = binomial(link = 'logit'),
                      alpha = 0, # for ridge
                      weights = as.integer(train.df.p$weight),
                      lambda = best_ridge_lambda.p) # tune model based on cv

# creating prediction metric from the testing set of cps data
test.df.p.preds <- test.df.p %>%
  mutate(
    # lasso gets the matrix
    lasso_pred.p = predict(final_lasso.p, x.test.p, type = 'response')[,1],
    # ridge gets the matrix
    ridge_pred.p = predict(final_ridge.p, x.test.p, type = 'response')[,1]
  )


# plotting ROC curves for both lasso and ridge regression
# ROC curve for lasso
lasso_rocCurve.p <- roc(response = as.factor(test.df.p.preds$FSSTATUSMD), # truth
                      predictor = test.df.p.preds$lasso_pred.p, # predicted preds of MLE
                      levels = c('0', '1')) # positive event comes second
# ROC curve for ridge
ridge_rocCurve.p <- roc(response = as.factor(test.df.p.preds$FSSTATUSMD), # truth
                      predictor = test.df.p.preds$ridge_pred.p, # predicted preds of MLE
                      levels = c('0', '1')) # positive event comes second

# make data frame of lasso ROC info
lasso_data.p <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve.p$specificities,
  Sensitivity = lasso_rocCurve.p$sensitivities,
  AUC = lasso_rocCurve.p$auc %>% as.numeric
)
# make data frame of ridge ROC info
ridge_data.p <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve.p$specificities,
  Sensitivity = ridge_rocCurve.p$sensitivities,
  AUC = ridge_rocCurve.p$auc%>% as.numeric
)

# combine all the data frames
roc_data.p <- rbind(lasso_data.p, ridge_data.p)


# plot the data together
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data.p) +
  geom_text(data = roc_data.p %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()
# save image into results folder
ggsave("Results/ROC_Curve_Lasso_Ridge_2.png", width = 12)
# AUC: lasso .61
# ridge: 0.609

# ------------------

# could an interaction term be used
# let's look into this
# loading libraries

# select proportional numeric columns to find correlation
selected_vars <- cps_data_proportions[, c("hhsize", "prop_female", "prop_hispanic", "prop_black", "prop_education", 
                              "prop_elderly", "prop_married", "prop_kids")]
# create correlation matrix
cor_matrix <- cor(selected_vars, use = "complete.obs")
cor_melt <- melt(cor_matrix)

# plot correlation heat map
ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis(
    option = "C",  # Use the "C" option for a perceptually uniform diverging scale
    direction = 1, # Default direction (positive values go to brighter colors)
    name = "Correlation" ) +  # Legend title +
  theme_minimal() +
  labs(title = "Correlation Matrix", fill = "Correlation")
# save image into results folder
ggsave("Results/Correlation_Map.png", width = 12)

# testing to find interaction terms in lasso or ridge regression using coefficients after
# modeling with lasso and rige
RNGkind(sample.kind = "default")
set.seed(120840281)
# create train and test split
train.idx.interaction <- sample(x = 1:nrow(cps_data_proportions), size = .7*nrow(cps_data_proportions))
train.df.interaction <- cps_data_proportions[train.idx.interaction, ]
test.df.interaction <- cps_data_proportions[-train.idx.interaction, ]
train.df.interaction$FSSTATUSMD <- as.factor(train.df.interaction$FSSTATUSMD)
test.df.interaction$FSSTATUSMD <- as.factor(test.df.interaction$FSSTATUSMD)

# model matrix using all interaction terms
x.train.interaction <- model.matrix(
  FSSTATUSMD ~ (hhsize + prop_black + prop_hispanic + prop_female +
                  prop_elderly + prop_kids + prop_education + prop_married)^2, 
  data = train.df.interaction)[, -1]

x.test.interaction <- model.matrix(
  FSSTATUSMD ~ (hhsize + prop_black + prop_hispanic + prop_female +
                  prop_elderly + prop_kids + prop_education + prop_married)^2, 
  data = test.df.interaction)[, -1]

# y train and test vectors with interaction terms
y.train.interaction <- as.vector(train.df.interaction$FSSTATUSMD)
y.test.interaction <- as.vector(test.df.interaction$FSSTATUSMD)
# ensure all columns are in correct form
train.df.interaction$weight <- as.numeric(train.df.interaction$weight)
y.train.interaction <- as.numeric(as.character(y.train.interaction))

# lasso model
lasso_model <- cv.glmnet(
  x.train.interaction, y.train.interaction, 
  alpha = 1, 
  family = binomial(link = "logit"), 
  weights = as.integer(train.df.interaction$weight)
)

# ridge model
ridge_model <- cv.glmnet(
  x.train.interaction, y.train.interaction, 
  alpha = 0, 
  family = binomial(link = "logit"), 
  weights = as.integer(train.df.interaction$weight)
)

# looking at lasso coefficients
lasso_coef <- coef(lasso_model, s = "lambda.min")
lasso_coef <- as.data.frame(as.matrix(lasso_coef))
head(lasso_coef)
colnames(lasso_coef)[1] <- "value"  # rename the first column to value for reproducability
significant_lasso <- as.data.frame(as.matrix(lasso_coef)) %>%
  filter(value != 0)  # keep only non-zero coefficients
significant_lasso
# interaction terms for lasso: prop_black:prop_hispanic, prop_kids:prop_married,
# and prop_hispanic:prop_education

# looking at ridge coefficients
ridge_coef <- coef(ridge_model, s = "lambda.min")
ridge_coef <- as.data.frame(as.matrix(ridge_coef))
head(ridge_coef)
colnames(ridge_coef)[1] <- "value"  # # rename the first column to value for reproducability
significant_ridge <- as.data.frame(as.matrix(ridge_coef)) %>%
  arrange(desc(abs(value)))  # ridge uses absolute values
significant_ridge
# interaction terms for ridge: prop_black:prop_hispanic, prop_kids:prop_married,
# prop_hispanic:prop_education

# going to use prob_blaack:prop_hispanic and prop_kids:prop_married for 
# both lasso and ridge regression

# --------------------

# starting lasso and ridge regression with interaction terms from above
RNGkind(sample.kind = "default")
set.seed(1302904)
# creating training and testing split
train.idx.int <- sample(x = 1:nrow(cps_data_proportions), size = .7*nrow(cps_data_proportions))
train.df.int <- cps_data_proportions[train.idx.int, ]
test.df.int <- cps_data_proportions[-train.idx.int, ]
train.df.int$FSSTATUSMD <- as.factor(train.df.int$FSSTATUSMD)
test.df.int$FSSTATUSMD <- as.factor(test.df.int$FSSTATUSMD)


# model matrix creating for x train and testing sets
x.train.int <- model.matrix(FSSTATUSMD ~ hhsize + prop_black*prop_hispanic +
                            prop_female + prop_elderly + prop_kids*prop_married + prop_education, 
                          data = train.df.int)[, -1]
x.test.int <- model.matrix(FSSTATUSMD ~ hhsize + prop_black*prop_hispanic +
                             prop_female + prop_elderly + prop_kids*prop_married + prop_education,
                         data = test.df.int)[, -1]

# creating vectors for y train and test sets
y.train.int <- as.vector(train.df.int$FSSTATUSMD)
y.test.int <- as.vector(test.df.int$FSSTATUSMD)


# insure all are correct form
class(y.train.int)
y.train.int <- as.numeric(y.train.int)

# fitting initial lasso before tuning
lr_lasso_cv.int <- cv.glmnet(x.train.int,
                           y.train.int,
                           family = binomial(link = 'logit'),
                           alpha = 1,
                           weights = as.integer(train.df.int$weight))

# fitting initial ridge before tuning
lr_ridge_cv.int <- cv.glmnet(x.train.int,
                           y.train.int,
                           family = binomial(link = 'logit'),
                           alpha = 0,
                           weights = as.integer(train.df.int$weight))

plot(lr_lasso_cv.int)
plot(lr_ridge_cv.int)


# creating variable to store best lambda for both lasso and ridge regression
best_lasso_lambda.int <- lr_lasso_cv.int$lambda.min
best_ridge_lambda.int <- lr_ridge_cv.int$lambda.min

# creating variable to store coef for lasso and ridge regression
lr_lasso_coef.int <- coef(lr_lasso_cv.int, s = "lambda.min") %>% as.matrix()
lr_ridge_coef.int <- coef(lr_ridge_cv.int, s = "lambda.min") %>% as.matrix()

# plotting coefficients
ggplot() + 
  geom_point(aes(lr_ridge_coef.int, lr_lasso_coef.int)) + 
  geom_abline(aes(slope = 1, intercept = 0)) + 
  xlim(c(-10, 10)) + 
  ylim(c(-10, 10))

# final lasso model using the best lambda
final_lasso.int <- glmnet(x.train.int, y.train.int, 
                        family = binomial(link = 'logit'),
                        alpha = 1, # for lasso
                        weights = as.integer(train.df.int$weight),
                        lambda = best_lasso_lambda.int) # tune model based on cv

# final ridge model using the best lambda
final_ridge.int <- glmnet(x.train.int, y.train.int, 
                        family = binomial(link = 'logit'),
                        alpha = 0, # for ridge
                        weights = as.integer(train.df.int$weight),
                        lambda = best_ridge_lambda.int) # tune model based on cv

# creating prediction metric from the testing set of cps data
test.df.int.preds <- test.df.int %>%
  mutate(
    # lasso gets the matrix
    lasso_pred.int = predict(final_lasso.int, x.test.int, type = 'response')[,1],
    # ridge gets the matrix
    ridge_pred.int = predict(final_ridge.int, x.test.int, type = 'response')[,1]
  )


# plotting ROC curves for both lasso and ridge regression
# ROC curve lasso
lasso_rocCurve.int <- roc(response = as.factor(test.df.int.preds$FSSTATUSMD), # truth
                        predictor = test.df.int.preds$lasso_pred.int, # predicted preds of MLE
                        levels = c('0', '1')) # positive event comes second
# ROC curve ridge
ridge_rocCurve.int <- roc(response = as.factor(test.df.int.preds$FSSTATUSMD), # truth
                        predictor = test.df.int.preds$ridge_pred.int, # predicted preds of MLE
                        levels = c('0', '1')) # positive event comes second

# make data frame of lasso ROC info
lasso_data.int <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve.int$specificities,
  Sensitivity = lasso_rocCurve.int$sensitivities,
  AUC = lasso_rocCurve.int$auc %>% as.numeric
)
# make data frame of ridge ROC info
ridge_data.int <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve.int$specificities,
  Sensitivity = ridge_rocCurve.int$sensitivities,
  AUC = ridge_rocCurve.int$auc%>% as.numeric
)

# combine all the data frames
roc_data.int <- rbind(lasso_data.int, ridge_data.int)

# plot the data together
ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model),data = roc_data.int) +
  geom_text(data = roc_data.int %>% group_by(Model) %>% slice(1), 
            aes(x = 0.75, y = c(0.65, 0.55), colour = Model,
                label = paste0(Model, " AUC = ", round(AUC, 3)))) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()
# save image into results folder
ggsave("Results/ROC_Curve_Lasso_Ridge_Final.png", width = 12)

# AUC: lasso: 0.652
# ridge: 0.65

# Extract the optimal threshold for Lasso model using coords function
lasso_optimal_coords <- coords(lasso_rocCurve.int, "best", ret = "threshold")
lasso_optimal_threshold <- lasso_optimal_coords$threshold

# Print the optimal threshold for Lasso
lasso_optimal_threshold
# 0.2861639


# find coefficients for lasso with interaction terms as this is the overall best model:
final_lasso_coef <- coef(final_lasso.int, s = best_lasso_lambda.int)
final_lasso_coef

# interpretation for these coefficients: 
# (Intercept)              -0.66966308
# hhsize                   -0.04845382
# prop_black                0.18918708
# prop_hispanic             0.24668906
# prop_female               0.29911520
# prop_elderly             -0.50284302
# prop_kids                 0.76971748
# prop_married             -0.20550229
# prop_education           -0.51921701
# prop_black:prop_hispanic -0.81904427
# prop_kids:prop_married   -2.02730782

# FOOD INSECURE OF LAST 30 DAYS
# larger households slightly decrease the probability of being food insecure 
# higher proportions of black individuals increase the probability of food insecure
# higher proportions of hispanic individuals increase the probability of being food insecure
# higher proportions of females in a household increase the probability of being food insecure
# higher proportions of elderly members in a household decreases the 
# probability of being food insecure
# higher proportions of children increases the probability of being food insecure
# higher proportions of married individuals decrease the probability of being food insecure
# higher education levels reduces the probability of being food insecure
# households with a higher proportion of both black and hispanic individuals 
# experience a decrease in the probability of being food insecure
# households with high proportions of children and married members decrease the probability
# of being food insecure

# probability of a senior being food insecure in a house with 3 others, no black people, no hispanic
# people no married people no kids and are all educated: 
odds_4 <- (-0.66966308+(-0.04845382*4)+(0.18918708*0)+(0.24668906*0)+(0.29911520*0)+(-0.50284302*0)+
  (0.76971748*0)+(-0.20550229*0)+(-0.51921701*1)+(-0.81904427*0)+(-2.02730782*0))
prob_4 <- (1/(1+(exp(-odds_4))))
# 0.2005765
# the probability that a senior living in this household is food insecure is 20%

# probability of a senior being food insecure in a house with 3 others, no black people,
# no hispanic people, no married people, no kids, and no education: 
odds_4_ed <- (-0.66966308+(-0.04845382*4)+(0.18918708*0)+(0.24668906*0)+(0.29911520*0)+(-0.50284302*0)+
                (0.76971748*0)+(-0.20550229*0)+(-0.51921701*0)+(-0.81904427*0)+(-2.02730782*0))
prob_4_ed <- (1/(1+(exp(-odds_4_ed))))
# 0.2966131

# the probability that a senior living in this household is food insecure is 29.6% - almost
# 10 % higher than with educated members in the family

odds_4_fem <- (-0.66966308+(-0.04845382*4)+(0.18918708*0)+(0.24668906*0)+(0.29911520*1)+(-0.50284302*0)+
             (0.76971748*0)+(-0.20550229*0)+(-0.51921701*1)+(-0.81904427*0)+(-2.02730782*0))
prob_4_fem <- (1/(1+(exp(-odds_4_fem))))
# 0.2528291

odds_4_man <- (-0.66966308+(-0.04845382*4)+(0.18918708*0)+(0.24668906*0)+(0.29911520*0)+(-0.50284302*0)+
                 (0.76971748*0)+(-0.20550229*0)+(-0.51921701*1)+(-0.81904427*0)+(-2.02730782*0))
prob_4_man <- (1/(1+(exp(-odds_4_man))))
# 0.2005765

# --------------------

# would a random forest be useful:

# baseline forest
RNGkind(sample.kind = "default")
set.seed(01252024)
train.idx.f <- sample(x = 1:nrow(cps_data_model), size = .7*nrow(cps_data_model))
train.df.f <- cps_data_model[train.idx.f, ]
test.df.f <- cps_data_model[-train.idx.f, ]
train.df.f$FSSTATUSMD <- as.factor(train.df.f$FSSTATUSMD)

# random forest before tuning
myforest <- randomForest(FSSTATUSMD ~ hhsize + female + hispanic +
               black + elderly + kids + education + married, 
             data = train.df.f,
             ntree = 1000,
             mtry = 3, 
             weights = as.integer(train.df.f$weight),
             importance = TRUE)

myforest

# calculate accuracy using confusionMatrix
train.pred <- predict(myforest, train.df.f)
train.cm <- confusionMatrix(train.pred, train.df.f$FSSTATUSMD)
print(train.cm)

# calculate OOB error (1 - accuracy)
oob_error <- 1 - train.cm$overall["Accuracy"]
cat("OOB Error Rate: ", oob_error, "\n")

(12122+327)/(12122+72+893+327)
# Accuracy : 0.9332 
# OOB: 0.06679588

# loop through each unique element in the vector below
Btry = c(15, 25, seq(from = 50, to = 100, by = 50))

# make room for B, OOB error
keeps = data.frame(ntree = rep(NA, length(Btry)),
                   OOB_error_rate = rep(NA, length(Btry)))

for (idx in 1:length(Btry)) {
  tempforest = randomForest(FSSTATUSMD ~ hhsize + female + hispanic +
                              black + elderly + kids + education + married,
                            data = train.df.f,
                            ntree = Btry[idx],
                            mtry = 6,
                            weights = as.integer(train.df.f$weight))
  # record how many trees we trued
  keeps[idx, "ntree"] = Btry[idx]
  # record what our OOB error rate was
  keeps[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.df.f$FSSTATUSMD)
  
}

qplot(ntree, OOB_error_rate, geom = c("point"), data = keeps) + 
  theme_bw() + labs(x = "Number of Trees", y = "OOB Error Rate")


# a sequence of m (# of explanatory sampled at each tree) that we want to 
# try 
# loop through each unique element in the vector below
# range of m values that we want to try
mtry = c(1:12) 

# make room for B, OOB error
keeps2 = data.frame(m = rep(NA, length(mtry)),
                    OOB_error_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)) {
  tempforest = randomForest(FSSTATUSMD ~ hhsize + female + hispanic +
                              black + elderly + kids + education + married,
                            data = train.df.f,
                            ntree = 1000,
                            mtry = mtry[idx],
                            weights = as.integer(train.df.f$weight))
  
  keeps2[idx, "m"] = mtry[idx]
  
  keeps2[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.df.f$FSSTATUSMD)
}

# print results
keeps2

# plot results
ggplot(data = keeps2) + 
  geom_line(aes(m, OOB_error_rate)) + 
  theme_bw() + labs(x = "m (mtry) value", y = "OOB Error Rate")

# final forest after tuning
finalforest <- randomForest(FSSTATUSMD ~ hhsize + female + hispanic +
                              black + elderly + kids + education + married,
                            data = train.df.f,
                            ntree = 1000,
                            mtry = 5, 
                            weights = as.integer(train.df.f$weight),
                            importance = TRUE)

pi_hat = predict(finalforest, test.df.f, type = "prob")[, "1"]
rocCurve = roc(response = test.df$FSSTATUSMD,
               predictor = pi_hat,
               levels = c("0", "1"))
# plot ROC curve using best threshold
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

# finding best coords to use as threshold for preditions
pi_star = coords(rocCurve, "best", ret = "threshold")$threshold[1]
pi_star
# overwrite those old predictions with something better
test.df.f$forest_pred = as.factor(ifelse(pi_hat > pi_star, "1", "0"))

# variable importance plot with x variables
varImpPlot(finalforest, type = 1)

# --------------

# best model: lasso with interaction terms + proportions
# will use this to predict total number of seniors in each PUMA with acs data

# ------------

# predicting on acs data

# ----------------

# compute proportions on acs data
acs_prob_col <- c("female", "hispanic", "black", "kids", "education", "married", "elderly")

# calculate proportion from household size just like what was done in cps
# must make data sets equal
acs_data_prob <- acs_data %>%
  mutate(across(
    all_of(acs_prob_col), 
    ~ . / hhsize,  # divide each column by household size
    .names = "prop_{.col}" # Create new proportional columns
  ))

# remove columns that are not proportions
acs_data_prob <- acs_data_prob %>% select(-female, -hispanic, -black, -kids, -education, -married, -elderly)

# create the model matrix for the acs data with interaction terms (same formula as for training)
x.acs.int <- model.matrix( ~ hhsize + prop_black*prop_hispanic +
                             prop_female + prop_elderly + prop_kids*prop_married + prop_education, 
                          data = acs_data_prob)[, -1]  # remove intercept

# ensure that acs data is formatted properly 
# predicting using the Lasso model
acs_data_prob$lasso_pred <- predict(final_lasso.int, x.acs.int, type = 'response')[, 1]

acs_data_prob$lasso_class <- ifelse(acs_data_prob$lasso_pred > 0.2861639, "1", "0")

# View the predictions for Lasso and Ridge models
head(acs_data_prob[, c("lasso_pred", "lasso_class")])

unique(acs_data_prob$lasso_class)
table(acs_data_prob$lasso_class)

# ------------

# clorapleth map of iowa using PUMA
seniors_puma <- read.csv("Data/total_iowa_seniors_by_puma.csv", stringsAsFactors = TRUE)

# download PUMA shape file for Iowa - help of chatGPT
iowa_pumas <- pumas(state = "IA", cb = TRUE, year = 2020)

# ensure each column lines up with one another 
# rename function is necessary here
seniors_puma <- seniors_puma %>% rename(GEOID20 = GEOID)
iowa_pumas$GEOID20 <- as.character(iowa_pumas$GEOID20)
seniors_puma$GEOID20 <- as.character(seniors_puma$GEOID20)

# merge the shape file with senior population data
iowa_pumas <- left_join(iowa_pumas, seniors_puma, by = "GEOID20")

# create the choropleth map 
ggplot(data = iowa_pumas) +
  geom_sf(aes(fill = senior_population), color = "black", size = 0.2) +
  scale_fill_distiller(
    name = "Senior Population", 
    palette = "YlGnBu",  # colorblind palette
    direction = 1       
  ) +
  labs(
    title = "Senior Population by PUMA in Iowa"
  ) +
  theme_minimal()
# save image into results folder
ggsave("Results/Iowa_Map_Senior_Population.png", width = 12)

# ----------------------

# merge the lasso predictions with the senior population data 
seniors_puma <- seniors_puma %>% rename(PUMA = GEOID20)
acs_data_prob <- left_join(acs_data_prob, seniors_puma, by = "PUMA")

# ------------

# summarize food insecurity by PUMA
# calculate weighted food insecurity using total_seniors and lasso_pred
puma_averages <- acs_data_prob %>%
  group_by(PUMA) %>%
  summarize(
    weighted_food_insecurity = weighted.mean(lasso_pred, senior_population, na.rm = TRUE), # using lasso_pred here
    weighted_food_insecurity_percent = weighted.mean(lasso_pred, senior_population, na.rm = TRUE) * 100,
    total_seniors = first(senior_population, na_rm = TRUE),
    average_hhsize = mean(hhsize, na.rm = TRUE)  
  ) %>%
  arrange(desc(weighted_food_insecurity))  # sort by food insecurity rate

# view the results
print(puma_averages)

# ------------

# merge puma_averages with senior population data to plot the choropleth map
puma_averages <- puma_averages %>% rename(GEOID20 = PUMA)
puma_merged <- left_join(iowa_pumas, puma_averages, by = "GEOID20")

# check the merged data to ensure it contains geometry and weighted_food_insecurity
head(puma_merged)

# create the choropleth map using ggplot
ggplot(data = puma_merged) +
  geom_sf(aes(fill = weighted_food_insecurity_percent), color = "black", size = 0.2) +
  scale_fill_distiller(
    name = "Weighted Food Insecurity %",
    palette = "YlGnBu",  # colorblind palette
    direction = 1) +
  labs(
    title = "Weighted Senior Food Insecurity by PUMA in Iowa",
  ) +
  theme_minimal()
# save image into results
ggsave("Results/Iowa_Map_Weighted_Senior_Insecurity.png", width = 12)

# calculate the number of food-insecure seniors
puma_averages <- puma_averages %>%
  mutate(
    food_insecure_seniors = (weighted_food_insecurity_percent / 100) * total_seniors
  )

# create a scatter plot of food-insecure seniors vs. senior population by PUMA
ggplot(puma_averages, aes(x = total_seniors, y = food_insecure_seniors)) +
  geom_point(aes(color = weighted_food_insecurity_percent), size = 3, alpha = 0.7) +
  scale_color_viridis_c(option = "C", name = "Food Insecurity (%)") +
  labs(
    title = "Food-Insecure Seniors vs. Senior Population by PUMA",
    subtitle = "Using Weighted Food Insecurity Percent",
    x = "Senior Population",
    y = "Number of Food-Insecure Seniors",
    caption = "Source: Your Data & US Census Bureau"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # move legend to the bottom of plot
# save image into results folder
ggsave("Results/Food_Insecure_Seniors_VS_Population_PUMA.png", width = 12)

# merge data sets with puma averages
iowa_pumas <- left_join(iowa_pumas, puma_averages, by = "GEOID20")

# plot 
ggplot(data = iowa_pumas) +
  geom_sf(aes(fill = food_insecure_seniors), color = "black", size = 0.2) +  # fill by food-insecure seniors
  scale_fill_distiller(
    name = "Food-Insecure Seniors",
    palette = "YlGnBu",  # colorblind palette
    direction = 1) +
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 20))  # move legend to the bottom of plot
# save image into results folder
ggsave("Results/Food_Insecure_Seniors_PUMA.png", width = 12)

# using chatGPT to help with modifications on map

# find the three PUMAs with the most food-insecure seniors
top_pumas <- iowa_pumas %>%
  arrange(desc(food_insecure_seniors)) %>%
  slice(1:3)  # select top 3 PUMAs

# plot the map
ggplot(data = iowa_pumas) +
  geom_sf(aes(fill = food_insecure_seniors), color = "black", size = 0.2) +  # fill by food-insecure seniors
  scale_fill_distiller(
    name = "Food-Insecure Seniors",
    palette = "YlGnBu",  # colorblind palette
    direction = 1) +
  geom_sf_text(
    data = top_pumas,
    aes(label = round(food_insecure_seniors)),
    color = "white",
    size = 3,  # ddjust text size
    fontface = "bold"  # bold text for emphasis
  ) +
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
    subtitle = "Top 3 PUMAs highlighted with the number of food-insecure seniors",
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # move legend to the bottom of plot
# save image into results folder
ggsave("Results/Food_Insecure_Seniors_Label.png", width = 12)

# identify top 3 PUMAs
top_3_pumas <- iowa_pumas %>%
  arrange(desc(food_insecure_seniors)) %>%
  slice(1:3)  # select top 3 PUMAs

# add a column to distinguish top 3 PUMAs
iowa_pumas <- iowa_pumas %>%
  mutate(highlight = ifelse(GEOID20 %in% top_3_pumas$GEOID20, "Top 3", "Other"))

# map with transparency for non-top PUMAs
ggplot(data = iowa_pumas) +
  geom_sf(aes(
    fill = highlight, 
    alpha = ifelse(highlight == "Top 3", 1, 0.4)
  ), color = "black", size = 0.2) +  # set transparency non top 3 PUMA
  geom_sf(
    data = top_3_pumas,  # highlighted PUMAs
    fill = NA,           # transparent fill
    color = "red",       # red border for emphasis
    size = 100           # Thicker border
  ) +
  geom_sf_text(
    data = top_3_pumas,
    aes(label = NAMELSAD20), 
    color = "black",
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Top 3" = "lightgrey", "Other" = "skyblue"),  # visualize Top 3
    name = "PUMA Category"
  ) +
  scale_alpha(guide = "none") + # remove legend
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
    subtitle = "Top 3 PUMA's Outlined",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # move legend to bottom
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_blank(),  # remove x-axis title since coordinates
    axis.title.y = element_blank()   # remove y-axis title since coordinates
  )
# save image into results folder
ggsave("Results/Food_Insecure_Seniors_Top_3.png", width = 12)

# Wesley Life serves in dallas and polk county: highlight those regions
# filter Polk and Dallas County geometry
highlighted_counties <- iowa_pumas %>%
  filter(str_detect(NAMELSAD20, "Polk|Dallas"))  # filter for Polk and Dallas

# map with Polk and Dallas Counties highlighted
ggplot(data = iowa_pumas) +
  geom_sf(aes(
    fill = highlight, 
    alpha = ifelse(highlight == "Top 3", 1, 0.3)  # transparency for non-highlighted areas
  ), color = "black", size = 0.2) +  
  geom_sf(
    data = top_3_pumas,  # highlighted PUMAs
    color = "black",     # black border for top 3
    size = 1             # thicker border
  ) +
  geom_sf_text(
    data = top_3_pumas,
    aes(label = NAMELSAD20),  
    color = "black",
    size = 4,
    fontface = "bold"
  ) +
  geom_sf(
    data = highlighted_counties,  # highlight Polk and Dallas Counties
    fill = NA,                    # transparent fill
    color = "red",                # red border for these counties
    size = 65                     # thicker red border
  ) +
  scale_fill_manual(
    values = c("Top 3" = "lightgrey", "Other" = "skyblue"),  # colorblind-safe colors
    name = "PUMA Category"
  ) +
  scale_alpha(guide = "none") +  # remove legend
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
    subtitle = "Top 3 PUMAs highlighted, with Polk and Dallas Counties outlined in red",
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # move legend to bottom
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_blank(),  # remove x-axis title since coordinate
    axis.title.y = element_blank()   # remove y-axis title since coordinate
  )
# save image into results folder
ggsave("Results/Food_Insecure_Seniors_Highlighted.png", width = 12)

# top three PUMA with most senior food insecurity within the last 30 days
# include Dubuque, Council Bluffs, and Des Moines (the city)
# since Welsey life here already covers Des Moines (since in polk county)
# can we find a place in the other 2 PUMA area's that offer
# meals to insecure seniors
# Council Bluffs: there is a meals on wheels in Council Bluffs, so pinpoint that 
# on the map
# Recommendation: Wesley Life partnership with the Council Bluffs location
# and work together to feed these seniors
# Dubuque: there are no meals on wheels programs in Dubuque - could expand or open
# a new franchise there or there is Northeast Iowa Area Agency on Aging Dubuque
# that works to supply meals to those seniors that are food insufficient
# - could partner with them to expand and create a partnership in Dubuque
# this location is pinpointed on the map

# create a data frame for the points
locations <- data.frame(
  name = c("Meals on Wheels Council Bluffs", "Northeast Iowa Area Agency on Aging, Dubuque"),
  latitude = c(41.2565, 42.5006243),  # latitude of locations
  longitude = c(-95.9345, -90.6647985)  # longitude of locations
)

# convert to sf object for compatibility with ggplot2
locations_sf <- st_as_sf(locations, coords = c("longitude", "latitude"), crs = 4326)

# ddd points to the map
ggplot(data = iowa_pumas) +
  geom_sf(aes(
    fill = highlight, 
    alpha = ifelse(highlight == "Top 3", 1, 0.3)  # transparency for non-highlighted areas
  ), color = "black", size = 0.2) +  
  geom_sf(
    data = top_3_pumas,  # highlighted PUMAs
    color = "black",     # black border for top 3
    size = 1             # thicker border
  ) +
  geom_sf_text(
    data = top_3_pumas,
    aes(label = NAMELSAD20),  
    color = "black",
    size = 4,
    fontface = "bold"
  ) +
  geom_sf(
    data = highlighted_counties,  # highlight Polk and Dallas Counties
    fill = NA,                    # transparent fill
    color = "red",                # red border for these counties
    size = 1.5                    # thicker red border
  ) +
  geom_sf(
    data = locations_sf,  # tlot the points of the two locations
    color = "black",       # color for points
    size = 3              # size of points
  ) +
  geom_sf_text(
    data = locations_sf, 
    aes(label = name),    # add names of the locations
    nudge_y = 0.1,        # slightly move labels to avoid overlap of words
    size = 3,
    color = "darkblue",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Top 3" = "#E69F00", "Other" = "#56B4E9"),  # colorblind-safe colors
    name = "PUMA Category"
  ) +
  scale_alpha(guide = "none") +  # remove legend titles
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
    subtitle = "Top 3 PUMAs highlighted with key agency locations marked",
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # move legend to bottom
    plot.title = element_text(face = "bold", size = 14)
  )

# make more appealing
# highlight top 3 PUMA counties and label them
ggplot(data = iowa_pumas) +
  geom_sf(aes(
    fill = highlight, 
    alpha = ifelse(highlight == "Top 3", 1, 0.3)  # transparency for non-highlighted areas
  ), color = "black", size = 0.2) +  
  geom_sf(
    data = top_3_pumas,  # highlighted PUMAs
    color = "black",     # black border for top 3
    size = 1             # thicker border
  ) +
  geom_sf(
    data = highlighted_counties,  # highlight Polk and Dallas Counties
    fill = NA,                    # transparent fill
    color = "red",                # red border for these counties
    size = 1.5                    # thicker red border
  ) +
  geom_sf(
    data = locations_sf,  # plot the points
    color = "blue",       # color for points
    size = 3              # size of points
  ) +
  # labels for the counties in top 3 PUMAs
  geom_text_repel(
    data = top_3_pumas,  # subset of counties within top 3 PUMAs
    aes(label = NAMELSAD20, geometry = geometry), 
    stat = "sf_coordinates", 
    size = 4, 
    color = "black", 
    fontface = "bold",
    nudge_y = 0.2,
    box.padding = 0.5,    # padding around labels
    point.padding = 0.3,  # space around points
    segment.color = NA    # no connecting lines for these labels
  ) +
  # labels for pinpoint locations
  geom_label_repel(
    data = locations_sf, 
    aes(label = name, geometry = geometry), 
    stat = "sf_coordinates", 
    size = 3,
    color = "darkblue",
    fontface = "bold",
    box.padding = 0.5,    
    point.padding = 0.3,  
    segment.color = "grey50",  
    seed = 42             
  ) +
  scale_fill_manual(
    values = c("Top 3" = "#E69F00", "Other" = "#56B4E9"),  
    name = "PUMA Category"
  ) +
  scale_alpha(guide = "none") +  
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
    subtitle = "Top 3 PUMAs highlighted with counties labeled"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",  # move legend to bottom
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_blank(),  # remove x-axis title as coordinates
    axis.title.y = element_blank()   # remove y-axis title as coordinates
  )
# save image into results folder
ggsave("Results/Food_Insecure_Seniors_External_Resources.png", width = 12)
