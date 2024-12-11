# Predicting Food Insecure Seniors in Iowa Using PUMA

This project analyzes factors contributing to food insecurity among seniors in Iowa at the PUMA (Public Use Microdata Area) level. Using advanced statistical modeling and machine learning techniques, the project identifies high-risk populations and provides insights for targeted interventions. 

## Introduction

Food insecurity is a critical issue, particularly among the elderly population. This project uses data from the U.S. Census Bureau and other sources to predict the likelihood of food insecurity among seniors. By identifying high-risk areas in Iowa, this analysis aims to guide policymakers and non-profit organizations in implementing targeted interventions. 
### Key Goals:
- Predict the probability of food insecurity at the household level using PUMA data.
- Highlight factors such as household composition, income, and demographics that influence food insecurity among seniors.
- Visualize results on a map to identify high-risk regions.

## Data Sources

### Primary Data: 
- CPS Data: Individual level data - aggregated onto household level - on demographics, income, and food security status.
- ACS Data: Demographic and socioeconomic data aggregated at the PUMA level.
### Secondary Data:
- Iowa shapefiles for PUMA boundaries.
- External datasets such as Meals on Wheels locations for additional resources.

## Methods

1. Data preperation:
- Aggregate both datasets onto family level and clean values.
- Proportions calculated for key variables
- Interaction terms and feature scaling applied for regression models.
2. Statistical Modeling:
- Lasso Regression: Used to predict food insecurity based on household characteristics and interactions.
- ROC Curve: Identified optimal probability threshold for classifying food insecurity.
- Clustering: Grouping food insecurity status results to predict which cluster a household falls under.
3. Geographic Analysis:
- Spatial join to link data with PUMA shapefiles.
- Mapping food insecurity levels and highlighting top PUMA's with high risk seniors throughout Iowa.

## Results

### Key Findings: 
- Proportion of elderly and household education levels significantly influence food insecurity.
- Specific PUMA's in Iowa, including Council Bluffs, Des Moines, and Dubuque show higher risks of food insecurity amongst seniors.

## Visualization

### Key Plots:
- Proportion of Food Insecure Seniors: Clorapleth map of Iowa - top PUMA's with food insecure seniors highlighted.
- Agency Locations: Added context for Meals on Wheels to partner with locations in PUMA's with high food insecure seniors.

