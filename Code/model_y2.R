##### PREP ####
rm (list=ls())

library(tidyverse)
library(ggthemes)
library(logistf)
library(glmnet)
library(haven)
library(reshape2)
library(cluster)
library(vegan)
library(pROC)
library(dplyr)
library(randomForest)
library(pROC)
library(sf)
library(tigris)
library(rpart.plot)
library(scales)
library(viridis)


source("code/clean_cps.R")
source("code/clean_acs.R")


summary(cps_data)
summary(acs_data)
# weights on PUMA

##### Impute Missing Values #####

# Function to calculate mode
get_mode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

cps_data <- cps_data %>%
  mutate(
    # Impute categorical/binary variables with mode
    FSSTATUS = ifelse(is.na(FSSTATUS), get_mode(FSSTATUS), FSSTATUS),
    FSSTATUSMD = ifelse(is.na(FSSTATUSMD), get_mode(FSSTATUSMD), FSSTATUSMD),
    FSFOODS = ifelse(is.na(FSFOODS), get_mode(FSFOODS), FSFOODS),
    
    
    # Impute numeric variables with median
   
    FSWROUTY = ifelse(is.na(FSWROUTY), median(FSWROUTY, na.rm = TRUE), FSWROUTY),
    FSBAL = ifelse(is.na(FSBAL), median(FSBAL, na.rm = TRUE), FSBAL),
    FSRAWSCRA = ifelse(is.na(FSRAWSCRA), median(FSRAWSCRA, na.rm = TRUE), FSRAWSCRA),
    FSTOTXPNC = ifelse(is.na(FSTOTXPNC), median(FSTOTXPNC, na.rm = TRUE), FSTOTXPNC),
   
    
  )

##### Create Clusters #####
# subset the Ys we want
cps_Y <- subset(cps_data,select = c(FSSTATUSMD,FSFOODS,FSSTATUS))
# Choosing to Omit NA's, imputed could skew data.
#cps_Y <- na.omit(cps_Y)
# 
cps_stand <- apply(cps_Y,2,function(x){(x - mean(x))/sd(x)})



summary(cps_Y)
summary(cps_stand)
summary(cps_data)

max <- 7 # this is the maximum number of clusters you think would be useful
wss <- (nrow(cps_stand)-1)*sum(apply(cps_stand,2,var)) # what if there was just 1 cluster
for (i in 2:max){
  wss[i] <- sum(kmeans(cps_stand,centers = i)$withinss)
  
}

# elbow plot
set.seed(123123)
ggplot()+
  geom_line(aes(x = 1:max,wss)) +
  geom_point(aes(x = 1:max,wss)) +
  scale_x_continuous(breaks = c(1:max))

# 2 and 3 and 4 kind of look like the "elbow: region of this plot
# it will be worth looking into all of these potentially

cps_kmeans2 <- kmeans(cps_stand,centers = 2)
cps_kmeans3 <- kmeans(cps_stand,centers = 3)
cps_kmeans4 <- kmeans(cps_stand,centers = 4)

# Create copies for all of these options

cps_Y2 <- cps_Y
cps_Y3 <- cps_Y
cps_Y4 <- cps_Y
cps_Y5 <- cps_Y

# Add clusters to respective df's
cps_Y2$km_cluster2 <- as.factor(cps_kmeans2$cluster)
cps_Y3$km_cluster3 <- as.factor(cps_kmeans3$cluster)
cps_Y4$km_cluster4 <- as.factor(cps_kmeans4$cluster)

#Check
head(cps_Y2)
summary(cps_Y2)

head(cps_Y3)
summary(cps_Y3)

head(cps_Y4)
summary(cps_Y4)

# convert to long form for visualization purposes
cps_long2 <- melt(cps_Y2, id.vars = c("km_cluster2"))
cps_long3 <- melt(cps_Y3, id.vars = c("km_cluster3"))
cps_long4 <- melt(cps_Y4, id.vars = c("km_cluster4"))

ggplot(data = cps_long2) +
  geom_boxplot(aes(x = km_cluster2,y = value,fill = km_cluster2)) + 
  facet_wrap(~variable,scales = "free") + 
  scale_fill_brewer(palette = "Dark2")

ggplot(data = cps_long3) +
  geom_boxplot(aes(x = km_cluster3,y = value,fill = km_cluster3)) + 
  facet_wrap(~variable,scales = "free") + 
  scale_fill_brewer(palette = "Dark2")

ggplot(data = cps_long4) +
  geom_boxplot(aes(x = km_cluster4,y = value,fill = km_cluster4)) + 
  facet_wrap(~variable,scales = "free") + 
  scale_fill_brewer(palette = "Dark2")

# We obviously have issues here....
# cannot perform this type of clustering on Binary Ys



#### hierarchical clustering ####

# hierarchical clustering with euclidean distance
cps_dist<-dist(cps_stand,method="euclidean")
cps_clust1 <- hclust(cps_dist, method = "average")
cps_clust2 <- hclust(cps_dist, method = "ward.D") 
# visualize 
plot(cps_clust1, labels = cps_Y$FSFOODS) 
rect.hclust(cps_clust1, k=2, border="red")

plot(cps_clust2, labels = cps_Y$FSFOODS) 
rect.hclust(cps_clust2, k=2, border="red")

# not good, many issues
# still not suited for Binary

# try with a different type of distance metric,
# one that can handle Binary, Gower Distance

# Gower needs factors as input
cps_Y[, c(1, 2, 3)] <- lapply(cps_Y[, c(1, 2, 3)], as.factor)

# Compute Gower distance
gower_dist <- daisy(cps_Y, metric = "gower")

# Perform hierarchical clustering
cps_clust3 <- hclust(gower_dist, method = "average")

# Plot the dendrogram
plot(cps_clust3)

# Cut the dendrogram into 4 clusters
cluster_assignments <- cutree(cps_clust3, k = 4)

# Add cluster assignments to cps_Y4 as a factor column
cps_Y5$cps_clust3 <- as.factor(cluster_assignments)


head(cps_Y5)




# visualize 
cps_Y5 %>% 
  melt(id.var ="cps_clust3") %>%
  ggplot() +
  geom_bar(aes(x = cps_clust3, fill = as.factor(value)), position = "fill") +
  facet_wrap(~variable, labeller = labeller(variable = c(FSSTATUSMD = "30 Day Food Insecurity",
                                                         FSFOODS = "Unhappy with Food Available",
                                                         FSSTATUS = "Year Food Insecurity"))) +  # Rename facets +
  labs(x = "Clusters", y = "Proportion", fill = "Presence") +
  scale_fill_brewer(palette = "Set2") +  # Set2 is color-blind-friendly
  labs(color = "Cluster Group") +  # Rename the legend
  theme_minimal() +
  theme(legend.title = element_text(size = 12),  # Adjust legend title size
        legend.text = element_text(size = 10))    # Adjust legend text size



summary(cps_Y5)

# Cluster 3 will be the cluster of interest,
# high FSSTATUSMD and FSFOODS and not perfect FSSTATUS. 
# We have 1537 observations in this group


#Cluster 3 has high food insecurity
#We want to predict Cluster 3
#Cluster 1: Food Secure
#Cluster 2: Food secure, but not the types of food they want
#Cluster 3: Food insecure
#Cluster 4: Has foods they want, but has been less secure in the last month.


##### Model Prep #####

# This much data and X variables will take a very long time to fit.
# Let's use some other methods to find the most important Y variables

head(cps_Y5)

# Create and add our Binary Y variable
# call this FoodInsecure, this our parameter of interest
cps_Y5$FoodInsecure = ifelse(cps_Y5$cps_clust3 == 3, 1, 0)

cps_Y6 <- select(cps_Y5,-c(cps_clust3))
summary(cps_Y6)

cps_data2 <- cps_data
cps_data2$FoodInsecure <- as.factor(cps_Y6$FoodInsecure)

# drop the Ys we clustered and FSRAWSCRA (it was cleaned into a different Y)
# drop the Ys not in ACS data
cps_data2 <- subset(cps_data2,select = -c(FSSTATUSMD,FSFOODS,FSSTATUS,FSRAWSCRA,
                                          CPSID,COUNTY,FSBAL,FSWROUTY,FSTOTXPNC_perpers,
                                          FSTOTXPNC))
summary(cps_data2)

# create a random forest to tell us our most important X variables
RNGkind(sample.kind = "default")
set.seed(123123)
train.idx = sample(x = 1:nrow(cps_data2), size = .8*nrow(cps_data2))
train.df = cps_data2[train.idx,]
test.df = cps_data2[-train.idx,]

# fit a baseline forest - not really necessaty but helps us estimate 
# how long it will take to tune

tempforest <- randomForest(FoodInsecure ~ hhsize + female + hispanic + black + 
                             kids + elderly + education + FamInc + married,
                           data = train.df,
                           ntree = 1000,
                           mtry = 4,
                           weights = train.df$weight
                           )


dim(train.df)
mtry <- seq(from = 1, to =11, by = 2)

keeps <- data.frame(m = rep(NA, length(mtry)),
                    OOB_err_rate = rep(NA, length(mtry)))

for (idx in 1:length(mtry)){
  print(paste0("Trying m = ", mtry[idx]))
  tempforest <- randomForest(FoodInsecure ~  hhsize + female + hispanic + black + 
                               kids + elderly + education + FamInc + married,
                             data = train.df,
                             ntree = 1000,
                             mtry = mtry[idx],
                             weights = train.df$weight)
  # record iteriation's m value in idx'th row
  keeps[idx, "m"] <- mtry[idx]
  # record oob error in idx'th row
  keeps[idx,"OOB_err_rate"] <- mean(predict(tempforest) != train.df$FoodInsecure)
}

keeps

# M 5 gives us the lowest error rate

# ploting error rates
ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_err_rate)) + 
  theme_bw() + labs(x = "m (mtry) value", y = "OOB Error rate (minimize)")+
  scale_x_continuous(breaks = c(1:10))

finalforest <- randomForest(FoodInsecure  ~ hhsize + female + hispanic + black + 
                              kids + elderly + education + FamInc + married,
                            data = train.df,
                            ntree = 1000,
                            mtry = 5,
                            importance = TRUE,
                            weights = train.df$weight)

# Validate model as a predictive tool

pi_hat <- predict(finalforest, test.df,type = "prob")[,1] # choose positive event

rocCurve <- roc(response = test.df$FoodInsecure,
                predictor = pi_hat,
                levels = c(0 ,1)) # MAKE SURE THIS LINES UP WITH YOUB CHOSEN POSITIVE EVENT

plot(rocCurve,print.auc = TRUE, print.thres=TRUE)

# save probabilities

pi_star = coords(rocCurve, "best", ret = "threshold")$threshold[1]



# that's it for prediction




# Define custom labels for the variables
renamed_labels <- c(
  FamInc = "Family Income",
  hhsize = "Household Size",
  hispanic = "Hispanic",
  married = "Married",
  education = "Education",
  kids = "Kids",
  black = "Black",
  female = "Gender",
  elderly = "Elderly"
)

# Plot variable importance with customizations
varImpPlot(finalforest, 
           type = 1,# Plot mean decrease in accuracy
           names.arg = renamed_labels, 
           main = "Variable Importance",  # Change title to "Variable Importance"
           col = "steelblue",            # Set bar color
           pch = 16,                     # Set point character
           cex = 1.5,                    # Adjust point size
           cex.main = 1.5,                # Title size
           cex.lab = 1.2,                 # Axis labels size
           lwd = 2,                      # Line width
           horiz = TRUE)                 # Horizontal bars for better readability

# This is good but we can make better

vi = as.data.frame(varImpPlot(finalforest,type = 1))
vi$variable <- rownames(vi)


# Basic Bar chart
ggplot(data = vi) + 
  geom_bar(aes(x = reorder(variable, MeanDecreaseAccuracy),
               weight = MeanDecreaseAccuracy) , position = "identity")+
  coord_flip()


# much better
ggplot(data = vi, aes(x = reorder(variable, MeanDecreaseAccuracy), 
                      y = MeanDecreaseAccuracy)) + 
  geom_bar(stat = "identity", fill = "steelblue") + # Use "steelblue" for bar color
  coord_flip() +  # Flip axes for horizontal bars
  labs(
    title = "Variable Importance", # Add a title
    x = "Variable", # X-axis label
    y = "Mean Decrease in Accuracy" # Y-axis label
  ) +
  theme_minimal(base_size = 14) +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5), # Center align the title
    axis.text = element_text(size = 12), # Adjust axis text size
    axis.title = element_text(size = 14) # Adjust axis title size
  )


# add a couple more touch ups
ggplot(data = vi) + 
  geom_bar(
    aes(
      x = reorder(
        recode(variable, !!!renamed_labels),
        MeanDecreaseAccuracy
      ),
      weight = MeanDecreaseAccuracy
    ), 
    position = "identity",
    fill = "steelblue"
  ) +
  coord_flip() +
  labs(
    x = "Variable", 
    y = "Mean Decrease Accuracy", 
    title = "Variable Importance"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )
# These plots give us the order of the most influential variables in predicting 
# our Y variable of "Food Insecure"




####MODELING#####


#Change the dataframes to matrices to conform with ridge and lasso
x.train = model.matrix(FoodInsecure ~ hhsize + female + hispanic + black + 
                          kids + elderly + education + FamInc + married, data = train.df)[,-1]
x.test = model.matrix(FoodInsecure ~ hhsize + female + hispanic + black + 
                         kids + elderly + education + FamInc + married , data = test.df)[,-1]

y.train = as.vector(train.df$FoodInsecure)
y.test = as.vector(test.df$FoodInsecure)

y.train <- as.factor(y.train)
y.test <- as.factor(y.test)

#Cross validation
# Ensure weights is numeric (if it's a column in your data)
#weights <- as.numeric(train.df$weight)  # Convert weight to numeric if needed

# Cross-validation with lasso
lr_lasso_cv = cv.glmnet(x.train,  # train x MATRIX - without y column
                        y.train,  # train y VECTOR - your y column
                        family = binomial(link = "logit"),
                        alpha = 1,
                        weights = train.df$weight  # Pass weights correctly here
)


lr_ridge_cv = cv.glmnet(x.train, #train x MATRIX - without y column
                        y.train,  #trian y VECTOR - your y column
                        family = binomial(link = "logit"),
                        alpha = 0,
                        weights = train.df$weight  # Pass weights correctly here
                        )

#Plotting
plot(lr_lasso_cv)
plot(lr_ridge_cv)

#To see coefficients
lr_lasso_coefs = coef(lr_lasso_cv, s = "lambda.min")
lr_ridge_coefs = coef(lr_ridge_cv, s = "lambda.min")

#We want the plots that minimize out of sample error
#We chose where lambda is smallest
best_lasso_lambda = lr_lasso_cv$lambda.min
best_ridge_lambda = lr_ridge_cv$lambda.min

final_lasso = glmnet(x.train, y.train,
                     family = binomial(link = "logit"),
                     alpha = 1,
                     lambda = best_lasso_lambda,
                     weights = train.df$weight)

final_ridge = glmnet(x.train, y.train,
                     family = binomial(link = "logit"),
                     alpha = 0,
                     lambda = best_ridge_lambda,
                     weights = train.df$weight)


test.df.preds = test.df %>% 
  mutate(
    lasso_pred = predict(final_lasso, x.test, type = "response")[,1],
    ridge_pred = predict(final_ridge, x.test, type = "response")[,1])


lasso_rocCurve = roc(response = as.factor(test.df.preds$FoodInsecure),
                     predictor = test.df.preds$lasso_pred,
                     levels = c("0", "1"))
ridge_rocCurve = roc(response = as.factor(test.df.preds$FoodInsecure),
                     predictor = test.df.preds$ridge_pred,
                     levels = c("0", "1"))


#make data frame of lasso ROC info
lasso_data <- data.frame(
  Model = "Lasso",
  Specificity = lasso_rocCurve$specificities,
  Sensitivity = lasso_rocCurve$sensitivities,
  AUC = lasso_rocCurve$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data <- data.frame(
  Model = "Ridge",
  Specificity = ridge_rocCurve$specificities,
  Sensitivity = ridge_rocCurve$sensitivities,
  AUC = ridge_rocCurve$auc%>% as.numeric
)    

# Combine all the data frames
roc_data_Y6 <- rbind( lasso_data, ridge_data)

ggplot() +
  geom_line(aes(x = 1 - Specificity, y = Sensitivity, color = Model), data = roc_data_Y6) +
  geom_text(
    data = roc_data_Y6 %>% group_by(Model) %>% slice(1),
    aes(x = 0.75, y = c(0.75, 0.65), color = Model, 
        label = paste0(Model, " AUC = ", AUC)),
    size = 5,
    hjust = 0
  ) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

# Both AUC's are 0.75
# Let's go back and reference our forest  and remake the model
# with only the most impactful x variables

##### Model 2 #####


x.train2 = model.matrix(FoodInsecure ~ hhsize + hispanic + 
                       education + FamInc + married, data = train.df)[,-1]
x.test2 = model.matrix(FoodInsecure ~ hhsize + hispanic + 
                        education + FamInc + married, data = test.df)[,-1]

y.train2 = as.vector(train.df$FoodInsecure)
y.test2 = as.vector(test.df$FoodInsecure)

y.train2 <- as.factor(y.train2)
y.test2 <- as.factor(y.test2)

#Cross validation
# Ensure weights is numeric (if it's a column in your data)
#weights <- as.numeric(train.df$weight)  # Convert weight to numeric if needed

# Cross-validation with lasso
lr_lasso_cv2 = cv.glmnet(x.train2,  # train x MATRIX - without y column
                        y.train2,  # train y VECTOR - your y column
                        family = binomial(link = "logit"),
                        alpha = 1,
                        weights = train.df$weight  # Pass weights correctly here
)


lr_ridge_cv2 = cv.glmnet(x.train2, #train x MATRIX - without y column
                        y.train2,  #trian y VECTOR - your y column
                        family = binomial(link = "logit"),
                        alpha = 0,
                        weights = train.df$weight  # Pass weights correctly here
)

#Plotting
plot(lr_lasso_cv2)
plot(lr_ridge_cv2)

#To see coefficients
lr_lasso_coefs2 = coef(lr_lasso_cv2, s = "lambda.min")
lr_ridge_coefs2 = coef(lr_ridge_cv2, s = "lambda.min")

#We want the plots that minimize out of sample error
#We chose where lambda is smallest
best_lasso_lambda2 = lr_lasso_cv2$lambda.min
best_ridge_lambda2 = lr_ridge_cv2$lambda.min

final_lasso2 = glmnet(x.train2, y.train2,
                     family = binomial(link = "logit"),
                     alpha = 1,
                     lambda = best_lasso_lambda2,
                     weights = train.df$weight)

final_ridge2 = glmnet(x.train2, y.train2,
                     family = binomial(link = "logit"),
                     alpha = 0,
                     lambda = best_ridge_lambda2,
                     weights = train.df$weight)


test.df.preds2 = test.df %>% 
  mutate(
    lasso_pred = predict(final_lasso2, x.test2, type = "response")[,1],
    ridge_pred = predict(final_ridge2, x.test2, type = "response")[,1])





lasso_rocCurve2 = roc(response = as.factor(test.df.preds2$FoodInsecure),
                     predictor = test.df.preds2$lasso_pred,
                     levels = c("0", "1"))
ridge_rocCurve2 = roc(response = as.factor(test.df.preds2$FoodInsecure),
                     predictor = test.df.preds2$ridge_pred,
                     levels = c("0", "1"))


#make data frame of lasso ROC info
lasso_data2 <- data.frame(
  Model = "Lasso",
  Specificity2 = lasso_rocCurve2$specificities,
  Sensitivity2 = lasso_rocCurve2$sensitivities,
  AUC = lasso_rocCurve2$auc %>% as.numeric
)
#make data frame of ridge ROC info
ridge_data2 <- data.frame(
  Model = "Ridge",
  Specificity2 = ridge_rocCurve2$specificities,
  Sensitivity2 = ridge_rocCurve2$sensitivities,
  AUC = ridge_rocCurve2$auc%>% as.numeric
)    

# Combine all the data frames
roc_data_Y2 <- rbind( lasso_data2, ridge_data2)

ggplot() +
  geom_line(aes(x = 1 - Specificity2, y = Sensitivity2, color = Model), data = roc_data_Y2) +
  geom_text(
    data = roc_data_Y2 %>% group_by(Model) %>% slice(1),
    aes(x = 0.75, y = c(0.75, 0.65), color = Model, 
        label = paste0(Model, " AUC = ", AUC)),
    size = 5,
    hjust = 0
  ) +
  scale_colour_brewer(palette = "Paired") +
  labs(x = "1 - Specificity", y = "Sensitivity", color = "Model") +
  theme_minimal()

# Pull threshold 

lasso_optimal_coords <- coords(lasso_rocCurve2, "best", ret = "threshold")
lasso_optimal_threshold <- lasso_optimal_coords$threshold 


# The AUC on these ridge and lasso models are very similair to the AUC
# from the full models earlier. This means our random forest was right 
# in predicting the 4 least impactful variables

##### Predict on ACS ##### 



# the final lasso is a bit better
summary(acs_data)

acs.mat = model.matrix( ~ hhsize + hispanic + education + FamInc + married,
                       data = acs_data)[,-1]


acs_data$lasso_pred <- predict(final_lasso2, acs.mat, type = 'response')[, 1]

head(acs_data$lasso_pred)

# compare threshold to predictions
acs_data$lasso_class <- ifelse(acs_data$lasso_pred > lasso_optimal_threshold, 1, 0)
summary(acs_data$lasso_class)
table(acs_data$lasso_class)

##### Interpretations ##### 

final_lasso_coef <- coef(final_lasso2, s = best_lasso_lambda2)
final_lasso_coef

# education   -2.937595e-01
# for every increase in education FoodInseucy probability goes down by 29%

# married     -3.160387e-01
# being married decreases being food insecure by 31%

# 


##### Visualizations #####

iowa_pumas <- pumas(state = "IA", cb = TRUE, year = 2020)
# Clorapleth map of iowa using PUMA

seniors_puma <- read.csv("data/total_iowa_seniors_by_puma.csv", stringsAsFactors = TRUE)

# download PUMA shapefile for Iowa



seniors_puma <- seniors_puma %>% rename(GEOID20 = GEOID)
iowa_pumas$GEOID20 <- as.character(iowa_pumas$GEOID20)
seniors_puma$GEOID20 <- as.character(seniors_puma$GEOID20)

# Merge the shapefile with senior population data
iowa_pumas <- left_join(iowa_pumas, seniors_puma, by = "GEOID20")



#FoodInsecure Pumas
acs_data$GEOID20 = acs_data$PUMA
joined = left_join(iowa_pumas, acs_data, "GEOID20")
acs_data$lasso_class = as.numeric(acs_data$lasso_class)
# create the choropleth map 


#Filter and Summarize Data
# Calculate the total number of observations where lasso_class == 1 for each PUMA
joined_summary <- joined %>%
  filter(lasso_class == 1) %>%  # Keep rows where lasso_class == 1
  group_by(PUMA, geometry, NAMELSAD20) %>%  # Group by PUMA and geometry
  summarize(total_obs = n(), .groups = "drop")  # Count observations per PUMA

#Create the Map
ggplot(data = joined_summary) +
  geom_sf(aes(fill = total_obs), color = "black", size = 0.2) +
  scale_fill_distiller(
    name = "Total Number of \nFood Insecure Families", 
    palette = "YlGnBu",  # Use a colorblind-friendly palette
    direction = 1        # Ensure the scale goes from low to high
  ) +
  labs(
    title = "Food Insecure Families by PUMA"
  ) +
  theme_minimal()

# Find the three PUMAs with the most food-insecure families
top_pumas <- joined_summary %>%
  arrange(desc(total_obs)) %>%
  slice(1:3)  # Select top 3 PUMAs

ggplot(data = joined_summary) +
  geom_sf(aes(fill = total_obs), color = "black", size = 0.2) +  # Fill by food-insecure seniors
  scale_fill_distiller(
    name = "Food-Insecure Family",
    palette = "YlGnBu",  # colorblind palette
    direction = 1) +
  geom_sf_text(
    data = top_pumas,
    aes(label = round(total_obs)),
    color = "white",
    size = 3,  # Adjust text size
    fontface = "bold"  # Bold text for emphasis
  ) +
  labs(
    title = "Food-Insecure Seniors by PUMA in Iowa",
    subtitle = "Top 3 PUMAs highlighted with the number of food-insecure seniors",
    caption = "Source: Your Data & US Census Bureau"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to the bottom of plot

# Add a column to indicate top 3 PUMAs
joined_summary <- joined_summary %>%
  mutate(is_top_puma = if_else(PUMA %in% top_pumas$PUMA, "Top 3", "Other"))


# Find the three PUMAs with the most food-insecure families
top_pumas <- joined_summary %>%
  arrange(desc(total_obs)) %>%
  slice(1:3)  # Select top 3 PUMAs


ggplot(data = joined_summary) +
  # Base layer with transparency for non-top PUMAs
  geom_sf(aes( alpha = is_top_puma), color = "black", size = 0.2) +
  scale_fill_distiller(
    name = "Food-Insecure Families",
    palette = "YlGnBu",
    direction = 1
  ) +
  scale_alpha_manual(
    values = c("Top 3" = 1, "Other" = 0.3),  # Top PUMAs fully opaque, others semi-transparent
    guide = "none"
  ) +
  # Red outline for top 3 PUMAs
  geom_sf(
    data = top_pumas,
    color = "red",
    size = 0.5,
    fill = NA  # No fill, just outline
  ) +
  # Labels for top 3 PUMAs
  geom_sf_text(
    data = top_pumas,
    aes(label = NAMELSAD20),
    color = "red",
    size = 3,
    fontface = "bold"
  ) +
  labs(
    title = "Food-Insecure Families by PUMA in Iowa",
    subtitle = "Top 3 PUMAs outlined in red and labeled",
    caption = "Source: Your Data & US Census Bureau",
    x = "Latitude",
    y = "Longitude"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")  # Move legend to bottom


