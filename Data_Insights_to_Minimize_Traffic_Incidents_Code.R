####### SJSU BUS 193 DATA MINING ################
#######Instructor: Prof. Shaonan tian ###########
#Group 15: Aariz Anjum, Ethan Ma, Michelle Le####
############### Final project ###################

#### How to load Data:
#### Download dataset from: https://www.kaggle.com/datasets/alexgude/california-traffic-collision-data-from-switrs
#### Extract All
#### Open Switrs Folder
#### Open switrs_raw_csvs
#### Import dataset from text (readr) : CollisionRecords.txt
#### Change read_csv path 

### Install and Load Required Libraries ###
library(readr)
library(randomForest)
library(dplyr)
library(MASS)
library(ggplot2)
library(rpart)
library(caret)


CollisionRecords <- read_csv("C:\Users\miche\Projects\Data_Insights_to_Minimize_Traffic_Incidents\Collision_Records.txt")
dim(CollisionRecords )


# Keep only the desired columns
CollisionRecords_new <- CollisionRecords[, c("DAY_OF_WEEK", "WEATHER_1", "ROAD_SURFACE", "ROAD_COND_1", "LIGHTING", 
                                             "INTERSECTION", "PEDESTRIAN_ACCIDENT", "BICYCLE_ACCIDENT", 
                                             "MOTORCYCLE_ACCIDENT", "TRUCK_ACCIDENT", 
                                             "ALCOHOL_INVOLVED", "COLLISION_SEVERITY" )]

## Make response variable binary

CollisionRecords_new$COLLISION_SEVERITY <- ifelse(CollisionRecords_new$COLLISION_SEVERITY == 0, 0, 
                                                  ifelse(CollisionRecords_new$COLLISION_SEVERITY == 1, 1,
                                                         ifelse(CollisionRecords_new$COLLISION_SEVERITY == 2, 1,
                                                                ifelse(CollisionRecords_new$COLLISION_SEVERITY == 3, 0,
                                                                       ifelse(CollisionRecords_new$COLLISION_SEVERITY == 4, 0, NA)))))
#Variable dictionary 
# 1 - Fatal injury
# 2 - Suspected serious injury or severe injury
# 3 - Suspected minor injury or visible injury
# 4 - Possible injury or complaint of pain
# 0 - No injury, also known as "property damage only" or PDO (PDO crashes not included on TIMS)

# Binary Variable
# 0: not severe
# 1: severe


CollisionRecords_new$COLLISION_SEVERITY <- factor(CollisionRecords_new$COLLISION_SEVERITY)
levels(CollisionRecords_new$COLLISION_SEVERITY)


# Make binary variables binary
CollisionRecords_new <- CollisionRecords_new %>%
  mutate(across(c(BICYCLE_ACCIDENT, MOTORCYCLE_ACCIDENT, TRUCK_ACCIDENT, ALCOHOL_INVOLVED,PEDESTRIAN_ACCIDENT), 
                ~ ifelse(is.na(.), "N", .)))

# Factorize ALL chosen variables
CollisionRecords_new$DAY_OF_WEEK <- as.factor(CollisionRecords_new$DAY_OF_WEEK)
CollisionRecords_new$WEATHER_1 <- as.factor(CollisionRecords_new$WEATHER_1)
CollisionRecords_new$ROAD_SURFACE <- as.factor(CollisionRecords_new$ROAD_SURFACE)
CollisionRecords_new$LIGHTING <- as.factor(CollisionRecords_new$LIGHTING)
CollisionRecords_new$INTERSECTION <- as.factor(CollisionRecords_new$INTERSECTION)
CollisionRecords_new$PEDESTRIAN_ACCIDENT <- as.factor(CollisionRecords_new$PEDESTRIAN_ACCIDENT )
CollisionRecords_new$BICYCLE_ACCIDENT <- as.factor(CollisionRecords_new$BICYCLE_ACCIDENT)
CollisionRecords_new$MOTORCYCLE_ACCIDENT <- as.factor(CollisionRecords_new$MOTORCYCLE_ACCIDENT)
CollisionRecords_new$TRUCK_ACCIDENT <- as.factor(CollisionRecords_new$TRUCK_ACCIDENT)
CollisionRecords_new$ALCOHOL_INVOLVED <- as.factor(CollisionRecords_new$ALCOHOL_INVOLVED)
CollisionRecords_new$COLLISION_SEVERITY <- as.factor(CollisionRecords_new$COLLISION_SEVERITY)
CollisionRecords_new$ROAD_COND_1 <- as.factor(CollisionRecords_new$ROAD_COND_1)

#Check structure
str(CollisionRecords_new)


# Omit N/A's
CollisionRecords_new <- na.omit(CollisionRecords_new)

#check N/A's
colSums(is.na(CollisionRecords_new))

dim(CollisionRecords_new)


#set.seed(193)
#CollisionRecords_new = CollisionRecords_new[sample(nrow(CollisionRecords_new), nrow(CollisionRecords_new) * 0.10), ]
#dim(CollisionRecords_new)

# Separate the data into two groups
data_0 <- CollisionRecords_new %>% filter(COLLISION_SEVERITY == 0)
data_1 <- CollisionRecords_new %>% filter(COLLISION_SEVERITY == 1)


# Get the proportion of each class in the original dataset
total_rows <- nrow(CollisionRecords_new)
prop_0 <- nrow(data_0) / total_rows
prop_1 <- nrow(data_1) / total_rows
print(prop_0)
print(prop_1)


# Randomly sample 90% of the 0's and 90% of the 1's
set.seed(193)
sampled_data_0 <- data_0 %>% sample_frac(0.9)
sampled_data_1 <- data_1 %>% sample_frac(0.9)

# Combine the sampled data back into a single training dataset
collision_train <- bind_rows(sampled_data_0, sampled_data_1)

# Create the test dataset by taking the remaining 10% of each class
collision_test <- bind_rows(
  data_0 %>% anti_join(sampled_data_0, by = names(data_0)),
  data_1 %>% anti_join(sampled_data_1, by = names(data_1))
)


dim(collision_train)
dim(collision_test)


# Get the counts of each level
collision_severity_counts <- table(collision_train$COLLISION_SEVERITY)

# Calculate the proportions
collision_severity_proportions <- collision_severity_counts / sum(collision_severity_counts)

print(collision_severity_proportions)

# create pie chart
collision_severity_counts <- table(collision_train$COLLISION_SEVERITY)
pie(collision_severity_counts, main = "Distribution of Collision Severity", 
    col = c("lightblue", "red"),
    labels = collision_severity_counts)
legend("bottomleft", 
       legend = c("Blue - Nonfatal Accidents (96.7%)", "Red - Fatal Accidents (3.3%)"), col = "lightblue", "red")

crash_frequency <- table(collision_train$DAY_OF_WEEK)

# create the line graph
plot(
  as.numeric(names(crash_frequency)),
  crash_frequency,
  type = "l",
  xlab = "Day of the Week",
  ylab = "", # <- empty to remove y axis labels!
  main = "Frequency of Crashes per Day of the Week",
  xaxt = "n",
)

# set x axis labels
axis(1, at = 1:7, labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
axis(2, at = pretty(crash_frequency), las = 1)
print(crash_frequency)


## Model 1: logistic regression ------------------------------------------------

# Model Fitting
collision_glm <- glm(COLLISION_SEVERITY ~ DAY_OF_WEEK + WEATHER_1 + ROAD_SURFACE + LIGHTING + 
                       INTERSECTION + PEDESTRIAN_ACCIDENT + BICYCLE_ACCIDENT + MOTORCYCLE_ACCIDENT + 
                       TRUCK_ACCIDENT + ALCOHOL_INVOLVED, 
                     data = collision_train, family = binomial)


summary(collision_glm)


# Perform stepwise selection
stepwise_model <- stepAIC(collision_glm, direction = "both")
summary(stepwise_model)

##---------------------------------------------- In Sample Metrics

# predicted probability In-sample 
collision_pred_resp <- predict(stepwise_model, newdata = collision_train, type="response")

# Misclassification Matrix In-sample
glm_mmatrix_train <- table(collision_train$COLLISION_SEVERITY, as.numeric(collision_pred_resp>0.5), dnn=c("Truth","Predicted"))
print(glm_mmatrix_train)

# Misclassification Rate In-sample###

# Calculate the total number of misclassified instances 
misclassified_1A <- sum(glm_mmatrix_train) - sum(diag(glm_mmatrix_train))  # Total minus correctly classified
# Calculate the total number of instances
total_1A <- sum(glm_mmatrix_train)
# Calculate the misclassification rate
misclassification_rate_glm_1A <- misclassified_1A / total_1A

# Print the result
cat("In-sample Misclassification Rate: ", misclassification_rate_glm_1A, "\n")
#Misclassification Rate:  0.01086667 

## Cost function: cost: 2x more for false positives)
cost_asymmetric <- function(r, pi) {
  cost <- ifelse(r == 0, (1 - pi), 2 * pi)  # false positives have higher cost
  return(mean(cost))
}
## In-sample ROC Curve & AUC ---------------------------------------------------

asym_cost <- cost_asymmetric(collision_train$COLLISION_SEVERITY, collision_pred_resp)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# Misclassification Cost with Asymmetric Cost:  0.9839961 


# Step 2: Create a prediction object
# Replace 'response' with the actual response variable in your test dataset
glm_pred <- prediction(collision_pred_resp, collision_train$COLLISION_SEVERITY)

# Step 3: Create a performance object for TPR and FPR
glm_perf <- performance(glm_pred, "tpr", "fpr")

# Step 4: Plot the ROC curve
plot(glm_perf, colorize = TRUE, main = "ROC Curve for Optimal GLM Model - In-sample")
# Add the AUC value to the plot (calculated below)
mtext(text = paste("AUC =", round(unlist(slot(performance(glm_pred, "auc"), "y.values")), 4)), 
      col = "blue", font = 3, cex = 1)

# Step 5: Calculate and display AUC
glm_auc_value_train <- unlist(slot(performance(glm_pred, "auc"), "y.values"))
print(paste("AUC:", glm_auc_value_train))



## Out of sample Metrics -------------------------------------------------------


# predicted probability Out-of-sample 
test_predictions_prob <- predict(stepwise_model, newdata = collision_test, type = "response")

# Misclassification Matrix Out of sample
glm_mmatrix_test <- table(collision_test$COLLISION_SEVERITY, (test_predictions_prob > 0.5)*1, dnn = c("Truth", "Predicted"))
print(glm_mmatrix_test )

# Calculate the total number of misclassified instances 
misclassified_1B <- sum(glm_mmatrix_test) - sum(diag(glm_mmatrix_test))  # Total minus correctly classified

# Calculate the total number of instances
total_1B <- sum(glm_mmatrix_test)

# Calculate the misclassification rate
misclassification_rate_glm_1B <- misclassified_1B / total_1B

# Print the result
cat("Out-of-sample Misclassification Rate: ", misclassification_rate_glm_1B, "\n")
#Misclassification Rate:  0.01086667 


## Cost function: misclassification cost with asymmetric cost (assuming asymmetric cost: 2x more for false positives)
cost_asymmetric <- function(r, pi) {
  cost <- ifelse(r == 0, (1 - pi), 2 * pi)  # false positives have higher cost
  return(mean(cost))
}

## Out of Sample ROC Curve & AUC ----------------------------------------------

# Use the predicted probabilities directly
asym_cost <- cost_asymmetric(collision_test$COLLISION_SEVERITY, test_predictions_prob)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# Misclassification Cost with Asymmetric Cost:  0.9839961 


# Step 2: Create a prediction object
# Replace 'response' with the actual response variable in your test dataset
glm_pred <- prediction(test_predictions_prob, collision_test$COLLISION_SEVERITY)

# Step 3: Create a performance object for TPR and FPR
glm_perf <- performance(glm_pred, "tpr", "fpr")

# Step 4: Plot the ROC curve
plot(glm_perf, colorize = TRUE, main = "ROC Curve for Optimal GLM Model - Out-of-sample")
# Add the AUC value to the plot (calculated below)
mtext(text = paste("AUC =", round(unlist(slot(performance(glm_pred, "auc"), "y.values")), 4)), 
      col = "blue", font = 3, cex = 1)

# Step 5: Calculate and display AUC
glm_auc_value_test <- unlist(slot(performance(glm_pred, "auc"), "y.values"))
print(paste("AUC:", glm_auc_value_test))

# ---------------------------------------- Model 2: Classification Tree

# fit Classification tree (CART) on training data
cart_model <- rpart(COLLISION_SEVERITY ~ DAY_OF_WEEK + WEATHER_1 + ROAD_SURFACE + +ROAD_COND_1 + LIGHTING + 
                      INTERSECTION + PEDESTRIAN_ACCIDENT + BICYCLE_ACCIDENT + MOTORCYCLE_ACCIDENT + 
                      TRUCK_ACCIDENT + ALCOHOL_INVOLVED, 
                    data = collision_train, method = "class",
                    control = rpart.control(cp = 0.0001, , maxdepth = 5)) 

plot(cart_model, margin=0.02, uniform=TRUE,main = "Classification Tree")
text(cart_model, cex = 0.5)

##---------------------------------------------- In Sample Metrics

# predicted probability In-sample 
cart_pred_prob <- predict(cart_model, newdata = collision_train, type = "prob")

# Misclassification Matrix In-sample
cart_mmatrix_train <- table(collision_train$COLLISION_SEVERITY, as.numeric(cart_pred_prob>0.5), dnn=c("Truth","Predicted"))
print(cart_mmatrix_train)

# Misclassification Rate In-sample###

# Calculate the total number of misclassified instances 
misclassified_2A <- sum(cart_mmatrix_train) - sum(diag(cart_mmatrix_train))  # Total minus correctly classified
# Calculate the total number of instances
cart_total_2A <- sum(cart_mmatrix_train)
# Calculate the misclassification rate
misclassification_rate_cart_2A <- misclassified_2A / cart_total_2A

# Print the result
cat("In-sample Misclassification Rate: ", misclassification_rate_cart_2A, "\n")
#Misclassification Rate:  0.01086667 


## In-sample ROC Curve & AUC ---------------------------------------------------

asym_cost <- cost_asymmetric(collision_train$COLLISION_SEVERITY, cart_pred_prob)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# Misclassification Cost with Asymmetric Cost:  0.9839961 


# Step 2: Create a prediction object
# Replace 'response' with the actual response variable in your test dataset
cart_pred <- prediction(cart_pred_prob[, 2], as.numeric(collision_train$COLLISION_SEVERITY))

# Step 3: Create a performance object for TPR and FPR
cart_perf <- performance(cart_pred, "tpr", "fpr")

# Step 4: Plot the ROC curve
plot(cart_perf, colorize = TRUE, main = "ROC Curve for Optimal CART Model - In-sample")
# Add the AUC value to the plot (calculated below)
mtext(text = paste("AUC =", round(unlist(slot(performance(cart_pred, "auc"), "y.values")), 4)), 
      col = "blue", font = 3, cex = 1)

# Step 5: Calculate and display AUC
cart_auc_value_train <- unlist(slot(performance(cart_pred, "auc"), "y.values"))
print(paste("AUC:", cart_auc_value_train))


## Out of sample Metrics -------------------------------------------------------


# predicted probability Out-of-sample 
cart_pred_prob <- predict(cart_model, newdata = collision_test, type = "prob")[, 2] 


# Misclassification Matrix Out of sample
cart_mmatrix_test <- table(collision_test$COLLISION_SEVERITY, (cart_pred_prob > 0.5)*1, dnn = c("Truth", "Predicted"))
print(cart_mmatrix_test )

# Calculate the total number of misclassified instances 
misclassified_2B <- sum(cart_mmatrix_test) - sum(diag(cart_mmatrix_test))  # Total minus correctly classified

# Calculate the total number of instances
cart_total_2B <- sum(cart_mmatrix_test)

# Calculate the misclassification rate
misclassification_rate_cart_2B <- misclassified_2B / cart_total_2B

# Print the result
cat("Out-of-sample Misclassification Rate: ", misclassification_rate_cart_2B, "\n")
#Misclassification Rate:  0.01086667 


## Out of Sample ROC Curve & AUC ----------------------------------------------

# Use the predicted probabilities directly
asym_cost <- cost_asymmetric(collision_test$COLLISION_SEVERITY, cart_pred_prob)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# Misclassification Cost with Asymmetric Cost:  0.9839961 


# Step 2: Create a prediction object
# Replace 'response' with the actual response variable in your test dataset
cart_pred <- prediction(cart_pred_prob, collision_test$COLLISION_SEVERITY)

# Step 3: Create a performance object for TPR and FPR
cart_perf <- performance(cart_pred, "tpr", "fpr")

# Step 4: Plot the ROC curve
plot(cart_perf, colorize = TRUE, main = "ROC Curve for Optimal cart Model - Out-of-sample")
# Add the AUC value to the plot (calculated below)
mtext(text = paste("AUC =", round(unlist(slot(performance(cart_pred, "auc"), "y.values")), 4)), 
      col = "blue", font = 3, cex = 1)

# Step 5: Calculate and display AUC
cart_auc_value_train <- unlist(slot(performance(cart_pred, "auc"), "y.values"))
print(paste("AUC:", cart_auc_value_train))

#---------------------------------------------------- Model 3
rf_model <- randomForest(COLLISION_SEVERITY ~ DAY_OF_WEEK + WEATHER_1 + ROAD_SURFACE + ROAD_COND_1 + LIGHTING + 
                           INTERSECTION + PEDESTRIAN_ACCIDENT + BICYCLE_ACCIDENT + MOTORCYCLE_ACCIDENT + 
                           TRUCK_ACCIDENT + ALCOHOL_INVOLVED, 
                         data = collision_train, 
                         ntree = 500, 
                         mtry = sqrt(ncol(collision_train)), 
                         importance = TRUE)

# View the model output
print(rf_model)




importance(rf_model)
varImpPlot(rf_model)

##---------------------------------------------- In Sample Metrics

# predicted probability In-sample 
rf_pred_prob <- predict(rf_model, newdata = collision_train, type = "prob")[, 2]  # Get probabilities for class 1

#  In-sample Misclassification Cost with Asymmetric Cost
asym_cost <- cost_asymmetric(collision_test$COLLISION_SEVERITY, rf_pred_prob)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# In-sample Misclassification Cost with Asymmetric Cost:  0.9839961 


# Misclassification Matrix In-sample
rf_mmatrix_train <- table(collision_train$COLLISION_SEVERITY, as.numeric(rf_pred_prob>0.5), dnn=c("Trut
h","Predicted"))
print(glm_mmatrix_train)


# Misclassification Rate In-sample###

# Calculate the total number of misclassified instances 
misclassified_2A <- sum(rf_mmatrix_train) - sum(diag(rf_mmatrix_train))  # Total minus correctly classified
# Calculate the total number of instances
rf_total_2A <- sum(rf_mmatrix_train)
# Calculate the misclassification rate
misclassification_rate_rf_2A <- misclassified_2A / rf_total_2A

# Print the result
cat("In-sample Misclassification Rate: ", misclassification_rate_rf_2A, "\n")
#Misclassification Rate:  0.01086667 


## In-sample ROC Curve & AUC ---------------------------------------------------

asym_cost <- cost_asymmetric(collision_train$COLLISION_SEVERITY, rf_pred_prob)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# Misclassification Cost with Asymmetric Cost:  0.9839961 


# Step 2: Create a prediction object
# Replace 'response' with the actual response variable in your test dataset
rf_pred <- prediction(rf_pred_prob, collision_train$COLLISION_SEVERITY)

# Step 3: Create a performance object for TPR and FPR
rf_perf <- performance(rf_pred, "tpr", "fpr")

# Step 4: Plot the ROC curve
plot(rf_perf, colorize = TRUE, main = "ROC Curve for Optimal rf Model - In-sample")
# Add the AUC value to the plot (calculated below)
mtext(text = paste("AUC =", round(unlist(slot(performance(rf_pred, "auc"), "y.values")), 4)), 
      col = "blue", font = 3, cex = 1)

# Step 5: Calculate and display AUC
rf_auc_value_train <- unlist(slot(performance(rf_pred, "auc"), "y.values"))
print(paste("AUC:", rf_auc_value_train))


## Out of sample Metrics -------------------------------------------------------


# predicted probability Out-of-sample 
rf_pred_prob <- predict(rf_model, newdata = collision_test, type = "prob")[, 2] 


# Misclassification Matrix Out of sample
rf_mmatrix_test <- table(collision_test$COLLISION_SEVERITY, (rf_pred_prob > 0.5)*1, dnn = c("Truth", "Predicted"))
print(rf_mmatrix_test )

# Calculate the total number of misclassified instances 
misclassified_2B <- sum(rf_mmatrix_test) - sum(diag(rf_mmatrix_test))  # Total minus correctly classified

# Calculate the total number of instances
rf_total_2B <- sum(rf_mmatrix_test)

# Calculate the misclassification rate
misclassification_rate_rf_2B <- misclassified_2B / rf_total_2B

# Print the result
cat("Out-of-sample Misclassification Rate: ", misclassification_rate_rf_2B, "\n")
#Misclassification Rate:  0.01086667 


## Out of Sample ROC Curve & AUC ----------------------------------------------

# Use the predicted probabilities directly
asym_cost <- cost_asymmetric(collision_test$COLLISION_SEVERITY, rf_pred_prob)
cat("Out-of-sample Misclassification Cost with Asymmetric Cost: ", asym_cost, "\n")
# Misclassification Cost with Asymmetric Cost:  0.9839961 


# Step 2: Create a prediction object
# Replace 'response' with the actual response variable in your test dataset
rf_pred <- prediction(rf_pred_prob, collision_test$COLLISION_SEVERITY)

# Step 3: Create a performance object for TPR and FPR
rf_perf <- performance(rf_pred, "tpr", "fpr")

# Step 4: Plot the ROC curve
plot(rf_perf, colorize = TRUE, main = "ROC Curve for Optimal rf Model - Out-of-sample")
# Add the AUC value to the plot (calculated below)
mtext(text = paste("AUC =", round(unlist(slot(performance(rf_pred, "auc"), "y.values")), 4)), 
      col = "blue", font = 3, cex = 1)

# Step 5: Calculate and display AUC
rf_auc_value_test <- unlist(slot(performance(rf_pred, "auc"), "y.values"))
print(paste("AUC:", rf_auc_value_test))



