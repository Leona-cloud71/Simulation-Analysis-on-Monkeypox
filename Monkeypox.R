



# Load necessary libraries
library(tidyverse)    # For data manipulation and visualization (includes dplyr, ggplot2, etc.)
library(ggplot2)      # For creating plots (part of tidyverse)
library(warnings)     # For handling warnings
library(readr)
library(dplyr)
library(plotly)

library(RColorBrewer)  # For the Set1 color palette


# Suppress warnings
options(warn = -1)

# Set display options (if needed)
options(dplyr.width = Inf)
options(dplyr.print_max = Inf)



##### Read the data
Monkey_pox <- read_csv("C:/Users/Lib 003/Desktop/Kaggle Website Research/Monkey_pox.csv")

# Drop the "Patient_ID" column
Monkey_pox <- Monkey_pox %>% select(-Patient_ID)


Monkey_Pox <- factor(Monkey_pox$MonkeyPox, levels = c(0, 1), labels = c("Negative", "Positive"))

# Create a count plot for the "MonkeyPox" column

ggplot(Monkey_pox, aes(x = MonkeyPox, fill = MonkeyPox)) + 
  geom_bar() + 
  theme_minimal() +
  labs(x = "MonkeyPox", y = "Count", title = "Count of MonkeyPox Cases") +
  scale_fill_brewer(palette = "Set2", name = "MonkeyPox")

# Replace values in the data
Monkeypox <- Monkeypox %>% 
  mutate(across(everything(), ~replace(., . %in% c("Positive", TRUE), 1))) %>% 
  mutate(across(everything(), ~replace(., . %in% c("Negative", FALSE), 0)))

# Display the first few rows of the data
head(Monkeypox)


# Monkeypox is your data frame and 'MonkeyPox' and 'Systemic Illness' are columns

ggplot(Monkey_pox, aes(x = Systemic_Illness, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # position = "dodge" makes bars side by side
  scale_fill_brewer(palette = "Set1", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Systemic Illness Lead to MonkeyPox", x = "Systemic Illness", y = "Count") +
  theme_minimal()



# Count plot for 'Sore Throat' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Sore_Throat, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set1", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Sore Throat Leads to MonkeyPox", x = "Sore Throat", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )



# Count plot for 'rectal_pain' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Rectal_Pain, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set2", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Rectal pain by MonkeyPox Status", x = "Rectal Pain", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )


# Count plot for 'Penile Odema' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Penile_Oedema, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set4", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Penile Odema by MonkeyPox Status", x = "Penile Odema", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )



# Count plot for 'Oral Leisons' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Oral_Lesions, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set3", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Oral Leisons by MonkeyPox Status", x = "Oral Leisons", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )



# Count plot for 'Solitary leisons' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Solitary_Lesion, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set2", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Solitary Leisons by MonkeyPox Status", x = "Solitary Leisons", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )



# Count plot for 'Swollen Tonsils' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Swollen_Tonsils, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set1", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Swollen Tonsils by MonkeyPox Status", x = "Swollen tonsils", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )



# Count plot for 'Hiv Infection' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = HIV_Infection, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set3", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "HIV Infection by MonkeyPox Status", x = "HIV Infection", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )




# Count plot for 'Sexually Transmitted Infection' by 'MonkeyPox' status
ggplot(Monkey_pox, aes(x = Sexually_Transmitted_Infection, fill = factor(MonkeyPox))) +
  geom_bar(position = "dodge") +  # Bars for different groups side by side
  scale_fill_brewer(palette = "Set4", name = "MonkeyPox") +  # Apply the Set1 color palette
  labs(title = "Sexually Transmitted Infection by MonkeyPox Status", x = "Sexually Transmitted Infection", y = "Count") +
  theme_minimal() +
  geom_text(
    aes(label = ..count..), 
    stat = "count", 
    position = position_dodge(width = 0.9), 
    vjust = -0.5  # Adjust vertical position of text
  )



# Replace values in the data
Monkey_pox <- Monkey_pox %>% 
  mutate(across(everything(), ~replace(., . %in% c("Positive", TRUE), 1))) %>% 
  mutate(across(everything(), ~replace(., . %in% c("Negative", FALSE), 0)))

# Display the first few rows of the data
head(Monkey_pox)


##### Correlation Matrix

# Select the specified columns
selected_columns <- Monkey_pox %>% 
  select(Rectal_Pain, Sore_Throat,Penile_Oedema, Oral_Lesions, Solitary_Lesion, Swollen_Tonsils, HIV_Infection, Sexually_Transmitted_Infection, MonkeyPox)

# Ensure all columns are numeric
selected_columns <- selected_columns %>% mutate_all(as.numeric)

# Calculate the correlation matrix
correlation_matrix <- cor(selected_columns, use = "complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "color", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "purple", number.cex = 0.7, 
         title = "Correlation Matrix", mar=c(0,0,1,0))


# Create the lower triangular correlation matrix heatmap
ggcorrplot(correlation_matrix, method = "circle", type = "lower", lab = TRUE, lab_size = 3, colors = c("purple", "white", "orange"), title = "Correlation Heap Matrix", ggtheme = theme_minimal() )


##############  Model Development

install.packages("pROC")

# Load necessary libraries
library(caret)        # For data splitting and model evaluation
library(randomForest) # For Random Forest
library(pROC)         # For ROC and AUC
set.seed(42)  # For reproducibility
library(pROC)
library(reshape2)

# Drop the "Monkeypox" and "Systemic Illnes" columns from the data frame
X <- Monkey_pox %>% select(Rectal_Pain, Sore_Throat,Penile_Oedema, Oral_Lesions, Solitary_Lesion, Swollen_Tonsils, HIV_Infection, Sexually_Transmitted_Infection)

# Display the first few rows of the resulting data frame
head(X)

# Select the "Category" column from the data frame
y <- Monkey_pox$MonkeyPox

head(y)


train_data <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_data, ]
X_test <- X[-train_data, ]
y_train <- y[train_data]
y_test <- y[-train_data]


# Convert target variable to numeric if it's a factor
y_train <- as.numeric(as.character(y_train))
y_test <- as.numeric(as.character(y_test))



# Train the Logistic Regression model
lr_model <- glm(y_train ~ ., data = X_train, family = binomial)

# Predict on test data
y_pred <- predict(lr_model, newdata = X_test, type = "response")
y_pred_class <- ifelse(y_pred > 0.5, 1, 0)  # Convert probabilities to binary class

# Evaluate performance
confusion_matrix_lr <- confusionMatrix(as.factor(y_pred_class), as.factor(y_test))
print("Logistic Regression Confusion Matrix:")
print(confusion_matrix_lr)

roc_lr <- roc(y_test, y_pred)
print("Logistic Regression ROC AUC:")
print(roc_lr$auc)



# Predict probabilities for the test data
y_pred_prob_lr <- predict(lr_model, newdata = X_test, type = "response")

# Calculate ROC curve
roc_lr <- roc(y_test, y_pred_prob_lr)

# Plot ROC curve
plot(roc_lr, main = "ROC Curve for Logistic Regression", col = "blue", lwd = 2)






# Train the Random Forest model
rfc <- randomForest(x = X_train, y = as.factor(y_train), ntree = 50, mtry = 3, nodesize = 50)

# Predict on test data
y_pred_rfc <- predict(rfc, newdata = X_test)

# Evaluate performance
confusion_matrix_rfc <- confusionMatrix(y_pred_rfc, as.factor(y_test))
print("Random Forest Confusion Matrix:")
print(confusion_matrix_rfc)

roc_rfc <- roc(y_test, as.numeric(y_pred_rfc))
print("Random Forest ROC AUC:")
print(roc_rfc$auc)




# Predict probabilities for the test data
y_pred_prob_rfc <- predict(rfc, newdata = X_test, type = "prob")[,2]  # Assuming the positive class is in the second column

# Calculate ROC curve
roc_rfc <- roc(y_test, y_pred_prob_rfc)

# Plot ROC curve
plot(roc_rfc, main = "ROC Curve for Random Forest", col = "red", lwd = 2)




# Plot ROC curves for both models
plot(roc_lr, main = "ROC Curve Comparison", col = "blue", lwd = 2)
plot(roc_rfc, col = "red", lwd = 2, add = TRUE)
legend("bottomright", legend = c("Logistic Regression", "Random Forest"), col = c("blue", "red"), lwd = 2)




########### CONFUSION MATRIX HEAP FOR RANDOM FOREST AND LOGISTIC REGRESSION

# Logistic Regression
y_pred_class_lr <- ifelse(y_pred_prob_lr > 0.5, 1, 0)  # Binary classification
conf_matrix_lr <- confusionMatrix(as.factor(y_pred_class_lr), as.factor(y_test))

# Random Forest
y_pred_class_rfc <- predict(rfc, newdata = X_test)
conf_matrix_rfc <- confusionMatrix(y_pred_class_rfc, as.factor(y_test))

# Logistic Regression
conf_matrix_lr_df <- as.data.frame(as.table(conf_matrix_lr$table))

# Random Forest
conf_matrix_rfc_df <- as.data.frame(as.table(conf_matrix_rfc$table))




# Plot heatmap for Logistic Regression
ggplot(conf_matrix_lr_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Confusion Matrix Heatmap for Logistic Regression",
       x = "Predicted Class",
       y = "Actual Class") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1)



# Plot heatmap for Random Forest
ggplot(conf_matrix_rfc_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Heatmap for Random Forest",
       x = "Predicted Class",
       y = "Actual Class") +
  theme_minimal() +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1)




