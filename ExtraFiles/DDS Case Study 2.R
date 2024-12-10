library(dplyr)
library(tidyr)
library(naniar)
library(caret)

wine_train_raw <- read.csv("https://raw.githubusercontent.com/tblakearmstrong/CaseStudy2DDS/refs/heads/main/RawFiles/Wine%20Train.csv", header = TRUE)
wine_test_raw <- read.csv("https://raw.githubusercontent.com/tblakearmstrong/CaseStudy2DDS/refs/heads/main/RawFiles/Wine%20Test%20Set.csv", header = TRUE)
wine_type_location <- read.csv("https://raw.githubusercontent.com/tblakearmstrong/CaseStudy2DDS/refs/heads/main/RawFiles/Wine%20Types%20And%20Locations.csv", header = TRUE)

#-------------------------- HANDLE THE MISSING VALUES --------------------------#

# Replace blanks with NA in 'type' and 'location' columns
wine_type_location <- wine_type_location %>%
  mutate(
    type = na_if(type, ""), # If 'type' is blank replace with NA
    location = na_if(location, "")
  )

# Create a new category "Missing" for NA values in categorical columns
wine_type_location <- wine_type_location %>%
  mutate(
    type = ifelse(is.na(type), "Missing", type),
    location = ifelse(is.na(location), "Missing", location)
  )

# Verify the transformation
cat("Unique values in 'type' after transforming NA:\n")
print(unique(wine_type_location$type))

cat("\nUnique values in 'location' after transforming NA:\n")
print(unique(wine_type_location$location))

#-------------------------- MERGE AND PREPARE THE DATA --------------------------#

# Combine train and test datasets
wine_train_raw$quality <- as.integer(wine_train_raw$quality)
wine_test_raw$quality <- NA  # Placeholder for compatibility
wine_total_raw <- rbind(wine_train_raw, wine_test_raw)

# Merge with the cleaned 'wine_type_location'
wine_full_set <- merge(wine_total_raw, wine_type_location, by = "ID")

# Split back into train and test sets
wine_train <- wine_full_set[!is.na(wine_full_set$quality), ]
wine_test <- wine_full_set[is.na(wine_full_set$quality), ]

#-------------------------- LINEAR REGRESSION MODEL --------------------------#

# Split training data into train/validation sets
set.seed(123)
train_indices <- sample(seq_len(nrow(wine_train)), size = 0.8 * nrow(wine_train))
train_data <- wine_train[train_indices, ]
val_data <- wine_train[-train_indices, ] # Validation set

# Train Linear Regression Model
lm_model <- lm(quality ~ fixed_acidity + volatile_acidity + residual_sugar + chlorides +
                 free_sulfur_dioxide + total_sulfur_dioxide + density + pH + sulphates + alcohol +
                 type + location, data = train_data)

# Generate predictions on validation set
val_predictions <- predict(lm_model, newdata = val_data)

# Calculate Mean Absolute Error (MAE)
mae_val <- mean(abs(val_predictions - val_data$quality))
cat("\nLinear Regression MAE on Validation Set:", mae_val, "\n")

#-------------------------- PREDICT AND SAVE THE RESULTS --------------------------# 

# Predict on the test set
wine_test$quality <- predict(lm_model, newdata = wine_test)

# Save predictions to CSV
write.csv(wine_test %>% select(ID, quality), "Wine_Quality_Predictions_LM.csv", row.names = FALSE)
cat("Predictions saved to 'Wine_Quality_Predictions_LM.csv'\n")
