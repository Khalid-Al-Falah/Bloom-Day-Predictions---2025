#' Author: Khalid Al-Falah
#' NOTE: Run replicated_CV_glmnet.r and `Weather For 20250 Estimates`.r files first

library(readxl)
library(dplyr)
library(tidyverse)
library(glmnet)
# Gather historic temperature and cherry blossom data
historic_temperatures <- read_excel("historic_temperatures.xlsx")

# Gather cherry blossom data
cherry <- read.csv("data/washingtondc.csv") |> 
  bind_rows(read.csv("data/liestal.csv")) |> 
  bind_rows(read.csv("data/kyoto.csv")) |> 
  bind_rows(read.csv("data/vancouver.csv")) |> 
  bind_rows(read.csv("data/nyc.csv"))

# Filter out blossom data after 1973 (no weather data before then)
rel_cherry_data <- cherry %>%
  filter(year >= 1973) %>%
  select(-bloom_date)

# Get estimated 2025 weather data
weather_2025_estimates <- washington |> 
  bind_rows(liestal) |> 
  bind_rows(kyoto) |> 
  bind_rows(vancouver) |> 
  bind_rows(newyork)

# Convert estimated data into proper format
y <- weather_2025_estimates %>%
  pivot_longer(cols = c(tmax, tmin, temp), names_to = "datatype", values_to = "value") %>%
  mutate(
    datatype = case_when(
      datatype == "tmax" ~ "TMAX",
      datatype == "tmin" ~ "TMIN",
      datatype == "temp" ~ "TAVG",
      TRUE ~ datatype  # Default case (shouldn't be needed, but good practice)
    ),
    value = (value - 32) * 5/9  # Convert Fahrenheit to Celsius
  )
# Merge estimates with historic temperatures
x <- historic_temperatures %>%
  mutate(year = year(date)) %>%
  filter(year != 2025) %>%
  bind_rows(y) %>%
  select(-station)
# Add warming and chilling periods
temp_with_periods <- x %>%
  mutate(Year = year(date), 
         Period = case_when(
           value <= 7.7778  ~ "Chilling Period",
           TRUE ~ "Warming Period"))   # Default case
# Split estimates into weeks
b <- temp_with_periods %>%
  filter(datatype == 'TAVG') %>%
  mutate(Week = week(date)) %>%
  group_by(location, Year, Week) %>%
  summarize(avg_TAVG = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = case_when(
    avg_TAVG <= 7.7778  ~ "Chilling Period",
    TRUE ~ "Warming Period"))   # Default case

# Ensure col name for cherry data
colnames(rel_cherry_data)[colnames(rel_cherry_data) == "year"] <- "Year"
# Create final data set. Exclude 2025 information
final_data <- b %>%
  pivot_wider(names_from=c(Week, Period), values_from = avg_TAVG) %>%
  left_join(rel_cherry_data, by = c("location", "Year")) %>%
  filter(Year != 2025)
# Remove these observations. They have no bloom data
final_data <- final_data %>%
  filter(!(location == "newyorkcity" & Year == 2023) & 
           !(location == "vancouver" & Year == 2021))

# Set up 2025 estimated data to be in same format as final data
data_2025 <- b %>%
  pivot_wider(names_from=c(Week, Period), values_from = avg_TAVG) %>%
  left_join(rel_cherry_data, by = c("location", "Year")) %>%
  filter(Year == 2025)

# Extract location and coordinate information from final_data
location_coords <- final_data %>%
  select(location, lat, long, alt) %>%
  distinct(location, .keep_all = TRUE)  # Ensure only one row per location


# Join the extracted coordinates with data_2025
data_2025 <- data_2025 %>%
  left_join(location_coords, by = "location") %>%
  mutate(
    lat = coalesce(lat.x, lat.y),
    long = coalesce(long.x, long.y),
    alt = coalesce(alt.x, alt.y)
  ) %>%
  select(-lat.x, -lat.y, -long.x, -long.y, -alt.x, -alt.y)  # Remove duplicate columns


# Manually create the design matrix, without the intercept!
cherry_train <- final_data %>%
  filter(Year < 2024)
cherry_test <- final_data %>%
  filter(Year >= 2023)

all_locations <- unique(final_data$location)  # Get all unique locations in full dataset

# Make sure cherry_train has correct # of levels for locations
cherry_train <- cherry_train %>%
  mutate(location = factor(location, levels = all_locations))
# Make sure nas are replaced with 0s
cherry_train <- cherry_train %>%
  mutate(across(matches("Chilling Period|Warming Period"), ~ replace_na(., 0)))
# Add linear weights to the data
cherry_train <- cherry_train %>%
  mutate(weight = exp(Year - max(Year)))

# Do the same for cherry_test
cherry_test <- cherry_test %>%
  mutate(location = factor(location, levels = all_locations))
cherry_test <- cherry_test %>%
  mutate(across(matches("Chilling Period|Warming Period"), ~ replace_na(., 0)))
cherry_test <- cherry_test %>%
  mutate(weight = exp((Year - min(Year)) / (max(Year) - min(Year))))

# Update final_data to also have the levels just in case we want it later
final_data <- final_data %>%
  mutate(location = factor(location, levels = all_locations))

# Create the model matrix
model_matrix <- model.matrix(bloom_doy ~ . - weight, data = cherry_train)[, -1]
# Create the model
lasso_fit <- glmnet(x = model_matrix,
                    y = cherry_train$bloom_doy,
                    family = "gaussian", alpha = 0, weights = cherry_train$weight)
# Cross validate
set.seed(123)
cv_fit <- replicated_cv_glmnet(
  x = model_matrix,
  y = cherry_train$bloom_doy,
  replications = 10,  # Number of replications
  family = "gaussian",
  alpha = 0,  # Elastic Net (0 = Ridge, 1 = Lasso)
  nfolds = 15  # Number of folds,
)

best_lambda <- cv_fit$lambda.min
lambda_1se <- cv_fit$lambda.1se

# Fit the final model using the optimal lambda
final_model <- glmnet(
  x = model_matrix,
  y = cherry_train$bloom_doy,
  family = "gaussian",
  alpha = 0,
  lambda = best_lambda,
  weights = cherry_train$weight
)


# Prepare test set matrix
x_test <- model.matrix(bloom_doy ~ . -weight, data = cherry_test)[, -1]

# Predict bloom dates
predictions <- predict(final_model, newx = x_test)

# Compare predictions with actual bloom dates
results <- data.frame(
  Year = cherry_test$Year,
  Actual = cherry_test$bloom_doy,
  Predicted = as.vector(predictions)
)

print(results)
# Compute RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
cat("RMSE:", rmse, "\n")


# Predicting 2025 data. We need to create a model using the most recent historic temperatures
# Set up final_train data to be in same format as cherry_train
final_train <- final_data 
final_train <- final_train %>%
  mutate(location = factor(location, levels = all_locations))
final_train <- final_train %>%
  mutate(across(matches("Chilling Period|Warming Period"), ~ replace_na(., 0)))
final_train <- final_train %>%
  mutate(weight = exp(Year - max(Year)))

# Find the model matrix
final_model_matrix <- model.matrix(bloom_doy ~ . - weight, data = final_train)[, -1]
# Apply lasso
lasso_fit <- glmnet(x = final_model_matrix,
                    y = final_train$bloom_doy,
                    family = "gaussian", alpha = 0, weights = final_train$weight)
# K-fold cross validate
set.seed(123)
cv_fit <- replicated_cv_glmnet(
  x = final_model_matrix,
  y = final_train$bloom_doy,
  replications = 10,  # Number of replications
  family = "gaussian",
  alpha = 0,  # Elastic Net (0 = Ridge, 1 = Lasso)
  nfolds = 15  # Number of folds,
)

best_lambda <- cv_fit$lambda.min
lambda_1se <- cv_fit$lambda.1se

# Fit the final model using the optimal lambda
combined_final_model <- glmnet(
  x = final_model_matrix,
  y = final_train$bloom_doy,
  family = "gaussian",
  alpha = 0,
  lambda = best_lambda,
  weights = final_train$weight
)

# Set up final_train in same format as final_train
final_test <- data_2025 %>%
  mutate(location = factor(location, levels = all_locations))
final_test <- final_test %>%
  mutate(across(matches("Chilling Period|Warming Period"), ~ replace_na(., 0))) %>%
  select(-bloom_doy)

# Ensure `data_2025` contains the same predictors as the training set
# (No bloom_date, since it's the response variable)
model_matrix_2025 <- model.matrix(~ ., data = final_test)[, -1]  # Remove intercept

# Predict bloom date using the best lambda from cross-validation
predicted_bloom_2025 <- predict(combined_final_model, s = best_lambda, newx = model_matrix_2025)
predicted_results_2025 <- data.frame(Location = final_test$location, Predicted_Bloom_DOY = round(predicted_bloom_2025),
                                     Year = 2025)
# Print result
cat("Predicted bloom date for 2025:")
predicted_results_2025 %>%
  mutate(predicted_date = strptime(paste(Year, s1), "%Y %j") |> 
           as_date())
