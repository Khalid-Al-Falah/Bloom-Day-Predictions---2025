install.packages("openxlsx")
library(openxlsx)
openxlsx::write.xlsx(data.frame(historic_temperatures), "historic_temperatures.xlsx")
openxlsx::write.xlsx(data.frame(final_data), "final_data.xlsx")
openxlsx::write.xlsx(data.frame(kyoto), "kyoto_2025.xlsx")

rel_cherry_data <- cherry %>%
  filter(year >= 1973) %>%
  select(-bloom_date)


# Get estimated 2025 weather data
weather_2025_estimates <- washington |> 
  bind_rows(liestal) |> 
  bind_rows(kyoto) |> 
  bind_rows(vancouver) |> 
  bind_rows(newyork)

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
# Merge estimates with historic temperatures. Get rid of station and attributes
x <- historic_temperatures %>%
  mutate(year = year(date)) %>%
  bind_rows(y) %>%
  select(-station, -attributes)

temp_with_periods <- x %>%
  mutate(Year = year(date), 
         Period = case_when(
           value <= 7.7778  ~ "Chilling Period",
           TRUE ~ "Warming Period"))   # Default case




a <- temp_with_periods %>%
  filter(datatype == 'TAVG') %>%
  mutate(Month = month(date)) %>%
  group_by(location, Year, Month) %>%
  summarize(avg_tmax = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = case_when(
           avg_tmax <= 7.7778  ~ "Chilling Period",
           TRUE ~ "Warming Period"))   # Default case




# Gather estimated temp for 2025 and add it into b
b <- temp_with_periods %>%
  filter(datatype == 'TAVG') %>%
  mutate(Week = week(date)) %>%
  group_by(location, Year, Week) %>%
  summarize(avg_TAVG = mean(value, na.rm = TRUE), .groups = "drop") %>%
  mutate(Period = case_when(
    avg_TAVG <= 7.7778  ~ "Chilling Period",
    TRUE ~ "Warming Period"))   # Default case



colnames(rel_cherry_data)[colnames(rel_cherry_data) == "year"] <- "Year"

test <- b %>%
  pivot_wider(names_from=c(Week, Period), values_from = avg_TAVG) %>%
  left_join(rel_cherry_data, by = c("location", "Year"))
new_final_data <- test %>%
  



chilling_warming_summary <- b %>%
  group_by(location, Year) %>%
  summarize(
    chilling_weeks = sum(Period == "Chilling Period"),  # Total chilling weeks
    total_chilling = sum(ifelse(Period == "Chilling Period", avg_TAVG, 0), na.rm = TRUE),  # Total chilling accumulation
    avg_chilling_temp = mean(ifelse(Period == "Chilling Period", avg_TAVG, NA), na.rm = TRUE),  # Avg temp during chilling
    
    warming_weeks = sum(Period == "Warming Period"),  # Total warming weeks
    total_warming = sum(ifelse(Period == "Warming Period", avg_TAVG, 0), na.rm = TRUE),  # Total warming accumulation
    avg_warming_temp = mean(ifelse(Period == "Warming Period", avg_TAVG, NA), na.rm = TRUE),  # Avg temp during warming
    .groups = "drop"
  )

colnames(chilling_warming_summary)[colnames(chilling_warming_summary) == "Year"] <- "year"

final_data <- rel_cherry_data %>%
  left_join(chilling_warming_summary, by = c("location", "year"))

# Remove rows with missing weather data

final_data <- final_data %>% 
  filter(!is.na(chilling_weeks) & !is.na(warming_weeks)) %>%
  mutate(year = as.numeric(year))
weather_2025 <- chilling_warming_summary %>%
  filter(year == 2025)
weather_2025[is.na(weather_2025)] <- 0

data_2025 <- rel_cherry_data %>%
  left_join(weather_2025_estimates, by = c("location", "Year" = "year"))


## Testing with Elastic Net:
library(glmnet)
library(dplyr)
# Manually create the design matrix, with out the intercept!
cherry_train <- final_data %>%
  filter(year <= 2015)
cherry_train <- cherry_train %>%
  mutate(bloom_date = as.numeric(lubridate::yday(as.Date(bloom_date))))

cherry_test <- final_data %>%
  filter(year > 2015)
cherry_test <- cherry_test %>%
  mutate(bloom_date = as.numeric(lubridate::yday(as.Date(bloom_date))))

all_locations <- unique(final_data$location)  # Get all unique locations in full dataset

cherry_train <- cherry_train %>%
  mutate(location = factor(location, levels = all_locations))

cherry_test <- cherry_test %>%
  mutate(location = factor(location, levels = all_locations))


model_matrix <- model.matrix(bloom_doy ~ ., data = cherry_train)[, -1]
lasso_fit <- glmnet(x = model_matrix,
                    y = cherry_train$bloom_date,
                    family = "gaussian", alpha = 0.5)
set.seed(123)
cv_fit <- replicated_cv_glmnet(
  x = model_matrix,
  y = cherry_train$bloom_date,
  replications = 10,  # Number of replications
  family = "gaussian",
  alpha = 0.5,  # Elastic Net (0 = Ridge, 1 = Lasso)
  nfolds = 10  # Number of folds
)

best_lambda <- cv_fit$lambda.min
lambda_1se <- cv_fit$lambda.1se

# Fit the final model using the optimal lambda
final_model <- glmnet(
  x = model_matrix,
  y = cherry_train$bloom_date,
  family = "gaussian",
  alpha = 0.5,
  lambda = best_lambda
)


# Prepare test set matrix
x_test <- model.matrix(bloom_doy ~ ., data = cherry_test)[, -1]

# Predict bloom dates
predictions <- predict(final_model, newx = x_test)

# Compare predictions with actual bloom dates
results <- data.frame(
  year = cherry_test$year,
  actual = cherry_test$bloom_date,
  predicted = as.vector(predictions)
)

print(results)
# Compute RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((results$actual - results$predicted)^2))
cat("RMSE:", rmse, "\n")


# Ensure factor levels match those in cherry_train
weather_2025 <- weather_2025 %>%
  left_join(final_data %>% select(location, lat, long, alt) %>% distinct(), by = "location")

# Ensure `weather_2025` contains the same predictors as the training set
# (No bloom_date, since it's the response variable)
model_matrix_2025 <- model.matrix(~ ., data = weather_2025)[, -1]  # Remove intercept

# Predict bloom date using the best lambda from cross-validation
predicted_bloom_2025 <- predict(lasso_fit, s = best_lambda, newx = model_matrix_2025)

# Print result
cat("Predicted bloom date for 2025:", predicted_bloom_2025, "\n")




