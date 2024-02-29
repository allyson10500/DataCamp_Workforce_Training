# Create a data split object
home_split <- initial_split(home_sales, 
                            prop = 0.7, 
                            strata = selling_price)

# Create the training data
home_training <- home_split %>%
  training()

# Create the test data
home_test <- home_split %>% 
  testing()

# Check number of rows in each dataset
nrow(home_training)
nrow(home_test)

# Distribution of selling_price in training data
home_training %>% 
  summarize(min_sell_price = min(selling_price),
            max_sell_price = max(selling_price),
            mean_sell_price = mean(selling_price),
            sd_sell_price = sd(selling_price))

# Distribution of selling_price in test data
home_test %>% 
  summarize(min_sell_price = min(selling_price),
            max_sell_price = max(selling_price),
            mean_sell_price = mean(selling_price),
            sd_sell_price = sd(selling_price))

# Initialize a linear regression object, linear_model
linear_model <- linear_reg() %>% 
  # Set the model engine
  set_engine('lm') %>% 
  # Set the model mode
  set_mode('regression')

# Fit the model using the training data
lm_fit <- linear_model %>% 
  fit(selling_price ~ home_age + sqft_living,
      data = home_training)

# Print lm_fit to view model information
lm_fit


tidy(lm_fit)

# Predict selling_price
home_predictions <- predict(lm_fit,
                            new_data = home_test)

# View predicted selling prices
home_predictions

# Combine test data with predictions
home_test_results <- home_test %>% 
  select(selling_price, home_age, sqft_living) %>% 
  bind_cols(home_predictions)

# View results
home_test_results

# Print home_test_results
home_test_results

# Calculate the RMSE metric
home_test_results %>% 
  rmse(truth = selling_price, estimate = .pred)

# Calculate the R squared metric
home_test_results %>% 
  rsq(truth = selling_price, estimate = .pred)

# Create an R squared plot of model performance
ggplot(home_test_results, aes(x = selling_price, y = .pred)) +
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')

# Define a linear regression model
linear_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression')

# Train linear_model with last_fit()
linear_fit <- linear_model %>% 
  last_fit(selling_price ~ ., split = home_split)

# Collect predictions and view results
predictions_df <- linear_fit %>% collect_predictions()
predictions_df
                                        
# Make an R squared plot using predictions_df
ggplot(predictions_df, aes(x = selling_price, y = .pred)) + 
  geom_point(alpha = 0.5) + 
  geom_abline(color = 'blue', linetype = 2) +
  coord_obs_pred() +
  labs(x = 'Actual Home Selling Price', y = 'Predicted Selling Price')

# Create data split object
telecom_split <- initial_split(telecom_df, prop = 0.75, strata = canceled_service)

# Create the training data
telecom_training <- telecom_split %>% 
  training()

# Create the test data
telecom_test <- telecom_split %>% 
  testing()

# Check the number of rows
nrow(telecom_training)
nrow(telecom_test)

# Specify a logistic regression model
logistic_model <- logistic_reg() %>% 
  # Set the engine
  set_engine('glm') %>% 
  # Set the mode
  set_mode('classification')

# Fit to training data
logistic_fit <- logistic_model %>% 
  fit(canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges,
      data = telecom_training)

# Print model fit object
logistic_fit

# Predict outcome categories
class_preds <- predict(logistic_fit, new_data = telecom_test,
                       type = 'class')

# Obtain estimated probabilities for each outcome value
prob_preds <- predict(logistic_fit, new_data = telecom_test, 
                      type = 'prob')

# Combine test set results
telecom_results <- telecom_test %>% 
  select(canceled_service) %>% 
  bind_cols(class_preds, prob_preds)

# View results tibble
telecom_results

# Calculate the confusion matrix
conf_mat(telecom_results, truth = canceled_service,
    estimate = .pred_class)

# Calculate the accuracy
accuracy(telecom_results, truth = canceled_service,
    estimate = .pred_class)

# Calculate the sensitivity
sens(telecom_results, truth = canceled_service,
    estimate = .pred_class)

# Calculate the specificity
spec(telecom_results, truth = canceled_service,
    estimate = .pred_class)

# Create a custom metric function
telecom_metrics <- metric_set(accuracy, sens, spec)

# Calculate metrics using model results tibble
telecom_metrics(telecom_results, truth = canceled_service,
                estimate = .pred_class)

# Create a confusion matrix
conf_mat(telecom_results, truth = canceled_service, estimate = .pred_class) %>% 
  # Pass to the summary() function
  summary()

# Create a confusion matrix
conf_mat(telecom_results,
         truth = canceled_service,
         estimate = .pred_class) %>% 
  # Create a heat map
  autoplot(type = 'heatmap')

# Create a confusion matrix
conf_mat(telecom_results,
         truth = canceled_service,
         estimate = .pred_class) %>% 

  # Create a mosaic plot
   autoplot(type = 'mosaic')

# Calculate metrics across thresholds
threshold_df <- telecom_results %>% 
  roc_curve(truth = canceled_service, .pred_yes)

# View results
threshold_df

# Plot ROC curve
threshold_df %>% 
  autoplot()

# Calculate ROC AUC
roc_auc(telecom_results,
    truth = canceled_service, 
    .pred_yes)

# Train model with last_fit()
telecom_last_fit <- logistic_model %>% 
  last_fit(canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges,
           split = telecom_split)

# View test set metrics
telecom_last_fit %>% 
  collect_metrics()

# Collect predictions
last_fit_results <- telecom_last_fit %>% 
  collect_predictions()

# View results
last_fit_results

# Custom metrics function
last_fit_metrics <- metric_set(accuracy, sens,
                               spec, roc_auc)

# Calculate metrics
last_fit_metrics(last_fit_results,
                 truth = canceled_service,
                 estimate = .pred_class,
                 .pred_yes)

# Train a logistic regression model - Easy Combined Way
logistic_fit <- logistic_model %>% 
  last_fit(canceled_service ~ avg_call_mins + avg_intl_mins + monthly_charges + months_with_company, 
           split = telecom_split)

# Collect metrics
logistic_fit %>% 
  collect_metrics()

# Collect model predictions
logistic_fit %>% 
  collect_predictions() %>% 
  # Plot ROC curve
  roc_curve(truth = canceled_service, .pred_yes) %>% 
  autoplot()

telecom_rec <- recipe(canceled_service ~ .,
                      data = telecom_df) %>% 
  step_log(avg_call_mins, base = 10)

summary(telecom_rec)

# Specify feature engineering recipe
telecom_log_rec <- recipe(canceled_service~., 
                          data = telecom_training) %>%
  # Add log transformation step for numeric predictors
  step_log(avg_call_mins, avg_intl_mins, base = 10)

# Print recipe object
telecom_log_rec

# View variable roles and data types
telecom_log_rec %>%
  summary()

# Train the telecom_log_rec object
telecom_log_rec_prep <- telecom_log_rec %>% 
  prep(training = telecom_training)

# Apply to training data
telecom_log_rec_prep %>% 
  bake(new_data = NULL)

# Train the telecom_log_rec object
telecom_log_rec_prep <- telecom_log_rec %>% 
  prep(training = telecom_training)

# Apply to test data
telecom_log_rec_prep %>% 
  bake(new_data = telecom_test)

telecom_training %>% 
  # Select numeric columns
  select_if(is.numeric) %>% 
  # Calculate correlation matrix
  cor()

telecom_training %>% 
  # Select numeric columns
  select_if(is.numeric) %>% 
  # Calculate correlation matrix
  cor()

# Plot correlated predictors
ggplot(telecom_training, aes(x = avg_data_gb, y = monthly_charges)) + 
  # Add points
  geom_point()  + 
  # Add title
  labs(title = "Monthly Charges vs. Average Data Usage",
       y = 'Monthly Charges ($)', x = 'Average Data Usage (GB)') 

# Specify a recipe object
telecom_cor_rec <- recipe(canceled_service ~ .,
                          data = telecom_training) %>% 
  # Remove correlated variables
  step_corr(all_numeric(), threshold = 0.8)

# Train the recipe
telecom_cor_rec_prep <- telecom_cor_rec %>% 
  prep(training = telecom_training)

# Apply to training data
telecom_cor_rec_prep %>% 
  bake(new_data = NULL)

# Apply to test data
telecom_cor_rec_prep %>% 
  bake(new_data = telecom_test)

# Specify a recipe object
telecom_norm_rec <- recipe(canceled_service ~ .,
                          data = telecom_training) %>% 
  # Remove correlated variables
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric())

# Train the recipe
telecom_norm_rec_prep <- telecom_norm_rec %>% 
  prep(training = telecom_training)

# Apply to test data
telecom_norm_rec_prep %>% 
  bake(new_data = telecom_test)

telecom_recipe_1 <- 
  recipe(canceled_service ~ avg_data_gb + contract, data = telecom_training)  %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>% 
  step_dummy(all_nominal(), -all_outcomes())

# Train and apply telecom_recipe_1 on the test data
telecom_recipe_1 %>% 
  prep(training = telecom_training) %>% 
  bake(new_data = telecom_test)

telecom_recipe_2 <- 
  recipe(canceled_service ~ avg_data_gb + contract, data = telecom_training)  %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>% 
  step_normalize(all_numeric(), -all_outcomes())

# Train and apply telecom_recipe_2 on the test data
telecom_recipe_2 %>% 
  prep(training = telecom_training) %>% 
  bake(new_data = telecom_test)

# Create a recipe that predicts canceled_service using the training data
telecom_recipe <- recipe(canceled_service~., data = telecom_training) %>% 
  # Remove correlated predictors
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())

# Train your recipe and apply it to the test data
telecom_recipe %>% 
  prep(training = telecom_training) %>% 
  bake(new_data = telecom_test)

telecom_recipe <- recipe(canceled_service ~ ., data = telecom_training) %>% 
  # Removed correlated predictors
  step_corr(all_numeric(), threshold = 0.8) %>% 
  # Log transform numeric predictors
  step_log(all_numeric(), base = 10) %>%
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())

# Train recipe
telecom_recipe_prep <- telecom_recipe %>% 
  prep(training = telecom_training)

# Transform training data
telecom_training_prep <- telecom_recipe_prep %>% 
  bake(new_data = NULL)

# Transform test data
telecom_test_prep <- telecom_recipe_prep %>% 
  bake(new_data = telecom_test)

telecom_test_prep

# Train logistic model
logistic_fit <- logistic_model %>% 
  fit(canceled_service ~ ., data = telecom_training_prep)

# Obtain class predictions
class_preds <- predict(logistic_fit, new_data = telecom_test_prep,
                       type = 'class')

# Obtain estimated probabilities
prob_preds <- predict(logistic_fit, new_data = telecom_test_prep, 
                      type = 'prob')

# Combine test set results
telecom_results <- telecom_test_prep %>% 
  select(canceled_service) %>% 
  cbind(class_preds, prob_preds)

telecom_results

# Create a confusion matrix
telecom_results %>% 
  conf_mat(truth = canceled_service, estimate = .pred_class)

# Calculate sensitivity
telecom_results %>% 
  sens(truth = canceled_service, estimate = .pred_class)

# Calculate specificity
telecom_results %>% 
  spec(truth = canceled_service, estimate = .pred_class)

# Plot ROC curve
telecom_results %>% 
  roc_curve(truth = canceled_service, .pred_yes) %>% 
  autoplot()

# Create data split object
loans_split <- initial_split(loans_df, 
                             strata = loan_default)

# Build training data
loans_training <- loans_split %>% 
  training()

# Build test data
loans_test <- loans_split %>% 
  testing()

# Check for correlated predictors
loans_training %>% 
  # Select numeric columns
  select_if(is.numeric) %>% 
  # Calculate correlation matrix
  cor()

dt_model <- decision_tree() %>% 
  # Specify the engine
  set_engine('rpart') %>% 
  # Specify the mode
  set_mode('classification')

# Build feature engineering pipeline
loans_recipe <- recipe(loan_default~.,
                        data = loans_training) %>% 
  # Correlation filter
  step_corr(all_numeric(), threshold = 0.85) %>% 
  # Normalize numeric predictors
  step_normalize(all_numeric()) %>% 
  # Create dummy variables
  step_dummy(all_nominal(), -all_outcomes())

loans_recipe

# Create a workflow
loans_dt_wkfl <- workflow() %>% 
  # Include the model object
  add_model(dt_model) %>% 
  # Include the recipe object
  add_recipe(loans_recipe)

# Train the workflow
loans_dt_wkfl_fit <- loans_dt_wkfl %>% 
  last_fit(split = loans_split)

# Calculate performance metrics on test data
loans_dt_wkfl_fit %>% 
  collect_metrics()

# Create cross validation folds
set.seed(290)
loans_folds <- vfold_cv(loans_training, v = 5,
                       strata = loan_default)

# Create custom metrics function
loans_metrics <- metric_set(roc_auc, sens, spec)

# Fit resamples
loans_dt_rs <- loans_dt_wkfl %>% 
  fit_resamples(resamples = loans_folds,
                metrics = loans_metrics)

# View performance metrics
loans_dt_rs %>% 
  collect_metrics()

logistic_model <- logistic_reg() %>% 
  # Specify the engine
  set_engine('glm') %>% 
  # Specify the mode
  set_mode('classification')

# Create workflow
loans_logistic_wkfl <- workflow() %>% 
  # Add model
  add_model(logistic_model) %>% 
  # Add recipe
  add_recipe(loans_recipe)

# Fit resamples
loans_logistic_rs <- loans_logistic_wkfl %>% 
  fit_resamples(resamples = loans_folds,
                metrics = loans_metrics)

# View performance metrics
loans_logistic_rs %>% collect_metrics()

# Detailed cross validation results
dt_rs_results <-  loans_dt_rs %>% 
  collect_metrics(summarize = FALSE)

# Explore model performance for decision tree
dt_rs_results %>% 
  group_by(.metric) %>% 
  summarize(min = min(.estimate), 
            median = median(.estimate), 
            max = max(.estimate))

# Detailed cross validation results
logistic_rs_results <- loans_logistic_rs %>% 
  collect_metrics(summarize = FALSE)


# Explore model performance for logistic regression
logistic_rs_results %>% 
 group_by(.metric) %>% 
  summarize(min = min(.estimate), 
            median = median(.estimate), 
            max = max(.estimate))

# Set tuning hyperparameters
dt_tune_model <- decision_tree(cost_complexity = tune(),
                               tree_depth = tune(),
                               min_n = tune()) %>% 
  # Specify engine
  set_engine('rpart') %>% 
  # Specify mode
  set_mode('classification')

# Create a tuning workflow
loans_tune_wkfl <- loans_dt_wkfl %>% 
  # Replace model
  update_model(dt_tune_model)

loans_tune_wkfl

# Hyperparameter tuning with grid search
set.seed(214)
dt_grid <- grid_random(parameters(dt_tune_model),
                       size = 5)

# Hyperparameter tuning
dt_tuning <- loans_tune_wkfl %>% 
  tune_grid(resamples = loans_folds,
            grid = dt_grid,
            metrics = loans_metrics)

# View results
dt_tuning %>% 
  collect_metrics()

# Collect detailed tuning results
dt_tuning_results <- dt_tuning %>% 
  collect_metrics(summarize = FALSE)

# Explore detailed ROC AUC results for each fold
dt_tuning_results %>% 
  filter(.metric == "roc_auc") %>% 
  group_by(id) %>% 
  summarize(min_roc_auc = min(.estimate),
            median_roc_auc = median(.estimate),
            max_roc_auc = max(.estimate))

# Display 5 best performing models
dt_tuning %>% 
  show_best(metric = 'roc_auc', n = 5)

# Select based on best performance
best_dt_model <- dt_tuning %>% 
  # Choose the best model based on roc_auc
  select_best(metric = 'roc_auc')

# Finalize your workflow
final_loans_wkfl <- loans_tune_wkfl %>% 
  finalize_workflow(best_dt_model)

final_loans_wkfl

# Train finalized decision tree workflow
loans_final_fit <- final_loans_wkfl %>% 
  last_fit(split = loans_split)

# View performance metrics
loans_final_fit %>% 
  collect_metrics()

# Create an ROC curve
loans_final_fit %>% 
  # Collect predictions
  collect_predictions() %>% 
  # Calculate ROC curve metrics
  roc_curve(truth = loan_default, .pred_yes) %>% 
  # Plot the ROC curve
  autoplot()

