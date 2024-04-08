# Find the number of features
credit_df %>% ncol()

# Compute each column variance
credit_df %>% 
  summarize(across(everything(), ~ var(., na.rm = TRUE))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "variance")

# Assign the zero-variance column
column_to_remove <- "num_credit_card"

# Create a correlation plot
credit_df %>% 
  select(where(is.numeric)) %>% 
  correlate() %>% 
  shave() %>% 
  rplot(print_cor = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Calculate the class probabilities
p_yes <- 4/12
p_no <- 8/12

# Calculate the entropy
entropy_root <- -(p_yes * log2(p_yes)) + 
  -(p_no * log2(p_no))

entropy_root

# Calculate the class probabilities in the left split
p_left_yes <- 3/5 
p_left_no <- 2/5

# Calculate the entropy of the left split
entropy_left <- -(p_left_yes * log2(p_left_yes)) +
  -(p_left_no * log2(p_left_no))

# Calculate the class probabilities in the right split
p_right_yes <- 1/7
p_right_no <- 6/7

# Calculate the entropy of the right split
entropy_right <- -(p_right_yes * log2(p_right_yes)) +
  -(p_right_no * log2(p_right_no))

# Calculate the split weights
p_left <- 5/12
p_right <- 7/12

# Calculate the information gain
info_gain <- entropy_root - 
  (p_left * entropy_left +
  p_right * entropy_right)

info_gain

# Calculate the minimum number of value combinations
healthcare_cat_df %>% 
  summarize(across(everything(), ~ length(unique(.)))) %>% 
  prod()

# Create zero-variance filter
zero_var_filter <- house_sales_df %>% 
  summarize(across(everything(), ~ var(., na.rm = TRUE))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "variance") %>% 
  filter(variance == 0) %>% 
  pull(feature)

zero_var_filter

# Create a missing values filter
na_filter <- house_sales_df %>% 
  summarize(across(everything(), ~ sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "NA_count") %>% 
  filter(NA_count > 0) %>% 
  pull(feature)
  
na_filter

# Combine the two filters
low_info_filter <- c(zero_var_filter, na_filter)

# Apply the filter
house_sales_filtered_df <- house_sales_df %>% 
  select(-all_of(low_info_filter))

# Display five rows of the reduced data set
house_sales_filtered_df %>%
  head(5)

# Calculate total rows
n <-  nrow(house_sales_df)

# Calculate missing value ratios
missing_vals_df <- house_sales_df %>% 
  summarize(across(everything(),
~ sum(is.na(.)))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "num_missing_values") %>% 
  mutate(missing_val_ratio = num_missing_values / n)

# Display missing value ratios
missing_vals_df

# Create the missing values filter
missing_vals_filter <- missing_vals_df %>%
filter(missing_val_ratio <= 0.5) %>%
pull(feature)

# Apply the missing values filter
filtered_house_sales_df <- house_sales_df %>% 
  select(missing_vals_filter)

# Display the first five rows of data
filtered_house_sales_df %>% head(5)

# Create missing values recipe
missing_vals_recipe <- 
  recipe(price ~ ., data = house_sales_df) %>% 
  step_filter_missing(all_predictors(), threshold = 0.5) %>% 
  prep()
  
# Apply recipe to data
filtered_house_sales_df <- 
  bake(missing_vals_recipe, new_data = NULL)

# Display the first five rows of data
filtered_house_sales_df %>% head(5)

# Calculate feature variances
houses_sales_variances <- house_sales_df %>% 
  summarize(across(everything(), ~ var(scale(., center = FALSE), na.rm = TRUE))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "variance") %>% 
  arrange(desc(variance))

houses_sales_variances

# Set variance threshold and create filter
low_var_filter <- houses_sales_variances %>% 
  filter(variance < 0.1) %>% 
  pull(feature)

# Apply the filter
filtered_house_sales_df <- house_sales_df %>% 
  select(-all_of(low_var_filter))
filtered_house_sales_df %>% head(5)

# Prepare recipe
low_variance_recipe <- recipe(price ~ ., data = house_sales_df) %>% 
  step_zv(all_predictors()) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  prep()

# Apply recipe
filtered_house_sales_df <- bake(low_variance_recipe, new_data =   NULL)

# View list of features removed by the near-zero variance step 
tidy(low_variance_recipe, number = 3)

# Create a correlation plot of the house sales
house_sales_df %>% 
  correlate() %>% 
  shave() %>%
  rplot(print_cor = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Create a recipe using step_corr to remove numeric predictors correlated > 0.7
corr_recipe <-  
  recipe(price ~ ., data = house_sales_df) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.7) %>% 
  prep() 

# Apply the recipe to the data
filtered_house_sales_df <- 
  corr_recipe %>% 
  bake(new_data = NULL)

# Identify the features that were removed
tidy(corr_recipe, number = 1)

# Initialize the split
split <- initial_split(attrition_df, prop = 0.8, strata = Attrition)

# Extract training set
train <- split %>% training()

# Extract testing set
test <-  split %>% testing()

# Create recipe
feature_selection_recipe <- 
  recipe(Attrition ~ ., data = train) %>% 
  step_filter_missing(all_predictors(), threshold = 0.5) %>% 
  step_scale(all_numeric_predictors()) %>% 
  step_nzv(all_predictors()) %>% 
  prep()
  
# Create model
lr_model <- logistic_reg() %>% 
  set_engine("glm")

# Add recipe and model to a workflow
attrition_wflow <- workflow() %>% 
  add_recipe(feature_selection_recipe) %>% 
  add_model(lr_model)

# Fit workflow to train data
attrition_fit <- 
  attrition_wflow %>% fit(data = train)

# Add the test predictions to the test data
attrition_pred_df <- predict(attrition_fit, test) %>% 
  bind_cols(test %>% select(Attrition))

# Evaluate F score
f_meas(attrition_pred_df, Attrition, .pred_class)

# Display model estimates
tidy(attrition_fit, number = 1)

# Scale the target variable
house_sales_df <-  house_sales_df %>% 
  mutate(price = as.vector(scale(price)))

# Create the training and testing sets
split <- initial_split(house_sales_df, prop = 0.8)
train <- split %>% training()
test <-  split %>% testing()

# Create recipe to scale the predictors
lasso_recipe <- 
  recipe(price ~ ., data = train) %>% 
  step_normalize(all_numeric_predictors())

# Train workflow model with penalty = 0.001 and view model variables
lasso_model <- linear_reg(penalty = 0.001, mixture = 1, engine = "glmnet")

lasso_workflow <- workflow(preprocessor = lasso_recipe, spec = lasso_model)

tidy(lasso_workflow %>% fit(train)) %>% filter(estimate > 0)

# Train the workflow model with penalty = 0.01 and view model variables
lasso_model <- linear_reg(penalty = 0.01, mixture = 1, engine = "glmnet")

lasso_workflow <- workflow(preprocessor = lasso_recipe, spec = lasso_model)

tidy(lasso_workflow %>% fit(train)) %>% filter(estimate > 0)

# Train the workflow model with penalty = 0.1 and view model variables
lasso_model <- linear_reg(penalty = 0.1, mixture = 1, engine = "glmnet")

lasso_workflow <- workflow(preprocessor = lasso_recipe, spec = lasso_model)

tidy(lasso_workflow %>% fit(train)) %>% filter(estimate > 0)

# Create tune-able model
lasso_model <- linear_reg(penalty = tune(), mixture = 1, engine = "glmnet")

lasso_workflow <- workflow(preprocessor = lasso_recipe, spec = lasso_model)

# Create a cross validation sample and sequence of penalty values
train_cv <- vfold_cv(train, v = 3)

penalty_grid <- grid_regular(penalty(range = c(-3, -1)), levels = 20)

# Create lasso models with different penalty values
lasso_grid <- tune_grid(
  lasso_workflow,
  resamples = train_cv,
  grid = penalty_grid)

# Plot RMSE vs. penalty values
autoplot(lasso_grid, metric = "rmse")

# Retrieve the best RMSE
best_rmse <- lasso_grid %>% 
  select_best("rmse")

# Refit the model with the best RMSE
final_lasso <- 
  finalize_workflow(lasso_workflow, best_rmse) %>% 
  fit(train)

# Display the non-zero model coefficients
tidy(final_lasso) %>% 
  filter(estimate > 0)

# Specify the random forest model
rf_spec <- rand_forest(mode = "classification", trees = 200) %>% 
  set_engine("ranger", importance = "impurity") 

# Fit the random forest model with all predictors
rf_fit <- rf_spec %>% 
  fit(Attrition ~., data = train)

# Create the test set prediction data frame
predict_df <- test %>% 
  bind_cols(predict = predict(rf_fit, test))

# Calculate F1 performance
f_meas(predict_df, Attrition, .pred_class)

# Extract the top ten features
top_features <- rf_fit %>% 
  vi(rank = TRUE) %>% 
  filter(Importance <= 10) %>% 
  pull(Variable)

# Add the target variable to the feature list
top_features <- c(top_features, "Attrition")

# Reduce and print the data sets
train_reduced <- train[top_features]
test_reduced <- test[top_features]
train_reduced %>% head(5)
test_reduced %>% head(5)

# Fit a reduced model
rf_reduced_fit <- rf_spec %>% 
  fit(Attrition ~ ., data = train_reduced)

# Create test set prediction data frame
predict_reduced_df <- test_reduced %>% 
  bind_cols(predict = predict(rf_reduced_fit, test_reduced))

# Calculate F1 performance
f_meas(predict_reduced_df, Attrition, .pred_class)

# Perform PCA
pca_res <- prcomp(credit_df %>% select(-credit_score), scale. = TRUE)

# Plot principal components and feature vectors
autoplot(pca_res, 
         data = credit_df, 
         colour = 'credit_score', 
         alpha = 0.3,
         loadings = TRUE, 
         loadings.label = TRUE, 
         loadings.colour = "black", 
         loadings.label.colour = "black")

# Build a PCA recipe
pca_recipe <- recipe(price ~ ., data = train) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_pca(all_numeric_predictors(), num_comp = 5) 

# Fit a workflow with a default linear_reg() model spec
house_sales_fit <- workflow(preprocessor = pca_recipe, spec = linear_reg()) %>% 
  fit(train)

# Create prediction df for the test set
house_sales_pred_df <- predict(house_sales_fit, test) %>% 
  bind_cols(test %>% select(price))

# Calculate the RMSE
rmse(house_sales_pred_df, price, .pred)

# Fit PCA to only the predictors
pca <- prcomp(house_sales_df %>% select(-price))

# Plot PCA and color code the target variable
autoplot(pca, data = house_sales_df, colour = "price", alpha = 0.7) +
  scale_color_gradient(low="gray", high="blue")

# Fit t-SNE
set.seed(1234)
tsne <- Rtsne(house_sales_df %>% select(-price), check_duplicates = FALSE)

# Bind t-SNE coordinates to the data frame
tsne_df <- house_sales_df %>% 
  bind_cols(tsne_x = tsne$Y[,1], tsne_y = tsne$Y[,2])

# Plot t-SNE
tsne_df %>% 
  ggplot(aes(x = tsne_x, y = tsne_y, color = price)) +
  geom_point() +
  scale_color_gradient(low="gray", high="blue")

# Fit UMAP
set.seed(1234)
umap_df <- recipe(price ~ ., data = house_sales_df) %>% 
  step_normalize(all_predictors()) %>% 
  step_umap(all_predictors(), num_comp = 2) %>% 
  prep() %>% 
  juice() 

# Plot UMAP
umap_df %>%  
  ggplot(aes(x = UMAP1, y = UMAP2, color = price)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low="gray", high="blue")

# Create a recipe to apply UMAP feature extraction
umap_recipe <-  recipe(credit_score ~ ., data = train) %>% 
  step_normalize(all_predictors()) %>% 
  step_umap(all_predictors(), outcome = vars(credit_score), num_comp = 4)

# Specify a decision tree model
umap_dt_model <- decision_tree(mode = "classification")

# Add the recipe and model to a workflow
umap_dt_workflow <-  workflow() %>% 
  add_recipe(umap_recipe) %>% 
  add_model(umap_dt_model)
umap_dt_workflow

# Evaluate the unreduced decision tree model performance
f_meas(predict_df, credit_score, .pred_class)

# Fit the UMAP decision tree model
umap_dt_fit <- umap_dt_workflow %>% 
  fit(data = train)

# Create test set prediction data frame for the UMAP model
predict_umap_df <- test %>% 
  bind_cols(predict = predict(umap_dt_fit, test))

# Calculate F1 performance of the UMAP model
f_meas(predict_umap_df, credit_score, .pred_class)

