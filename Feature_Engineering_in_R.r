# Build a linear model for the newton the data and assign it to lr_force
lr_force <- lm(force ~ distance, data = newton)

# Create a new data frame by binding the prediction values to the original data
df <- newton %>% bind_cols(lr_pred = predict(lr_force))

# Generate a scatterplot of force vs. distance
df %>%
  ggplot(aes(x = distance, y = force)) +
  geom_point() +
# Add a regression line with the fitted values
  geom_line(aes(y = lr_pred), color = "blue", lwd = .75) +
  ggtitle("Linear regression of force vs. distance") +
  theme_classic()

# Create a new variable inv_square_distance
df_inverse <- df %>% mutate(inv_square_distance = 1/distance^2)

# Build a simple regression model
lr_force_2 <- lm(force ~ inv_square_distance, data = df_inverse)

# Bind your predictions to df_inverse
df_inverse <- df_inverse %>% bind_cols(lr2_pred = predict(lr_force_2))

df_inverse %>% ggplot(aes(x = distance, y = force)) +
  geom_point() +
  geom_line(aes(y = lr2_pred), col = "blue", lwd = .75) +
  ggtitle("Linear regression of force vs. inv_square_distance") +
  theme_classic()

flights <- flights %>%
  select(flight, sched_dep_time, dep_delay, sched_arr_time, carrier, origin, dest, distance, date, arrival) %>%

# Tranform all character-type variables to factors
  mutate(date = as.Date(date), across(where(is.character), as.factor))

# Split the flights data into test and train sets
set.seed(246)
split <- flights %>% initial_split(prop = 3/4, strata = arrival)
test <- training(split)
train <- testing(split)

test %>% select(arrival) %>% table() %>% prop.table()
train %>% select(arrival) %>% table() %>% prop.table()

lr_model <- logistic_reg()

# Assign an "ID" role to flight
lr_recipe <- recipe(arrival ~., data = train) %>% update_role(flight, new_role = "ID") %>%
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>% step_dummy(all_nominal_predictors())

# Bundle the model and the recipe into a workflow object
lr_workflow <- workflow() %>% add_model(lr_model) %>% add_recipe(lr_recipe)
lr_workflow

# Fit lr_workflow workflow to the test data  
lr_fit <- lr_workflow %>% fit(data = test)

# Tidy the fitted workflow  
tidy(lr_fit)

# Explore missing data on the attrition dataset
vis_miss(attrition)

# Select the variables with missing values and rerun the analysis on those variables.
attrition %>% 
  select("BusinessTravel", "DistanceFromHome",
         "StockOptionLevel", "WorkLifeBalance") %>%
  vis_miss()

lr_model <- logistic_reg()

lr_recipe <- 
  recipe(Attrition ~., data = train) %>%

# Update the role of "...1" to "ID"
  update_role(...1, new_role = "ID" ) %>%

# Impute values to all predictors where data are missing
  step_impute_knn(all_predictors()) %>%

# Create dummy variables for all nominal predictors
  step_dummy(all_nominal_predictors())

lr_recipe

# Bundle model and recipe in workflow
lr_workflow <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe)

# Fit workflow to the train data
lr_fit <- fit(lr_workflow, data = train)

# Generate an augmented data frame for performance assessment
lr_aug <- lr_fit %>% augment(test)

lr_aug %>% roc_curve(truth = Attrition, .pred_No) %>% autoplot()
bind_rows(lr_aug %>% roc_auc(truth = Attrition, .pred_No),          
          lr_aug %>% accuracy(truth = Attrition, .pred_class))

lr_recipe <- recipe(children ~., data = train) %>%

# Generate "day of the week", "week" and "month" features
  step_date(arrival_date, features = c("dow", "week", "month")) %>%

# Create dummy variables for all nominal predictors
  step_dummy(all_nominal_predictors())

# Bundle your model and recipe in a workflow
lr_workflow <-workflow() %>% add_model(lr_model) %>% add_recipe(lr_recipe)

# Fit the workflow to the training data
lr_fit <-  lr_workflow %>% fit(data = train)
lr_aug <- lr_fit %>% augment(test)
bind_rows(roc_auc(lr_aug,truth = children, .pred_children),accuracy(lr_aug,truth = children, .pred_class))

lr_model <- logistic_reg()

lr_recipe <- 
  recipe(Attrition~., data = train) %>%

# Normalize all numeric predictors
  step_normalize(all_numeric_predictors()) %>%

# Log-transform all numeric features, with an offset of one
  step_log(all_numeric_predictors, offset = 1)

lr_workflow <- 
  workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe)

lr_workflow

# Fit the workflow to the train data
lr_fit <- 
  fit(lr_workflow, data = train)

# Augment the lr_fit object
lr_aug <-
  augment(lr_fit, new_data = test)

lr_aug

# Define a custom assessment function 
class_evaluate <- metric_set(roc_auc, accuracy, sens, spec)

# Assess your model using your new function
class_evaluate(lr_aug, truth = Attrition,
               estimate = .pred_class,
               .pred_No)

# Create a plain recipe defining only the model formula
lr_recipe_plain <- 
  recipe(Attrition ~., data = train)

lr_workflow_plain <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_plain)
lr_fit_plain <- lr_workflow_plain %>%
  fit(train)
lr_aug_plain <-
  lr_fit_plain %>% augment(test)
lr_aug_plain %>% class_evaluate(truth = Attrition,
                 estimate = .pred_class,.pred_No)

# Create a recipe that uses Box-Cox to transform all numeric features
lr_recipe_BC <- 
  recipe(Attrition ~., data = train) %>%
  step_BoxCox(all_numeric())

lr_workflow_BC <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_BC)
lr_fit_BC <- lr_workflow_BC %>%
  fit(train)
lr_aug_BC <-
  lr_fit_BC %>% augment(test)
lr_aug_BC %>% class_evaluate(truth = Attrition,
                 estimate = .pred_class,.pred_No)

# Create a recipe that uses Yeo-Johnson to transform all numeric features
lr_recipe_YJ <- 
  recipe(Attrition ~., data = train) %>%
  step_YeoJohnson(all_numeric())

lr_workflow_YJ <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_YJ)
lr_fit_YJ <- lr_workflow_YJ %>%
  fit(train)
lr_aug_YJ <-
  lr_fit_YJ %>% augment(test)
lr_aug_YJ %>% class_evaluate(truth = Attrition,
                 estimate = .pred_class,.pred_No)

lr_recipe_plain <- recipe(Attrition ~., data = train)

# Bundle the model and recipe
lr_workflow_plain <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_plain)
lr_fit_plain <- lr_workflow_plain %>%
  fit(train)

# Augment the fit workflow
lr_aug_plain <- lr_fit_plain %>%
  augment(test)
lr_aug_plain %>%
  class_evaluate(truth = Attrition,estimate = .pred_class,
                 .pred_No)

lr_recipe_poly <- 
  recipe(Attrition ~., data = train) %>%

# Add a polynomial transformation to all numeric predictors
  step_poly(all_numeric_predictors())

lr_workflow_poly <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_poly)

# Fit workflow to the train data
lr_fit_poly <- lr_workflow_poly %>% fit(train)
lr_aug_poly <- lr_fit_poly %>% augment(test)
lr_aug_poly %>% class_evaluate(truth = Attrition, estimate = .pred_class,.pred_No)

# Add percentile tansformation to all numeric predictors
lr_recipe_perc <- 
  recipe(Attrition ~., data = train) %>%
  step_percentile(all_numeric_predictors())
lr_workflow_perc <-
  workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_perc)
lr_fit_perc <- lr_workflow_perc %>% fit(train)
lr_aug_perc <- lr_fit_perc %>% augment(test)
lr_aug_perc %>% class_evaluate(truth = Attrition,
                 estimate = .pred_class,.pred_No)

lr_recipe <- recipe(Attrition ~., data = train) %>%

# Apply a Yeo-Johnson transformation to all numeric variables
  step_YeoJohnson(all_numeric()) %>%

# Transform all numeric predictors into percentiles
 step_percentile(all_numeric_predictors()) %>%

# Create dummy variables for all nominal predictors
  step_dummy(all_nominal_predictors())

lr_workflow <- workflow() %>% add_model(lr_model) %>% add_recipe(lr_recipe)
lr_fit <- lr_workflow %>% fit(train)
lr_aug <- lr_fit %>% augment(test)
lr_aug %>% class_evaluate(truth = Attrition, estimate = .pred_class,.pred_No)

pc_recipe <- recipe(~., data = attrition_num) %>%

# Remove possible near-zero variance features
  step_nzv(all_numeric()) %>%

# Normalize all numeric data
  step_normalize(all_numeric()) %>%

# Apply PCA
  step_pca(all_numeric())

# Access the names of the output elements by preparing the recipe
pca_output <- prep(pc_recipe)
names(pca_output)

pca_output$steps[[3]]$res$sdev

sdev <- pca_output$steps[[3]]$res$sdev
# Calculate percentage of variance explained
var_explained <- sdev^2/sum(sdev^2)

# Create a tibble with principal components, variance explained and cumulative variance explained
PCA = tibble(PC = 1:length(sdev), var_explained = var_explained, 
       cumulative = cumsum(var_explained)) 

# Use the information in the PCA tibble to create a column plot of variance explained
PCA %>% ggplot(aes(x = PC, y = var_explained)) +
  geom_col(fill = "steelblue") +
  xlab("Principal components") +
  ylab("Variance explained")

attrition %>%
# Select education field
  select(EducationField) %>%

# Print a frequency table of factor values
  table()

recipe <- recipe(~EducationField, data = attrition_train) %>%
# Add a step to the recipe that generates a dummy_hash index for EducationField
  step_dummy_hash(EducationField, prefix = NULL, signed = FALSE, num_terms = 50L)

# Prepare the recipe
object <- recipe %>%
  prep()

# Bake the prepped recipe
baked <- bake(object, new_data = attrition_test)

# Bind the baked recipe table and the EducationField values into one table
bind_cols(attrition_test$EducationField, baked)[1:7,c(1,18:20)]

Hash %>%
select(1:2)

# Convert the baked tibble to a matrix
attrition_hash <- as.matrix(baked)[1:50,] 

# Plot the attrTheTition_hash matrix
plot(attrition_hash, 
     col = c("white","steelblue"), 
     key = NULL,
     border = NA)
lr_model <- logistic_reg()

# Create recipe using the Bayes encoder
lr_recipe_glm <- 
  recipe(Attrition ~ JobRole, data = train) %>%
  step_lencode_bayes(JobRole, outcome = vars(Attrition))

# Bundle with workflow
lr_workflow_glm <-
  workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe_glm)

lr_workflow_glm

# Fit the workflow to the training set
lr_fit_glm <- lr_workflow_glm %>%
  fit(train)

# Augment the fit using the test split
lr_aug_glm <- lr_fit_glm %>%
  augment(test)

# Assess the model
glm_metrics <- lr_aug_glm %>%
  class_evaluate(truth = Attrition,
                 estimate = .pred_class,
                 .pred_Yes)

glm_metrics

model <- c("glm", "glm","bayes","bayes", "mixed", "mixed")

# Bind models by rows
models <- bind_rows(glm_metrics,bayes_metrics, mixed_metrics)%>%
  add_column(model = model)%>%
  select(-.estimator) %>%
  spread(model,.estimate)

models

# Create a parallel coordinates plot
ggparcoord(models,
           columns = 2:4, groupColumn = 1,
           scale="globalminmax",
           showPoints = TRUE) 

lr_model <- logistic_reg()

# Create a recipe to predict Attrition based on all features
lr_recipe <- 
  recipe(Attrition~., 
         data = train) 

# Bundle the model and recipe in a workflow
lr_workflow <- 
  workflow() %>%
  add_model(lr_model) %>%
  add_recipe(lr_recipe)

lr_workflow

lr_fit <- lr_workflow %>%
  fit(test)

# Augment the fit object to assess the model
lr_aug <- lr_fit %>%
  augment(test)

lr_aug %>% class_evaluate(truth = Attrition,
                          estimate = .pred_class,
                          .pred_No)
lr_aug

lr_fit <- lr_workflow %>%
  fit(test)

lr_aug <- lr_fit %>%
  augment(test)

lr_aug %>% class_evaluate(truth = Attrition,
                          estimate = .pred_class,
                          .pred_No)

# Create a variable importance chart
lr_fit %>%
  extract_fit_parsnip() %>%
  vip(aesthetics = list(fill = "steelblue"), num_features = 5)

# Create a recipe that models Attrition using all the predictors
recipe_full <- recipe(Attrition~., data = train)

workflow_full <- workflow() %>%
  add_model(model) %>%
  add_recipe(recipe_full)

# Fit the workflow to the training data
fit_full <- workflow_full %>%
  fit(data = train)

# Use the fit_full object to graph the variable importance of your model. Apply extract_fit_parsnip() function before vip()
fit_full %>% extract_fit_parsnip() %>%
  vip(aesthetics = list(fill = "steelblue"))

# Create an augmented object from the fitted full model
aug_full <-
  fit_full %>%
  augment(test)

# Assess model performance using class_evaluate
aug_full %>% class_evaluate(truth = Attrition, 
               estimate = .pred_class,
               .pred_Yes)

# Create a recipe using the formula syntax that includes only OverTime, DistanceFromHome and NumCompaniesWorked as predictors
recipe_reduced <-
  recipe(Attrition ~ OverTime + DistanceFromHome + NumCompaniesWorked, data = train)

# Bundle the recipe with your model
workflow_reduced <-
  workflow() %>%
  add_model(model) %>%
  add_recipe(recipe_reduced)

# Augment the fitted workflow for analysis using the test data
aug_reduced <-
  fit_reduced %>%
  augment(test)

full_model <- aug_full %>% class_evaluate(truth = Attrition, 
                            estimate = .pred_class, .pred_Yes)

# Evaluate your reduced model for comparison
reduced_model <- aug_reduced %>% class_evaluate(truth = Attrition, 
                                             estimate = .pred_class, .pred_Yes)

bind_cols(full_model,reduced_model) %>%
  select(1,3,6) %>%
  rename(metric = .metric...1, full_model = .estimate...3,
         reduced_model = .estimate...6)

model_lasso_manual <- logistic_reg() %>%

# Set the glmnet engine for your logistic regression model
  set_engine("glmnet") %>%

# Set arguments to run Lasso with a penalty of 0.06
  set_args(mixture = 1, penalty = 0.06)

workflow_lasso_manual <- workflow() %>%
  add_model(model_lasso_manual) %>%
  add_recipe(recipe)

fit_lasso_manual <- workflow_lasso_manual %>% 
  fit(train)

tidy(fit_lasso_manual)

# Set up your model so that the penalty is tuned automatically
model_lasso_tuned <- logistic_reg() %>% set_engine("glmnet") %>%
  set_args(mixture = 1, penalty = tune()) 
workflow_lasso_tuned <- workflow() %>%
  add_model(model_lasso_tuned) %>%
  add_recipe(recipe)

# Configure a penalty grid with 30 levels
penalty_grid <- grid_regular(penalty(range = c(-3, 1)), levels = 30)

tune_output <- tune_grid(workflow_lasso_tuned,
  resamples = vfold_cv(train, v = 5),
  metrics = metric_set(roc_auc),grid = penalty_grid)

autoplot(tune_output)

# Select the optimal penalty for the Lasso
best_penalty <- select_by_one_std_err(tune_output, metric = 'roc_auc', desc(penalty)) 
best_penalty

# Fit a final model using the optimal penalty
final_fit <- finalize_workflow(workflow_lasso_tuned, best_penalty) %>%
  fit(data = train)

final_fit %>% tidy()

final_fit %>% augment(test) %>% class_evaluate(truth = Attrition, 
                                   estimate = .pred_class,
                                   .pred_Yes)

# Transform all character values to factors
attrition <- 
  attrition %>%
  mutate(across(where(is_character), as_factor))
  
# Create train and test splits
set.seed(123)
split <- initial_split(attrition, strata = Attrition)
test <- testing(split)
train <- training(split)

glimpse(train)

recipe <- recipe(Attrition ~ ., data = train) %>%
  update_role(...1, new_role = "ID") %>%

# Normalize all numeric features
  step_normalize(all_numeric_predictors()) %>% 

# Impute missing values using the knn imputation algorithm
  step_impute_knn(all_predictors()) %>%

# Create dummy variables for all nominal predictors
  step_dummy(all_nominal_predictors())
 
recipe

# Set up the penalty for tuning
lr_model <- logistic_reg() %>% set_engine("glmnet") %>%
  set_args(mixture = 1, penalty = tune())

lr_penalty_grid <- grid_regular(penalty(range = c(-3, 1)),levels = 30)

# Bundle your model and recipe in a workflow
lr_workflow <- workflow() %>%
  add_model(lr_model) %>%
  add_recipe(recipe)

lr_workflow

# Set up a tune grid to explore your model performance against roc_auc
lr_tune_output <- tune_grid(lr_workflow, resamples = vfold_cv(train, v = 5),
  metrics = metric_set(roc_auc), grid = lr_penalty_grid)

# Select the best penalty value
best_penalty <- select_by_one_std_err(lr_tune_output, metric = 'roc_auc', desc(penalty)) 

# Fit the final workflow with the best penalty
lr_final_fit<- finalize_workflow(lr_workflow, best_penalty) %>% fit(data = train)

lr_final_fit %>% augment(test) %>% 
class_evaluate(truth = Attrition, estimate = .pred_class, .pred_Yes)

