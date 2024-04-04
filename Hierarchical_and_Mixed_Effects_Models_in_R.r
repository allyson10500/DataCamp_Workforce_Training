# Plot the data
ggplot(data = student_data, aes(x = mathknow, y = mathgain)) +
    geom_point() +
    geom_smooth(method = "lm")

# Fit a linear model
summary(lm(mathgain ~ mathknow , data =  student_data))

# Summarize the student data at the classroom level
class_data <-
    student_data %>%
    group_by(classid, schoolid) %>%
    summarize(mathgain_class = mean(mathgain),
              mathknow_class = mean(mathknow),
              n_class = n(), .groups = "keep")

# Model the math gain with the student-level data
lm(mathgain ~ mathknow, data = student_data)

# Model the math gain with the classroom-level data
lm(mathgain_class ~ mathknow_class, data = class_data)

# Summarize the data at the school level
school_data <-
    student_data %>%
    group_by(schoolid) %>%
    summarize(mathgain_school = mean(mathgain),
              mathknow_school = mean(mathknow),
              n_school = n(), .groups = 'keep')

# Model the data at the school-level
lm(mathgain_school ~ mathknow_school, data = school_data)

# Summarize school by class (s_by_c)
s_by_c_data <-
    class_data %>%
    group_by(schoolid) %>%
    summarize(mathgain_s_by_c = mean(mathgain_class),
              mathknow_s_by_c = mean(mathknow_class),
              n_s_by_c = n(), .groups = 'keep')

# Model the data at the school-level after summarizing
# students at the class level
lm(mathgain_s_by_c ~ mathknow_s_by_c, data = s_by_c_data)

# Use a linear model to estimate the global intercept
lm(mathgain~1, data = school_3_data)

# Use summarize to calculate the mean
school_3_data %>%
    summarize(mean(mathgain))

# Use a linear model to estimate mathgain in each classroom
lm(mathgain~classid, data = school_3_data)

# Change classid to be a factor
school_3_data <-
    school_3_data %>%
    mutate(classid = factor(classid))

# Use a linear model to estimate mathgain in each classroom
lm(mathgain~classid, data = school_3_data)

# Calculate the mean of mathgain for each class
school_3_data %>%
    group_by(classid) %>%
    summarize(n(), mathgain_class = mean(mathgain))

# Estimate an intercept for each class
lm(mathgain~classid-1, data = school_3_data)

#You observed the relationships between means and intercepts. Notice how the global intercept was the same as the mean across all the data. When examining intercepts, notice how R's default option gave you the difference between each classroom and the reference classroom. With formulas in R, the reference group is always the first group in a factor unless you change this setting. When using the - 1 option, the intercept for each classroom was the classroom's mean. With multiple regression, the first group can have an intercept estimated for each group or contrasts estimated. The additional discrete predictors can only have contrasts estimated.

# Use a linear model to estimate how math kindergarten scores predict math gains later
lm(mathgain~mathkind, data = school_3_data)

# Build a multiple regression
lm(mathgain~classid+mathkind-1, data = school_3_data)

# Build a multiple regression with interaction
lm(mathgain~classid*mathkind-1, data = school_3_data)

# Build a liner model including class as fixed-effect model
lm_out <- lm(mathgain~classid + mathkind, data = student_data)

# Build a mixed-effect model including class id as a random-effect
lmer_out <- lmer(mathgain ~ mathkind + (1 | classid), data = student_data)

# Extract out the slope estimate for mathkind
tidy(lm_out) %>%
    filter(term == "mathkind")
    
tidy(lmer_out) %>%
    filter(term == "mathkind")

# Re-run the models to load their outputs
lm_out <- lm(mathgain ~ classid + mathkind, data = student_data)
lmer_out <- lmer(mathgain ~ mathkind + (1 | classid), data = student_data)

# Add the predictions to the original data
student_data_subset <-
    student_data %>%
    mutate(lm_predict = predict(lm_out),
           lmer_predict = predict(lmer_out)) %>%
    filter(schoolid == "1")

# Plot the predicted values
ggplot(student_data_subset,
       aes(x = mathkind, y = mathgain, color = classid)) +
    geom_point() +
    geom_line(aes(x = mathkind, y = lm_predict)) +
    geom_line(aes(x = mathkind, y = lmer_predict), linetype = 'dashed') +
    xlab("Kindergarten math score") +
    ylab("Math gain later in school") +
    theme_bw() +
    scale_color_manual("Class ID", values = c("red", "blue"))

# Rescale mathkind to make the model more stable
student_data <-
	student_data %>%
    mutate(mathkind_scaled = scale(mathkind))

# Build lmer models
lmer_intercept <- lmer(mathgain ~ mathkind_scaled + (1 | classid),
                       data = student_data)
lmer_slope     <- lmer(mathgain ~ (mathkind_scaled | classid),
                       data = student_data)

# Rescale mathkind to make the model more stable
student_data <-
	student_data %>%
    mutate(mathkind_scaled = scale(mathkind))

# Re-run the models to load their outputs
lmer_intercept <- lmer(mathgain ~ mathkind_scaled + (1 | classid),
                       data = student_data)
lmer_slope     <- lmer(mathgain ~ (mathkind_scaled | classid),
                       data = student_data)

# Add the predictions to the original data
student_data_subset <-
    student_data %>%
    mutate(lmer_intercept = predict(lmer_intercept),
           lmer_slope = predict(lmer_slope)) %>%
    filter(schoolid == "1")

# Plot the predicted values
ggplot(student_data_subset,
       aes(x = mathkind_scaled, y = mathgain, color = classid)) +
    geom_point() +
    geom_line(aes(x = mathkind_scaled, y = lmer_intercept)) +
    geom_line(aes(x = mathkind_scaled, y = lmer_slope), linetype = 'dashed') +
    theme_bw() +
    scale_color_manual("Class ID", values = c("red", "blue"))

# Build the model
lmer_classroom <- lmer(mathgain~mathknow+mathprep+sex+mathkind+ses + (1 | classid), data = student_data)

# Print the model's output
print(lmer_classroom)

# Extract coefficents
lmer_coef <-
    tidy(lmer_classroom, conf.int = TRUE)

# Print coefficents
print(lmer_coef)

# Plot results
lmer_coef %>%
    filter(effect == "fixed" & term != "(Intercept)") %>%
    ggplot(., aes(x = term, y = estimate,
                  ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0, color = 'red') + 
    geom_point() +
    geom_linerange() +
    coord_flip() +
    theme_bw() +
    ylab("Coefficient estimate and 95% CI") +
    xlab("Regression coefficient")

# Build a lmer with State as a random effect
birth_rate_state_model <- lmer(BirthRate ~ (1 | State),
                            data = county_births_data)

# Look at the model's summary and the residuals
summary(birth_rate_state_model)
plot(birth_rate_state_model)

# Plot the predicted values from the model on top of the plot shown during the video
county_births_data$birthPredictState <-
	predict(birth_rate_state_model, county_births_data)
ggplot() + 
    theme_minimal() +
    geom_point(data = county_births_data,
               aes(x = TotalPopulation, y = BirthRate)) + 
    geom_point(data = county_births_data,
               aes(x = TotalPopulation, y = birthPredictState),
               color = 'blue', alpha = 0.5) 

# Include the AverageAgeofMother as a fixed effect within the lmer and state as a random effect
age_mother_model <- lmer(BirthRate ~ AverageAgeofMother + (1 | State), county_births_data)
summary(age_mother_model)

# Include the AverageAgeofMother as fixed-effect and State as a random-effect
model_a <- lmer(BirthRate ~ AverageAgeofMother + (1 | State), county_births_data)
tidy(model_a)

# Include the AverageAgeofMother as fixed-effect and LogTotalPop and State as random-effects
model_b <- lmer(BirthRate ~ AverageAgeofMother + (LogTotalPop | State), county_births_data)
tidy(model_b)

# Include AverageAgeofMother as fixed-effect and LogTotalPop and State as uncorrelated random-effects
model_c <- lmer(BirthRate ~ AverageAgeofMother + (LogTotalPop || State), county_births_data)
# Compare outputs of both models 
summary(model_b)
summary(model_c) #creates an error because A perfect negative correlation exists between the random-effect slope and intercept. Thus, fitting the model without the correlation produces an error message. The random-effect slope and random-effect intercept cannot be estimated independent of each other.

# Construct a lmer() 
out <- lmer(BirthRate ~ AverageAgeofMother + (AverageAgeofMother|State), county_births_data)


# Look at the summary
summary(out)

# Extract the fixed-effect coefficients
fixef(out)

# Extract the random-effect coefficients
ranef(out)

# Estimate the confidence intervals 
confint(out)

# Use the tidy function
tidy(out, conf.int = TRUE)

# Extract out the parameter estimates and confidence intervals
coef_estimates <-
    tidy(out, conf.int = TRUE) %>%
    filter(effect == "fixed")

# Print the new dataframe
print(coef_estimates)

# Plot the results using ggplot2
ggplot(coef_estimates, aes(x = term, y = estimate,
                     ymin = conf.low, ymax = conf.high)) +
    geom_hline( yintercept = 0, color = 'red' ) +
    geom_linerange() + geom_point() + coord_flip() + theme_minimal()

# Plot the change in crime through time by County
plot1 <- 
    ggplot(data = md_crime, 
           aes(x = Year, y = Crime, group = County)) +
    geom_line() + 
    theme_minimal() +
    ylab("Major crimes reported per county")
print(plot1)

# Add the trend line for each county
plot1 + geom_smooth(method = "lm", se = FALSE)

# Fit the model with Year as both a fixed and random-effect
lmer(Crime ~ Year + (1 + Year | County) , data = md_crime)

# Fit the model with Year2 rather than Year
lmer(Crime ~ Year2 + (1 + Year2 | County) , data = md_crime)

# Load lmerTest
library(lmerTest)

# Fit a lmer use lme4
out_lme4 <- 
    lme4::lmer(Crime ~ Year2 + (1 + Year2 | County), 
              data = md_crime)

# Fit a lmer use lmerTest
out_lmerTest <- 
    lmerTest::lmer(Crime ~ Year2 + (1 + Year2 | County), 
                  data = md_crime)

# Look at the summaries
summary(out_lme4)
summary(out_lmerTest)
#lmerTest includes p-values as part of the outputs. Using this extra package helps people to estimate p-values when they need or want to with lmer models.

# Build the Null model with only County as a random-effect
null_model <- lmer(Crime ~ (1 | County) , data = md_crime)

# Build the Year2 model with Year2 as a fixed and random slope and County as the random-effect
year_model <- lmer(Crime ~ Year2 + (1 + Year2 | County) , data = md_crime)

# Compare null_model and year_model using an anova
anova(null_model, year_model)

# Fit a glm using data in a long format
fit_long <- glm(mortality ~ dose, data = df_long, 
                family = "binomial")
summary(fit_long)

# Fit a glm using data in a short format with two columns
fit_short <- glm(cbind(mortality, survival) ~ dose, 
                data = df_short, family = "binomial")
summary(fit_short)

# Fit a glm using data in a short format with weights
fit_short_p <- glm(mortalityP  ~ dose , data = df_short, 
                   weights = nReps , family = "binomial")
summary(fit_short_p)

# Fit the linear model
summary(lm(y~x))

# Fit the generalized linear model
summary(glm(y~x, family = "poisson"))

# Plot the data using jittered points and the default stat_smooth
ggplot(data = df_long, aes(x = dose, y = mortality)) + 
    geom_jitter(height = 0.05, width = 0.1) +
    stat_smooth(fill = 'pink', color = 'red') 

# Plot the data using jittered points and the the glm stat_smooth
ggplot(data = df_long, aes(x = dose, y = mortality)) + 
    geom_jitter(height = 0.05, width = 0.1) +
    stat_smooth(method = "glm",  
                method.args = list(family = "binomial"))

# Fit glm_out and look at its coefficient estimates 
glm_out <- glm(mortality ~ dose + replicate - 1, 
               family = "binomial", data = df)
coef(glm_out)

# Load lmerTest
library(lmerTest)

# Fit glmer_out and look at its coefficient estimates 
glmer_out <- glmer(mortality ~ dose + (1 |replicate), family = "binomial", data = df )
coef(glmer_out)

# Load lmerTest
library(lmerTest)

# Fit the model and look at its summary 
model_out <- glmer(cbind(Purchases, Pass) ~ friend + ranking + (1|city), family = "binomial", data = all_data)
summary(model_out) 

# Run the code to see how to calculate odds ratios
summary(model_out)
exp(fixef(model_out))
exp(confint(model_out))

# Create the tidied output
tidy(model_out, conf.int = TRUE, exponentiate = TRUE)

# Load lmerTest
library(lmerTest)

# Fit a Poisson glmer
glmer_out <- glmer(clicks ~ webpage + (1|group), family = "poisson", data = user_groups)

# Examine output with summary()
summary(glmer_out)

# Age goes before year
model_out <- glmer(count ~ age + year + (year|county), family = "poisson", data = il_data)
summary(model_out)

# Extract out fixed effects
fixef(model_out)
# Extract out random effects 
ranef(model_out)

# Run code to see one method for plotting the data
ggplot(data = il_data_2, 
       aes(x = year, y = count, group = county)) +
    geom_line() +
    facet_grid(age ~ . ) +
    stat_smooth(method = "glm",
                method.args = list(family = "poisson"), 
                se = FALSE,
                alpha = 0.5) +
    theme_minimal()

# Set the seed to be 345659
set.seed(345659)

# Model 10 individuals 
n_ind <- 10

# simulate before with mean of 0 and sd of 0.5
before <- rnorm(n = n_ind, mean = 0, sd = 0.5)
# simulate after with mean effect of 4.5 and standard devation of 5
after  <- before + rnorm(n = n_ind, mean = 4.5, sd = 5)

# Run a standard, non-paired t-test
t.test(x = before, y = after, paired = FALSE)

# Run a standard, paired t-test
t.test(x = before, y =after, paired = TRUE)

# Create the data.frame, using the variables from the previous exercise. 
# y is the joined vectors before and after.
# trial is the repeated names before and after, each one repeated n_ind
# ind is the letter of the individual, repeated 2 times (because there were two observations)
dat <- data.frame(y = c(before, after), 
                  trial = rep(c("before", "after"), each = n_ind),
                  ind = rep(letters[1:n_ind], times = 2))

# Run a standard, paired t-test
t.test(before, after, paired = TRUE)

# Run a lmer and save it as lmer_out
lmer_out <- lmer(y ~ trial + (1|ind), data = dat)

# Look at the summary() of lmer_out
summary(lmer_out)

ggplot(data = sleep, aes(x = group, y = extra)) +
    geom_point()

# Replace geom_point with geom_line
ggplot(data = sleep, aes(x = group, y = extra, group = ID)) +
    geom_line()

# Add an x and y label to the plot and change the theme to be "minimal"
ggplot(sleep, aes(x = group, y = extra, group = ID)) +
  geom_line() +
  xlab(label = "Drug") +
  ylab(label = "Extra sleep") + 
  theme_minimal()

# Build a simple linear model 
lm(extra ~ group + ID, data = sleep)

# Build a mixed-effects regression
lmer_out <- lmer(extra ~ group + (1|ID), sleep)

# Run an anova() on lmer_out
anova(lmer_out)

# Look at the summary() of lmer_out
summary(lmer_out)

# Load the tidyr package
library(tidyr)

# Make the data wider
sleep_wide <- 
    pivot_wider(sleep, 
                names_from = group, values_from = extra)

# Calculate the difference 
sleep_wide$diff <- sleep_wide$`2` - sleep_wide$`1` 

# Use the data sleep_wide and diff for the aesthetic x  
ggplot(sleep_wide, aes(x = diff)) + 
  geom_histogram() +
  ylab(label = "Count") +
  xlab(label = "Extra sleep from drug 2") +
  theme_bw()

# Plot the TotalIncidents of hate crimes in NY by Year, grouped by County
ggplot(hate, aes(Year, TotalIncidents, group = County))+
  geom_line()+
  # Add a Poisson trend line for each county
  geom_smooth(method = "glm", method.args = c("poisson"), se = FALSE)

# Model TotalIncidents predicted by Year and County
# Use a Poisson regression with the hate data.frame 
glm( TotalIncidents ~ Year + County ,family = "poisson", data = hate)

# Build a glmer with County as a random-effect intercept and Year as both a fixed- and random-effect slope
glmer( TotalIncidents ~ Year + (Year|County) ,family = "poisson", data = hate)

# Create a new column, Year2 that starts with the min of Year
hate$Year2 <- hate$Year - min(hate$Year)

# Build a glmer with County as a random-effect intercept and Year2 as both a fixed- and random-effect slope
glmer_out <- glmer( TotalIncidents ~ Year2 + (Year2|County) ,family = "poisson", data = hate)

# Examine the summary of glmer_out
summary(glmer_out)

# Extract out the fixed-effect slope for Year2
Year2_slope <- fixef(glmer_out)['Year2']

# Extract out the random-effect slopes for county
county_slope <- ranef(glmer_out)$County

# Create a new column for the slope
county_slope$slope <- county_slope$Year2 + Year2_slope

# Use the row names to create a county name column
county_slope$county <- rownames(county_slope)

# Create an ordered county-level factor based upon slope values
county_slope$county_plot <- factor(county_slope$county, 
                                   levels = county_slope$county[order(county_slope$slope)])

# Now plot the results using ggplot2
ggplot(data = county_slope, aes(x = county_plot, y = slope)) + 
    geom_point() +
    coord_flip() + 
    theme_bw() + 
    ylab("Change in hate crimes per year")  +
    xlab("County")

