# View the whole population dataset
View(spotify_population)

# Sample 1000 rows from spotify_population
spotify_sample <- slice_sample(spotify_population, n = 1000)

# See the result
spotify_sample

# From previous step
spotify_sample <- spotify_population %>% 
  slice_sample(n = 1000)

# Calculate the mean duration in mins from spotify_population
mean_dur_pop <- summarize(spotify_population, mean(duration_minutes))


# Calculate the mean duration in mins from spotify_sample
mean_dur_samp <- summarize(spotify_sample, mean(duration_minutes))


# See the results
mean_dur_pop
mean_dur_samp

# Get the loudness column of spotify_population
loudness_pop <- spotify_population$loudness

# Sample 100 values of loudness_pop
loudness_samp <- sample(loudness_pop, size = 100)

# See the results
loudness_samp

# From previous step
loudness_pop <- spotify_population$loudness
loudness_samp <- sample(loudness_pop, size = 100)

# Calculate the standard deviation of loudness_pop
sd_loudness_pop <- sd(loudness_pop)

# Calculate the standard deviation of loudness_samp
sd_loudness_samp <- sd(loudness_samp)

# See the results
sd_loudness_pop
sd_loudness_samp

# Visualize the distribution of acousticness as a histogram with a binwidth of 0.01
ggplot(spotify_population, aes(acousticness))+
    geom_histogram(binwidth = 0.01)

# Update the histogram to use spotify_mysterious_sample with x-axis limits from 0 to 1
ggplot(spotify_mysterious_sample, aes(acousticness)) +
  geom_histogram(binwidth = 0.01) + 
  xlim(0,1)

# Visualize the distribution of duration_minutes as a histogram with a binwidth of 0.5
ggplot(spotify_population, aes(duration_minutes))+
    geom_histogram(binwidth = 0.5)

# Update the histogram to use spotify_mysterious_sample2 with x-axis limits from 0 to 15
ggplot(spotify_mysterious_sample2, aes(duration_minutes)) +
  geom_histogram(binwidth = 0.01)+ 
  xlim(0,15)

# Generate random numbers from ...
randoms <- data.frame(
  # a uniform distribution from -3 to 3
  uniform =runif(n_numbers, -3, 3),
  # a normal distribution with mean 5 and sd 2
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# From previous step
randoms <- data.frame(
  uniform = runif(n_numbers, min = -3, max = 3),
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# Plot a histogram of uniform values, binwidth 0.25
ggplot(randoms, aes(uniform)) +
    geom_histogram(binwidth = 0.25)

# From previous step
randoms <- data.frame(
  uniform = runif(n_numbers, min = -3, max = 3),
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

# Plot a histogram of normal values, binwidth 0.5
ggplot(randoms, aes(normal)) +
    geom_histogram(binwidth = 0.5)

# View the attrition_pop dataset
View(attrition_pop)

# Set the seed
set.seed(100)

attrition_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column() %>% 
  # Get 200 rows using simple random sampling
  slice_sample(n=200)

# View the attrition_samp dataset
View(attrition_samp)

# Set the sample size to 200
sample_size <- 200

# Get the population size from attrition_pop
pop_size <- nrow(attrition_pop)

# Calculate the interval
interval <- pop_size %/% sample_size

# From previous step
sample_size <- 200
pop_size <- nrow(attrition_pop)
interval <- pop_size %/% sample_size

# Get row indexes for the sample
row_indexes <- seq_len(sample_size) * interval

attrition_sys_samp <- attrition_pop %>% 
  # Add a row ID column
  rowid_to_column() %>% 
  # Get 200 rows using systematic sampling
  slice(row_indexes)

# See the result
View(attrition_sys_samp)

# Add a row ID column to attrition_pop
attrition_pop_id <- attrition_pop %>% 
  rowid_to_column()

# Using attrition_pop_id, plot YearsAtCompany vs. rowid
ggplot(attrition_pop_id, aes(x = rowid, y = YearsAtCompany)) +
  # Make it a scatter plot
  geom_point() +
  # Add a smooth trend line
  geom_smooth()

# Shuffle the rows of attrition_pop then add row IDs
attrition_shuffled <- attrition_pop %>% 
slice_sample(prop = 1) %>%
rowid_to_column()


# Using attrition_shuffled, plot YearsAtCompany vs. rowid
# Add points and a smooth trend line
ggplot(attrition_shuffled, aes(x = rowid, y = YearsAtCompany)) + 
geom_point() + 
geom_smooth()

education_counts_pop <- attrition_pop %>% 
  # Count the employees by Education level, sorting by n
  count(Education, sort = TRUE) %>% 
  # Add a percent column
  mutate(percent = 100 * n / sum(n))

# See the results
education_counts_pop

# From previous step
attrition_pop %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))

# Use proportional stratified sampling to get 40% of each Education group
attrition_strat <- attrition_pop %>%
group_by(Education) %>%
slice_sample(prop = .4) %>%
ungroup()

# See the result
attrition_strat

# From previous steps
attrition_pop %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))
attrition_strat <- attrition_pop %>% 
  group_by(Education) %>% 
  slice_sample(prop = 0.4) %>% 
  ungroup()

# Get the counts and percents from attrition_strat
education_counts_strat <- attrition_strat %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))

# See the results
education_counts_strat

# Use equal counts stratified sampling to get 30 employees from each Education group
attrition_eq <- attrition_pop %>%
group_by(Education) %>%
slice_sample(n = 30) %>%
ungroup()

# See the results
attrition_eq

# From previous step
attrition_eq <- attrition_pop %>%
  group_by(Education) %>% 
  slice_sample(n = 30) %>%
  ungroup()

# Get the counts and percents from attrition_eq
education_counts_eq <- attrition_eq %>% 
  count(Education, sort = TRUE) %>% 
  mutate(percent = 100 * n / sum(n))

# See the results
education_counts_eq

# Using attrition_pop, plot YearsAtCompany as a histogram with binwidth 1
ggplot(attrition_pop, aes(x = YearsAtCompany)) + 
geom_histogram( binwidth = 1)

# Sample 400 employees weighted by YearsAtCompany
attrition_weight <- attrition_pop %>%
slice_sample(n = 400, weight_by = YearsAtCompany)

# See the results
attrition_weight

# From previous step
attrition_weight <- attrition_pop %>% 
  slice_sample(n = 400, weight_by = YearsAtCompany)

# Using attrition_weight, plot YearsAtCompany as a histogram with binwidth 1
ggplot(attrition_weight, aes(YearsAtCompany)) + geom_histogram(binwidth = 1)

# Get unique JobRole values
job_roles_pop <- unique(attrition_pop$JobRole)

# Randomly sample four JobRole values
job_roles_samp <- sample(job_roles_pop, size = 4)

# See the result
job_roles_samp

# From previous step
job_roles_pop <- unique(attrition_pop$JobRole)
job_roles_samp <- sample(job_roles_pop, size = 4)

# Filter for rows where JobRole is in job_roles_samp
attrition_filtered <- attrition_pop %>% 
  filter(JobRole %in% job_roles_samp)

# Randomly sample 10 employees from each sampled job role
attrition_clus <- attrition_filtered %>% 
  group_by(JobRole) %>% 
  slice_sample(n = 10)

# See the result
attrition_clus

# Perform simple random sampling to get 0.25 of the population
attrition_srs <- attrition_pop %>% slice_sample(prop = 0.25)

# Perform stratified sampling to get 0.25 of each relationship group
attrition_strat <- attrition_pop %>% 
  group_by(RelationshipSatisfaction) %>% 
  slice_sample(prop = 0.25) %>% 
  ungroup()

# Get unique values of RelationshipSatisfaction
satisfaction_unique <- unique(attrition_pop$RelationshipSatisfaction)

# Randomly sample for 2 of the unique satisfaction values
satisfaction_samp <- sample(satisfaction_unique, size = 2)

# Perform cluster sampling on the selected group getting 0.25 of the population
attrition_clust <- attrition_pop %>%
  filter(RelationshipSatisfaction %in% satisfaction_samp) %>% 
  group_by(RelationshipSatisfaction) %>% 
  slice_sample(n = nrow(attrition_pop) / 4) %>% 
  ungroup()

# Use the whole population dataset 
mean_attrition_pop <- attrition_pop %>% 
  # Group by relationship satisfaction level
  group_by(RelationshipSatisfaction) %>% 
  # Calculate the proportion of employee attrition
  summarize(mean_attition = mean(Attrition == "Yes"))

# See the result
mean_attrition_pop

# Calculate the same thing for the simple random sample 
mean_attrition_srs <- attrition_srs %>% 
  # Group by relationship satisfaction level
  group_by(RelationshipSatisfaction) %>% 
  # Calculate the proportion of employee attrition
  summarize(mean_attition = mean(Attrition == "Yes"))

# See the result
mean_attrition_srs

# Calculate the same thing for the stratified sample 
mean_attrition_strat <- attrition_strat %>% 
  # Group by relationship satisfaction level
  group_by(RelationshipSatisfaction) %>% 
  # Calculate the proportion of employee attrition
  summarize(mean_attition = mean(Attrition == "Yes"))
# See the result
mean_attrition_strat

# Calculate the same thing for the cluster sample 
mean_attrition_clust <- attrition_clust %>% 
  # Group by relationship satisfaction level
  group_by(RelationshipSatisfaction) %>% 
  # Calculate the proportion of employee attrition
  summarize(mean_attition = mean(Attrition == "Yes"))
# See the result
mean_attrition_clust

# Generate a simple random sample of 10 rows 
attrition_srs10 <- attrition_pop %>% slice_sample(n= 10)

# Calculate the proportion of employee attrition in the sample
mean_attrition_srs10 <- attrition_srs10 %>%
summarize(mean_attrition = mean(Attrition == "Yes"))

# Calculate the relative error percentage
rel_error_pct10 <- 100*abs(mean_attrition_pop - mean_attrition_srs10)/mean_attrition_pop

# See the result
rel_error_pct10

# Calculate the relative error percentage again with a sample of 100 rows
# Generate a simple random sample of 100 rows 
attrition_srs100 <- attrition_pop %>% slice_sample(n= 100)

# Calculate the proportion of employee attrition in the sample
mean_attrition_srs100 <- attrition_srs100 %>%
summarize(mean_attrition = mean(Attrition == "Yes"))

rel_error_pct100 <- 100*abs(mean_attrition_pop - mean_attrition_srs100)/mean_attrition_pop

# See the result
rel_error_pct100

# Replicate this code 500 times
mean_attritions <- replicate(n=500,
  attrition_pop %>% 
    slice_sample(n = 20) %>% 
    summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
    pull(mean_attrition) 
)

# See the result
head(mean_attritions)

# From previous step
mean_attritions <- replicate(
  n = 500,
  attrition_pop %>% 
    slice_sample(n = 20) %>% 
    summarize(mean_attrition = mean(Attrition == "Yes")) %>% 
    pull(mean_attrition)
)

# Store mean_attritions in a tibble in a column named sample_mean
sample_means <- tibble(sample_mean = mean_attritions)

# Plot a histogram of the `sample_mean` column, binwidth 0.05
ggplot(sample_means, aes(x = sample_mean)) + geom_histogram(binwidth = 0.05)

# Expand a grid representing 5 8-sided dice
dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
) 

# See the result
dice

dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
) %>% 
  # Add a column of mean rolls
  mutate(mean_roll = (die1 + die2 +die3 + die4 +die5)/5)

# From previous step
dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8
) %>% 
  mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)

# Using dice, draw a bar plot of mean_roll as a factor
ggplot(dice, aes(factor(mean_roll))) + geom_bar()

# Sample one to eight, five times, with replacement
five_rolls <- five_rolls <- sample(1:8, size = 5, replace = TRUE)

# Calculate the mean of five_rolls
mean(five_rolls)

# Replicate the sampling code 1000 times
sample_means_1000 <- replicate(n=1000,

  {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)

# See the result
sample_means_1000

# From previous step
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)

# Wrap sample_means_1000 in the sample_mean column of a tibble
sample_means <- tibble(sample_mean = sample_means_1000)

# See the result
sample_means

# From previous steps
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)
sample_means <- tibble(
  sample_mean = sample_means_1000
)

# Using sample_means, draw a bar plot of sample_mean as a factor
ggplot(sample_means, aes(factor(sample_mean))) + geom_bar()

# Calculate the mean across replicates of the mean attritions in sampling_distribution_5
mean_of_means_5 <- sampling_distribution_5 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))

# Do the same for sampling_distribution_50
mean_of_means_50 <- sampling_distribution_50 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))

# ... and for sampling_distribution_500
mean_of_means_500 <- sampling_distribution_500 %>%
  summarize(mean_mean_attrition = mean(mean_attrition))


# See the results
mean_of_means_5
mean_of_means_50
mean_of_means_500

# Calculate the standard deviation across replicates of the mean attritions in sampling_distribution_5
sd_of_means_5 <- sampling_distribution_5 %>%
  summarize(sd_mean_attrition = sd(mean_attrition))

# Do the same for sampling_distribution_50
sd_of_means_50 <- sampling_distribution_50 %>%
  summarize(sd_mean_attrition = sd(mean_attrition))


# ... and for sampling_distribution_500
sd_of_means_500 <- sampling_distribution_500 %>%
  summarize(sd_mean_attrition = sd(mean_attrition))

# See the results
sd_of_means_5
sd_of_means_50
sd_of_means_500

# Generate 1 bootstrap resample
spotify_1_resample <- spotify_sample %>%  slice_sample(prop = 1, replace = TRUE)


# See the result
spotify_1_resample

# From previous step
spotify_1_resample <- spotify_sample %>% 
  slice_sample(prop = 1, replace = TRUE)

# Calculate mean danceability of resample
mean_danceability_1 <- spotify_1_resample %>% 
  summarize(mean_danceability = mean(danceability)) %>% 
  pull(mean_danceability)

# See the result
mean_danceability_1

# Replicate this 1000 times
mean_danceability_1000 <- replicate(
  n=1000,
  expr = {
    spotify_1_resample <- spotify_sample %>% 
      slice_sample(prop = 1, replace = TRUE)
    spotify_1_resample %>% 
      summarize(mean_danceability = mean(danceability)) %>% 
      pull(mean_danceability)})
      
# See the result
head(mean_danceability_1000)

# From previous steps
mean_danceability_1000 <- load_step_4()

# Store the resamples in a tibble
bootstrap_distn <- tibble(
  resample_mean = mean_danceability_1000)

# Draw a histogram of the resample means with binwidth 0.002
ggplot(bootstrap_distn, aes(resample_mean)) + geom_histogram(binwidth = 0.002)

# Generate a sampling distribution
mean_popularity_2000_samp <- replicate(
  # Use 2000 replicates
  n=2000,
  expr = {
    # Start with the population
    spotify_population %>% 
      # Sample 500 rows without replacement
      slice_sample(n = 500) %>% 
      # Calculate the mean popularity as mean_popularity
      summarize(mean_popularity = mean(popularity)) %>% 
      # Pull out the mean popularity
      pull(mean_popularity)})

# See the result
head(mean_popularity_2000_samp)

# Generate a bootstrap distribution
mean_popularity_2000_boot <- replicate(
  # Use 2000 replicates
  n = 2000,
  expr = {
    # Start with the sample
    spotify_sample %>% 
      # Sample same number of rows with replacement
      slice_sample(prop = 1, replace = TRUE) %>% 
      # Calculate the mean popularity
      summarize(mean_popularity = mean(popularity)) %>% 
      # Pull out the mean popularity
      pull(mean_popularity)
  }
)

# See the result
mean_popularity_2000_boot

# Calculate the true population mean popularity
pop_mean <- spotify_population %>% 
  summarize(mean(popularity))

# Calculate the original sample mean popularity
samp_mean <- spotify_sample %>% 
  summarize(mean(popularity))

# Calculate the sampling dist'n estimate of mean popularity
samp_distn_mean <- sampling_distribution %>%
  summarize(mean(sample_mean))


# Calculate the bootstrap dist'n estimate of mean popularity
boot_distn_mean <- bootstrap_distribution %>%
summarize(mean(resample_mean))

# See the results
c(pop = pop_mean, samp = samp_mean, sam_distn = samp_distn_mean, boot_distn = boot_distn_mean)

# Calculate the true popluation std dev popularity
pop_sd <- spotify_population %>%
summarize(sd(popularity))


# Calculate the true sample std dev popularity
samp_sd <- spotify_sample %>%
summarize(sd(popularity))


# Calculate the sampling dist'n estimate of std dev popularity
samp_distn_sd <- sampling_distribution %>%
summarize(sd(sample_mean))*sqrt(500)


# Calculate the bootstrap dist'n estimate of std dev popularity
boot_distn_sd <- bootstrap_distribution %>%
summarize(sd(resample_mean))*sqrt(500)


# See the results
c(pop = pop_sd, samp = samp_sd, sam_distn = samp_distn_sd, boot_distn = boot_distn_sd)

# Generate a 95% confidence interval using the quantile method
conf_int_quantile <- bootstrap_distribution %>% 
  summarize(lower = quantile(resample_mean, 0.025), 
  upper = quantile(resample_mean, 0.975))

# See the result
conf_int_quantile

# Generate a 95% confidence interval using the std error method
conf_int_std_error <- bootstrap_distribution %>% 
  summarize(point_estimate = mean(resample_mean),
  std_error = sd(resample_mean),
  lower = qnorm(0.025, point_estimate, std_error),
  upper = qnorm(0.975, point_estimate, std_error))

# See the result
conf_int_std_error

