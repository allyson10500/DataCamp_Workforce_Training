# Filter for Belgium
belgium_consumption <- food_consumption %>%
  filter(country == "Belgium")

# Filter for USA
usa_consumption <- food_consumption %>%
  filter(country == "USA")

# Calculate mean and median consumption in Belgium
mean(belgium_consumption$consumption)
median(belgium_consumption$consumption)

# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
median(usa_consumption$consumption)

food_consumption %>%
  # Filter for rice food category
  filter(food_category == "rice") %>%
  # Create histogram of co2_emission
  ggplot(aes(co2_emission)) +
    geom_histogram()

# Calculate the quartiles of co2_emission
quantile(food_consumption$co2_emission)

# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarize(var_co2 = var(co2_emission),
     sd_co2 = sd(co2_emission))

# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption, aes(co2_emission)) +
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category)

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

emissions_by_country

# Count the deals for each product
amir_deals %>%
  count(amir_deals$product)

# Set random seed to 31
set.seed(31)

# Sample 5 deals without replacement
amir_deals %>%
  sample_n(5)

# Create a histogram of group_size
ggplot(restaurant_groups, aes(x=group_size)) +
  geom_histogram(bins = 5)

# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Set random seed to 334
set.seed(334)

# Set random seed to 10
set.seed(10)

# Simulate a single deal
rbinom(1, 1, .3)

# Probability of closing 3 out of 3 deals
dbinom(3 , 3,.3)

# Expected number won with 30% win rate
won_30pct <- 3 * .3
won_30pct

# Expected number won with 25% win rate
won_25pct <- 3 * .25
won_25pct

# Expected number won with 35% win rate
won_35pct <- 3 * .35
won_35pct

# Histogram of amount with 10 bins
ggplot(amir_deals, aes(amount)) + geom_histogram(bins = 10)

# Probability of deal < 7500
pnorm(7500, 5000, 2000)

# Calculate new average amount
new_mean <- 5000 * 1.2

# Calculate new standard deviation
new_sd <- 2000 * 1.3

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, new_mean, new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(amount)) + geom_histogram(bins = 10)

# Create a histogram of num_users
ggplot(amir_deals, aes(num_users)) + geom_histogram(bins = 10)

# Set seed to 321
set.seed(321)

# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, size = 20) %>% mean())



# Calculate mean of sample_means
mean(sample_means)

# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)

# Probability of 5 responses
dpois(5, 4)

# Probability response takes < 1 hour
pexp(1, rate = .4)

# Create a scatterplot of happiness_score vs. life_exp
ggplot(world_happiness, aes(y = happiness_score, x = life_exp)) + geom_point()

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(y = life_exp, x = gdp_per_cap)) + geom_point()

# Scatterplot of happiness_score vs. gdp_per_cap
ggplot(world_happiness, aes(y = happiness_score, x = gdp_per_cap)) + geom_point()
# Calculate correlation
cor(world_happiness$gdp_per_cap, world_happiness$happiness_score)

# Scatterplot of grams_sugar_per_day and happiness_score
ggplot(world_happiness, aes(x = grams_sugar_per_day, y = happiness_score)) + geom_point()

# Correlation between grams_sugar_per_day and happiness_score
cor(world_happiness$grams_sugar_per_day, world_happiness$happiness_score)
  