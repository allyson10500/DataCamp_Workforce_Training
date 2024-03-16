# View the late_shipments dataset
View(late_shipments)

# Calculate the proportion of late shipments
late_prop_samp <- late_shipments %>% 
  summarize(prop_late_shipments = mean(late == "Yes")) %>% 
  pull(prop_late_shipments)

# See the results
late_prop_samp

# Hypothesize that the proportion is 6%
late_prop_hyp <- 0.06

# Calculate the standard error
std_error <- late_shipments_boot_distn %>% 
  summarize(sd_late_prop = sd(late_prop)) %>% 
  pull(sd_late_prop)

# Find z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp) / std_error

# See the results
z_score

# Calculate the z-score of late_prop_samp
z_score <- (late_prop_samp - late_prop_hyp)/std_error

# Calculate the p-value
p_value <- pnorm(z_score, lower.tail = FALSE)
                 
# See the result
p_value   

# Calculate 95% confidence interval using quantile method
conf_int_quantile <- late_shipments_boot_distn %>%
  summarize(lower = quantile(prop_late_shipments, 0.025),
  upper = quantile(prop_late_shipments, 0.975))

# See the result
conf_int_quantile

# Calculate the numerator of the test statistic
numerator <- xbar_no - xbar_yes

# Calculate the denominator of the test statistic
denominator <- sqrt(s_yes^2 / n_yes + s_no^2 / n_no)

# Calculate the test statistic
t_stat <- numerator/denominator

# See the result
t_stat

# Calculate the degrees of freedom
degrees_of_freedom <- n_no + n_yes -2

# Calculate the p-value from the test stat
p_value <- pt(t_stat, df = degrees_of_freedom, lower.tail = TRUE)

# See the result
p_value

# View the dem_votes_potus_12_16 dataset
View(dem_votes_potus_12_16)

# Calculate the differences from 2012 to 2016
sample_dem_data <- dem_votes_potus_12_16 %>%
mutate(diff = dem_percent_12 - dem_percent_16)

# See the result
sample_dem_data

# From previous step
sample_dem_data <- dem_votes_potus_12_16 %>% 
  mutate(diff = dem_percent_12 - dem_percent_16)

# Find mean and standard deviation of differences
diff_stats <- sample_dem_data %>% summarize(xbar_diff = mean(diff), 
s_diff = sd(diff))

# See the result
diff_stats

# Using sample_dem_data, plot diff as a histogram
ggplot(sample_dem_data, aes(x = diff)) + geom_histogram(binwidth = 1)

# Conduct a t-test on diff
test_results <- t.test(sample_dem_data$diff,
alternative = "greater",
mu = 0)

# See the results
test_results

# Conduct a paired t-test on dem_percent_12 and dem_percent_16
test_results <- t.test(sample_dem_data$dem_percent_12,
sample_dem_data$dem_percent_16,
alternative = "greater",
mu = 0,
paired = TRUE)

# See the results
test_results

# Using late_shipments, group by shipment mode, and calculate the mean and std dev of pack price
late_shipments %>% group_by(shipment_mode) %>%
summarize(
    xbar_pack_price = mean(pack_price),
    s_pack_price = sd(pack_price)
)

# Using late_shipments, plot pack_price vs. shipment_mode
# as a box plot with flipped x and y coordinates
ggplot(late_shipments, aes(shipment_mode, pack_price)) + geom_boxplot() +
coord_flip()

# Run a linear regression of pack price vs. shipment mode 
mdl_pack_price_vs_shipment_mode <- lm(pack_price ~ shipment_mode, data= late_shipments)

# See the results
summary(mdl_pack_price_vs_shipment_mode)

# From previous step
mdl_pack_price_vs_shipment_mode <- lm(pack_price ~ shipment_mode, data = late_shipments)

# Perform ANOVA on the regression model
anova(mdl_pack_price_vs_shipment_mode)

# Perform pairwise t-tests on pack price, grouped by shipment mode, no p-value adjustment
test_results <- pairwise.t.test(late_shipments$pack_price, late_shipments$shipment_mode, p.adjust.method = "none")

# See the results
test_results

# Modify the pairwise t-tests to use Bonferroni p-value adjustment
test_results <- pairwise.t.test(
  late_shipments$pack_price,
  late_shipments$shipment_mode,
  p.adjust.method = "bonferroni"
)

# See the results
test_results

# Hypothesize that the proportion of late shipments is 6%
p_0 <- 0.06

# Calculate the sample proportion of late shipments
p_hat <- late_shipments %>%
summarize(prop_late = mean(late == "Yes")) %>%
pull(prop_late)

# Calculate the sample size
n <- nrow(late_shipments)

# From previous step
p_0 <- 0.06
p_hat <- late_shipments %>%
  summarize(prop_late = mean(late == "Yes")) %>%
  pull(prop_late)
n <- nrow(late_shipments)

# Calculate the numerator of the test statistic
numerator <- p_hat - p_0

# Calculate the denominator of the test statistic
denominator <- sqrt(p_0 * (1-p_0) / n)

# Calculate the test statistic
z_score <- numerator/denominator

# See the result
z_score

# From previous step
p_0 <- 0.06
p_hat <- late_shipments %>%
  summarize(prop_late = mean(late == "Yes")) %>%
  pull(prop_late)
n <- nrow(late_shipments)
numerator <- p_hat - p_0
denominator <- sqrt(p_0 * (1 - p_0) / n)
z_score <- numerator / denominator

# Calculate the p-value from the z-score
p_value <- pnorm(z_score, lower.tail = FALSE)

# See the result
p_value

# See the sample variables
print(p_hats)
print(ns)

# Calculate the pooled estimate of the population proportion
p_hat <- weighted.mean(p_hats, ns)
# Or explicitly using 
# (p_hats["reasonable"] * ns["reasonable"] + p_hats["expensive"] * ns["expensive"]) / (ns["reasonable"] + ns["expensive"])

# See the result
p_hat

# From previous step
p_hat <- weighted.mean(p_hats, ns)

# Calculate sample prop'n times one minus sample prop'n
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)

# Divide this by the sample sizes
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns

# Calculate std. error
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))

# See the result
std_error

# From previous steps
p_hat <- weighted.mean(p_hats, ns)
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))

# Calculate the z-score
z_score <- (p_hats["expensive"] - p_hats["reasonable"]) / std_error

# See the result
z_score

# From previous steps
p_hat <- weighted.mean(p_hats, ns)
p_hat_times_not_p_hat <- p_hat * (1 - p_hat)
p_hat_times_not_p_hat_over_ns <- p_hat_times_not_p_hat / ns
std_error <- sqrt(sum(p_hat_times_not_p_hat_over_ns))
z_score <- (p_hats["expensive"] - p_hats["reasonable"]) / std_error

# Calculate the p-value from the z-score
p_value <- pnorm(z_score, lower.tail = FALSE)

# See the result
p_value

# Perform a proportion test appropriate to the hypotheses 
test_results <- late_shipments %>%
prop_test(late~freight_cost_group,
order = c("expensive", "reasonable"),
success = "Yes",
alternative = "greater",
correct = FALSE)

# See the results
test_results

# Plot vendor_inco_term filled by freight_cost_group.
# Make it a proportional stacked bar plot.
ggplot(late_shipments, aes(vendor_inco_term, fill = freight_cost_group)) + geom_bar(position = "fill")

# Perform a chi-square test of independence on freight_cost_group and vendor_inco_term
test_results <- late_shipments %>%
chisq_test(freight_cost_group ~ vendor_inco_term)

# See the results
test_results

# Using late_shipments, count the vendor incoterms
vendor_inco_term_counts <- late_shipments %>%
count(vendor_inco_term)

# Get the number of rows in the whole sample
n_total <- nrow(late_shipments)

hypothesized <- tribble(
  ~ vendor_inco_term, ~ prop,
  "EXW", 0.75,
  "CIP", 0.05,
  "DDP", 0.1,
  "FCA", 0.1
) %>%
  # Add a column of hypothesized counts for the incoterms
  mutate(n = prop * n_total)

# See the results
hypothesized

# From previous step
vendor_inco_term_counts <- late_shipments %>% 
  count(vendor_inco_term)
n_total <- nrow(late_shipments)
hypothesized <- tribble(
  ~ vendor_inco_term, ~ prop,
  "EXW", 0.75,
  "CIP", 0.05,
  "DDP", 0.1,
  "FCA", 0.1
) %>%
  mutate(n = prop * n_total)

# Using vendor_inco_term_counts, plot n vs. vendor_inco_term 
ggplot(vendor_inco_term_counts, aes(vendor_inco_term, n)) +
  # Make it a (precalculated) bar plot
  geom_col() +
  # Add points from hypothesized 
  geom_point(data = hypothesized)

hypothesized_props <- c(
  EXW = 0.75, CIP = 0.05, DDP = 0.1, FCA = 0.1
)

# Run chi-square goodness of fit test on vendor_inco_term
test_results <- late_shipments %>%
chisq_test(response = vendor_inco_term,
p = hypothesized_props)

# See the results
test_results

# Get counts by freight_cost_group
counts <- late_shipments %>% count(freight_cost_group)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 30)

# Get counts by late
counts <- late_shipments %>% count(late)


# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 10)

# Count the values of vendor_inco_term and freight_cost_group
counts <- late_shipments %>% count(vendor_inco_term, freight_cost_group)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 5)

# Count the values of shipment_mode
counts <- late_shipments %>% count(shipment_mode)

# See the result
counts

# Inspect whether the counts are big enough
all(counts$n >= 30)

# Perform a proportion test appropriate to the hypotheses 
test_results <- late_shipments %>% 
  prop_test(
    late ~ freight_cost_group,
    order = c("expensive", "reasonable"),
    success = "Yes",
    alternative = "greater",
    correct = FALSE
  )

# See the results
test_results

# Specify that we are interested in late proportions across freight_cost_groups, where "Yes" denotes success
specified <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  )

# See the result
specified

# Extend the pipeline to declare a null hypothesis that the variables are independent
hypothesized <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
   hypothesize(null = "independence")

# See the result
hypothesized

# Extend the pipeline to generate 2000 permutations
generated <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute")

# See the result
generated

# Extend the pipeline to calculate the difference in proportions (expensive minus reasonable)
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props" ,
    order = c("expensive", "reasonable")
  )

# See the result
null_distn

# Visualize the null distribution
visualize(null_distn)

null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# Copy, paste, and modify the pipeline to get the observed statistic
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  #hypothesize(null = "independence") %>% 
  #generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# See the result
obs_stat

# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# Visualize the null dist'n, adding a vertical line at the observed statistic
visualize(null_distn) + geom_vline(
aes(xintercept = stat),
data = obs_stat,
color = "red")

# From previous steps
null_distn <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 2000, type = "permute") %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )
obs_stat <- late_shipments %>% 
  specify(
    late ~ freight_cost_group, 
    success = "Yes"
  ) %>% 
  calculate(
    stat = "diff in props", 
    order = c("expensive", "reasonable")
  )

# Get the p-value
p_value <- get_p_value(
null_distn, obs_stat,
direction = "two sided" # Not alternative = "two.sided"
)

# See the result
p_value

# Fill out the null distribution pipeline
null_distn <- late_shipments %>% 
  # Specify weight_kilograms vs. late
  specify(weight_kilograms ~ late) %>% 
  # Declare a null hypothesis of independence
  hypothesize(null = "independence") %>% 
  # Generate 1000 permutation replicates
  generate(reps = 1000, type = "permute") %>% 
  # Calculate the difference in means ("No" minus "Yes")
  calculate(stat = "diff in means", order = c("No", "Yes"))

# See the results
null_distn

# From previous step
null_distn <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))

# Calculate the observed difference in means
obs_stat <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))

# See the result
obs_stat

# From previous steps
null_distn <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))
obs_stat <- late_shipments %>% 
  specify(weight_kilograms ~ late) %>% 
  calculate(stat = "diff in means", order = c("No", "Yes"))

# Get the p-value
p_value <- get_p_value(
  null_distn, obs_stat,
  direction = "less"
)

# See the result
p_value

# Run a Wilcoxon-Mann-Whitney test on weight_kilograms vs. late
test_results <- wilcox.test(
  weight_kilograms ~ late, 
  data = late_shipments
)

# See the result
test_results

# Run a Kruskal-Wallace test on weight_kilograms vs. shipment_mode
test_results <- kruskal.test(
  weight_kilograms ~ shipment_mode, 
  data = late_shipments
)

# See the result
test_results