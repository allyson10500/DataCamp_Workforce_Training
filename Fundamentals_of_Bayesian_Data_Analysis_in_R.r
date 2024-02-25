data <- c(1, 0, 0, 1)
prop_model(data)

# Update the data and rerun prop_model
data = c(1,1,0,0,0,0,0,0,0,0,0,0,0)
prop_model(data)

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)
         
# Extract and explore the posterior

posterior <- prop_model(data)

head(posterior)

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)

# Extract and explore the posterior
posterior <- prop_model(data)
head(posterior)

# Plot the histogram of the posterior
hist(posterior)

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)

# Extract and explore the posterior
posterior <- prop_model(data)
head(posterior)

# Edit the histogram
hist(posterior, breaks = 30, xlim = c(0,1), col = "palegreen4")

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)
posterior <- prop_model(data)
hist(posterior, breaks = 30, xlim = c(0, 1), col = "palegreen4")

# Calculate the median
median(posterior)

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)
posterior <- prop_model(data)
hist(posterior, breaks = 30, xlim = c(0, 1), col = "palegreen4")

# Calculate some measures of interest using the posterior
median(posterior)

# Calculate the credible interval
quantile(posterior,  probs = c(0.05, 0.95))

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)
posterior <- prop_model(data)
hist(posterior, breaks = 30, xlim = c(0, 1), col = "palegreen4")

# Calculate the median
median(posterior)

# Calculate the credible interval
quantile(posterior, c(0.05, 0.95))

# Calculate the sum
sum(posterior > 0.07)

data = c(1, 0, 0, 1, 0, 0,
         0, 0, 0, 0, 0, 0, 0)
posterior <- prop_model(data)
hist(posterior, breaks = 30, xlim = c(0, 1), col = "palegreen4")

# Calculate the median
median(posterior)

# Calculate the credible interval
quantile(posterior, c(0.05, 0.95))

# Calculate the probability
sum(posterior>0.07)/length(posterior)

# The generative zombie drug model

# Set parameters
prop_success <- 0.42
n_zombies <- 100

# Simulating data
data <- c()
for(zombie in 1:n_zombies) {
  data[zombie] <- runif(1, min = 0, max = 1) < prop_success
}
data <- as.numeric(data)
data

# The generative zombie drug model

# Set parameters
prop_success <- 0.42
n_zombies <- 100

# Simulating data
data <- c()
for(zombie in 1:n_zombies) {
  data[zombie] <- runif(1, min = 0, max = 1) < prop_success
}

# Count cured
data <- sum(data)
data

# Try out rbinom
rbinom(n = 1, size = 100, prob = 0.42)

# Change the parameters
rbinom(n = 200, size = 100, prob = 0.42)

# Fill in the parameters
n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n_samples, size = n_ads_shown, 
                     prob = proportion_clicks)

# Visualize n_visitors

hist(n_visitors)

n_samples <- 100000
n_ads_shown <- 100

# Update proportion_clicks
proportion_clicks <- runif(n = n_samples, min = 0.0, max = 0.2)

n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)

n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n_samples, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_samples, size = n_ads_shown, prob = proportion_clicks)

# Visualize proportion clicks
hist(proportion_clicks)

# Visualize n_visitors
hist(n_visitors)

# Create the prior data frame
prior <- data.frame(proportion_clicks, n_visitors)

# Examine the prior data frame
head(prior)

# Create the posterior data frame
posterior <- prior[prior$n_visitors == 13, ]

# Visualize posterior proportion clicks
hist(posterior$proportion_clicks)

# Assign posterior to a new variable called prior
prior <- posterior

# Take a look at the first rows in prior
head(prior)

# Replace prior$n_visitors with a new sample and visualize the result
n_samples <-  nrow(prior)
n_ads_shown <- 100
prior$n_visitors <- rbinom(n_samples, size = n_ads_shown, prob = prior$proportion_clicks)
hist(prior$n_visitors)

# Calculate the probability that you will get 5 or more visitors
sum(prior$n_visitors >= 5)/length(prior$n_visitors)

# Explore using the rbeta function
beta_sample <- rbeta(n = 1000000, shape1 = 1, shape2 = 1)

# Visualize the results
hist(beta_sample)

# Modify the parameters
beta_sample <- rbeta(n = 1000000, shape1 = -1, shape2 = 1)

# Explore the results
head(beta_sample)

# Modify the parameters
beta_sample <- rbeta(n = 1000000, shape1 = 100, shape2 = 100)

# Visualize the results
hist(beta_sample)

# Modify the parameters
beta_sample <- rbeta(n = 1000000, shape1 = 100, shape2 = 20)

# Visualize the results
hist(beta_sample)

n_draws <- 100000
n_ads_shown <- 100

# Change the prior on proportion_clicks
proportion_clicks <- 
  rbeta(n_draws, shape1 = 5, shape2 = 95)
n_visitors <- 
  rbinom(n_draws, size = n_ads_shown, 
         prob = proportion_clicks)
prior <- 
  data.frame(proportion_clicks, n_visitors)
posterior <- 
  prior[prior$n_visitors == 13, ]

# This plots the prior and the posterior in the same plot
par(mfcol = c(2, 1))
hist(prior$proportion_clicks, 
     xlim = c(0, 0.25))
hist(posterior$proportion_clicks, 
     xlim = c(0, 0.25))

# Define parameters
n_draws <- 100000
n_ads_shown <- 100
proportion_clicks <- runif(n_draws, min = 0.0, max = 0.2)
n_visitors <- rbinom(n = n_draws, size = n_ads_shown, 
                     prob = proportion_clicks)
prior <- data.frame(proportion_clicks, n_visitors)

# Create the posteriors for video and text ads
posterior_video <- prior[prior$n_visitors == 13, ]
posterior_text <- prior[prior$n_visitors == 6, ]

# Visualize the posteriors
hist(posterior_video$proportion_clicks, xlim = c(0, 0.25))
hist(posterior_text$proportion_clicks, xlim = c(0, 0.25))

posterior <- data.frame(
    video_prop = posterior_video$proportion_clicks[1:4000],
    text_prop  = posterior_text$proportion_click[1:4000])
    
# Calculate the posterior difference: video_prop - text_prop
posterior$prop_diff <- posterior$video_prop - posterior$text_prop

# Visualize prop_diff
hist(posterior$prop_diff)

# Calculate the median of prop_diff
median(posterior$prop_diff)

# Calculate the proportion
sum(posterior$prop_diff > 0)/length(posterior$prop_diff)

visitor_spend <- 2.53
video_cost <- 0.25
text_cost <- 0.05

# Add the column posterior$video_profit
posterior$video_profit <- posterior$video_prop * visitor_spend - video_cost

# Add the column posterior$text_profit
posterior$text_profit <- posterior$text_prop * visitor_spend - text_cost

# Visualize the video_profit and text_profit columns
hist(posterior$text_profit)
hist(posterior$video_profit)

# Add the column posterior$profit_diff
posterior$profit_diff <- posterior$video_profit - posterior$text_profit

# Visualize posterior$profit_diff
hist(posterior$profit_diff)

# Calculate a "best guess" for the difference in profits
median(posterior$profit_diff)

# Calculate the probability that text ads are better than video ads
sum(posterior$profit_diff <0)/length(posterior$profit_diff)

# Simulate from a Poisson distribution and visualize the result
x <- rpois(n=10000, lambda = 3)
hist(x)

# Simulate from a Poisson distribution and visualize the result
x <- rpois(n=10000, lambda = 11.5)
hist(x)

# Calculate the probability of break-even
sum(x>=15)/length(x)

# Replace proportion_clicks with mean_clicks and change the parameters
n_draws <- 100000
mean_clicks <- runif(n_draws, min = 0, max = 80)
n_visitors <- rpois(n = n_draws, mean_clicks)
                     
prior <- data.frame(mean_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 13, ]

hist(prior$mean_clicks)

# Change this model so that it uses a Poisson distribution
n_draws <- 100000
n_ads_shown <- 100
mean_clicks <- runif(n_draws, min = 0, max = 80)
n_visitors <- rpois(n_draws, lambda = mean_clicks)
                     
prior <- data.frame(mean_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 13, ]

# Change the model according to instructions
n_draws <- 100000
mean_clicks <- runif(n_draws, min = 0, max = 80)
n_visitors <- rpois(n = n_draws, mean_clicks)

prior <- data.frame(mean_clicks, n_visitors)
posterior <- prior[prior$n_visitors == 19, ]

# Visualize mean_clicks
hist(prior$mean_clicks)
hist(posterior$mean_clicks)

# Calculate the probability of drawing any of the four aces
prob_to_draw_ace <- 1/52 +1/52 +1/52 +1/52

# Calculate the probability of picking four aces in a row
prob_to_draw_four_aces <- 4/52 * 3/51 *2/50 *1/49

# Rewrite this code so that it uses dbinom instead of rbinom
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n = 99999, 
    size = n_ads_shown, prob = proportion_clicks)
prob_13_visitors <- dbinom(x = 13, size = 100, prob = 0.1)
prob_13_visitors

# Change the code to calculate probability distribution of n_visitors
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- 0:100
prob <- dbinom(n_visitors, 
    size = n_ads_shown, prob = proportion_clicks)
prob

# Plot the distribution
plot(x = n_visitors, y = prob, type = "h")

# Change the code according to the instructions
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- 13
prob <- dbinom(n_visitors, 
    size = n_ads_shown, prob = proportion_clicks)
prob

plot(proportion_clicks, prob, type = "h")

n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- seq(0, 100, by = 1)
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, 
    size = n_ads_shown, prob = pars$proportion_clicks)

# Add the column pars$probability and normalize it
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability/sum(pars$probability)

n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- seq(0, 100, by = 1)
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, 
    size = n_ads_shown, prob = pars$proportion_clicks)
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)
# Condition on the data 
pars <- pars[pars$n_visitors == 6, ]
# Normalize again
pars$probability <- pars$probability/sum(pars$probability)
# Plot the posterior pars$probability
plot(x = pars$proportion_clicks, y = pars$probability, type = "h")

# Simplify the code below by directly conditioning on the data
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- 6
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, 
    size = n_ads_shown, prob = pars$proportion_clicks)
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)
plot(pars$proportion_clicks, pars$probability, type = "h")

# Assign mu and sigma
mu <- mean(c(3164, 3362, 4435, 3542, 3578, 4529))
sigma <- sd(c(3164, 3362, 4435, 3542, 3578, 4529))

weight_distr <- rnorm(n = 100000, mean = mu, sd = sigma)
hist(weight_distr, 60, xlim = c(0, 6000), col = "lightgreen")

# Create weight
weight <- seq(0, 6000, by = 100)

# Calculate likelihood
likelihood <- dnorm(weight, mu, sigma)

# Plot the distribution of weight
plot(x = weight, y = likelihood, type = "h")

# The IQ of a bunch of zombies
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Defining the parameter grid
pars <- expand.grid(mu = seq(0, 150, length.out = 100), 
                    sigma = seq(0.1, 50, length.out = 100))
# Defining and calculating the prior density for each parameter combination
pars$mu_prior <- dnorm(pars$mu, mean = 100, sd = 100)
pars$sigma_prior <- dunif(pars$sigma, min = 0.1, max = 50)
pars$prior <- pars$mu_prior * pars$sigma_prior
# Calculating the likelihood for each parameter combination
for(i in 1:nrow(pars)) {
  likelihoods <- dnorm(iq, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihoods)
}
# Calculate the probability of each parameter combination
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum( pars$probability )

head(pars)
sample_indices <- sample( nrow(pars), size = 10000,
    replace = TRUE, prob = pars$probability)
head(sample_indices)

# Sample from pars to calculate some new measures
pars_sample <- pars[sample_indices, c("mu","sigma")]

# Visualize the mean IQ
hist(pars_sample$mu, 100, col = 'blue')

# Calculate quantiles
quantile(pars_sample$mu, c(0.025, 0.5, 0.975))

head(pars_sample)
pred_iq <- rnorm(10000, mean = pars_sample$mu, 
                 sd = pars_sample$sigma)

# Visualize pred_iq
hist(pred_iq)

# Calculate the probability of a zombie being "smart" (+60 IQ)
sum(pred_iq >60)/length(pred_iq)

# The IQ of zombies on a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)

# Calculate the mean difference in IQ between the two groups
mean(iq_brains) - mean(iq_regular)

# Fit the BEST model to the data from both groups
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)

# Plot the model result
plot(best_posterior)

# The IQ of zombies given a regular diet and a brain based diet.
iq_brains <- c(44, 52, 42, 66, 53, 42, 55, 57, 56, 51)
iq_regular <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 150)

# Modify the data above and calculate the difference in means
mean(iq_brains) - mean(iq_regular)

# Fit the BEST model to the modified data and plot the result
library(BEST)
best_posterior <- BESTmcmc(iq_brains, iq_regular)
plot(best_posterior)
