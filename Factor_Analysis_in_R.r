# Load the psych package
library(psych)
 
# Conduct a single-factor EFA
EFA_model <- fa(gcbs)

# View the results
print(EFA_model)

# Set up the single-factor EFA
EFA_model <- fa(gcbs)

# View the factor loadings
EFA_model$loadings

# Create a path diagram of the items' factor loadings
fa.diagram(EFA_model)

# Take a look at the first few lines of the response data and their corresponding sum scores
head(gcbs)
rowSums(gcbs[1:6, ])

# Then look at the first few lines of individuals' factor scores
head(EFA_model$scores)

# To get a feel for how the factor scores are distributed, look at their summary statistics and density plot.
summary(EFA_model$scores)

plot(density(EFA_model$scores, na.rm = TRUE), 
    main = "Factor Scores")

# Basic descriptive statistics
describe(gcbs)

# Graphical representation of error
error.dots(gcbs)

# Graphical representation of error
error.bars(gcbs)

# Establish two sets of indices to split the dataset
N <- nrow(gcbs)
indices <- seq(1, N)
indices_EFA <- sample(indices, floor((.5*N)))
indices_CFA <- indices[!(indices %in% indices_EFA)]

# Use those indices to split the dataset into halves for your EFA and CFA
gcbs_EFA <- gcbs[indices_EFA, ]
gcbs_CFA <- gcbs[indices_CFA, ]

# Use the indices from the previous exercise to create a grouping variable
group_var <- vector("numeric", nrow(gcbs))
group_var[indices_EFA] <- 1
group_var[indices_CFA] <- 2

# Bind that grouping variable onto the gcbs dataset
gcbs_grouped <- cbind(gcbs, group_var)

# Compare stats across groups
describeBy(gcbs_grouped, group = group_var)
statsBy(gcbs_grouped, group = "group_var")

lowerCor(gcbs, use = "pairwise.complete.obs")

corr.test(gcbs, use = "pairwise.complete.obs")$p

corr.test(gcbs, use = "pairwise.complete.obs")$ci

# Estimate coefficient alpha
alpha(gcbs)

# Calculate split-half reliability
splitHalf(gcbs)

#both above 0.8 = good reliability

# Establish two sets of indices to split the dataset
N <- nrow(bfi)
indices <- seq(1, N)
indices_EFA <- sample(indices, floor((.5*N)))
indices_CFA <- indices[!(indices %in% indices_EFA)]

# Use those indices to split the dataset into halves for your EFA and CFA
bfi_EFA <- bfi[indices_EFA, ]
bfi_CFA <- bfi[indices_CFA, ]

# Calculate the correlation matrix first
bfi_EFA_cor <- cor(bfi_EFA, use = "pairwise.complete.obs")

# Then use that correlation matrix to calculate eigenvalues
eigenvals <- eigen(bfi_EFA_cor)

# Look at the eigenvalues returned
eigenvals$values

# Calculate the correlation matrix first
bfi_EFA_cor <- cor(bfi_EFA, use = "pairwise.complete.obs")

# Then use that correlation matrix to create the scree plot
scree(bfi_EFA_cor, factors = FALSE)
#6 recommended factors

# Run the EFA with six factors (as indicated by your scree plot)
EFA_model <- fa(bfi_EFA, nfactors = 6)

# View results from the model object
print(EFA_model)

# Run the EFA with six factors (as indicated by your scree plot)
EFA_model <- fa(bfi_EFA, nfactors = 6)

# View items' factor loadings
EFA_model$loadings

# View the first few lines of examinees' factor scores
head(EFA_model$scores)

#chi-sq non significant & The TLI is above the 0.90 cutoff for good fit, and the RMSEA is below the 0.05 cutoff for good fit.

# Run each theorized EFA on your dataset
bfi_theory <- fa(bfi_EFA, nfactors = 5)
bfi_eigen <- fa(bfi_EFA, nfactors = 6)

# Compare the BIC values
bfi_theory$BIC
bfi_eigen$BIC

# Conduct a five-factor EFA on the EFA half of the dataset
EFA_model <- fa(bfi_EFA, nfactors=5)

# Use the wrapper function to create syntax for use with the sem() function
EFA_syn <- structure.sem(EFA_model)

# Set up syntax specifying which items load onto each factor
theory_syn_eq <- "
AGE: A1, A2, A3, A4, A5
CON: C1, C2, C3, C4, C5
EXT: E1, E2, E3, E4, E5
NEU: N1, N2, N3, N4, N5
OPE: O1, O2, O3, O4, O5
"

# Feed the syntax in to have variances and covariances automatically added
theory_syn <- cfa(text = theory_syn_eq,
    reference.indicators = FALSE)

# Use the sem() function to run a CFA
theory_CFA <- sem(theory_syn, data = bfi_CFA)

# Use the summary function to view fit information and parameter estimates
summary(theory_CFA)


# Set the options to include various fit indices so they will print
options(fit.indices = c("CFI", "GFI", "RMSEA", "BIC"))

# Use the summary function to view fit information and parameter estimates
summary(theory_CFA)

# Run a CFA using the EFA syntax you created earlier
EFA_CFA <- sem(EFA_syn, data = bfi_CFA)

# Locate the BIC in the fit statistics of the summary output
summary(EFA_CFA)$BIC

# Compare EFA_CFA BIC to the BIC from the CFA based on theory
summary(theory_CFA)$BIC
#lower BIC preferred

# View the first five rows of the EFA loadings
EFA_model$loadings[1:5,]

# View the first five loadings from the CFA estimated from the EFA results
summary(EFA_CFA)$coeff[1:5,]

# Extracting factor scores from the EFA model
EFA_scores <- EFA_model$scores

# Calculating factor scores by applying the CFA parameters to the EFA dataset
CFA_scores <- fscores(EFA_CFA, data=bfi_EFA)

# Comparing factor scores from the EFA and CFA results from the bfi_EFA dataset
plot(density(EFA_scores[,1], na.rm=TRUE),
    xlim=c(-3, 3), ylim=c(0, 1), col="blue")
lines(density(CFA_scores[,1], na.rm=TRUE),
    xlim=c(-3, 3), ylim=c(0, 1), col="red")

# Add some plausible item/factor loadings to the syntax
theory_syn_add <- "
AGE: A1, A2, A3, A4, A5
CON: C1, C2, C3, C4, C5
EXT: E1, E2, E3, E4, E5, N4
NEU: N1, N2, N3, N4, N5, E3
OPE: O1, O2, O3, O4, O5
"

# Convert your equations to sem-compatible syntax
theory_syn2 <- cfa(text = theory_syn_add, reference.indicators = FALSE)

# Run a CFA with the revised syntax
theory_CFA_add <- sem(model = theory_syn2, data = bfi_CFA)

# Conduct a likelihood ratio test
anova(theory_CFA, theory_CFA_add)

# Compare the comparative fit indices - higher is better!
summary(theory_CFA)$CFI
summary(theory_CFA_add)$CFI

# Compare the RMSEA values - lower is better!
summary(theory_CFA)$RMSEA
summary(theory_CFA_add)$RMSEA

# Compare BIC values - lower is better
summary(theory_CFA)$BIC
summary(theory_CFA_add)$BIC

# Remove the weakest factor loading from the syntax
theory_syn_del <- "
AGE: A1, A2, A3, A4, A5
CON: C1, C2, C3, C4, C5
EXT: E1, E2, E3, E4, E5
NEU: N1, N2, N3, N4, N5
OPE: O1, O2, O3, O5
"

# Convert your equations to sem-compatible syntax
theory_syn3 <- cfa(text = theory_syn_del, reference.indicators = FALSE)

# Run a CFA with the revised syntax
theory_CFA_del <- sem(model = theory_syn3, data = bfi_CFA)

# Compare the comparative fit indices - higher is better!
summary(theory_CFA)$CFI
summary(theory_CFA_del)$CFI

# Compare the RMSEA values - lower is better!
summary(theory_CFA)$RMSEA
summary(theory_CFA_del)$RMSEA

# Compare BIC values
summary(theory_CFA)$BIC
summary(theory_CFA_del)$BIC


