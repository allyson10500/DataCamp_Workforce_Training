# Load ggplot2
library(ggplot2)

# Construct a histogram of the weights
ggplot(data = ce, mapping = aes(x = FINLWT21)) +
    geom_histogram()

# Look at the apisrs dataset
glimpse(apisrs)

# Specify a simple random sampling for apisrs
apisrs_design <- svydesign(data = apisrs, weights = ~pw, fpc = ~fpc, id = ~1)

# Produce a summary of the design
summary(apisrs_design)

# Glimpse the data
glimpse(apistrat)

# Summarize strata sample sizes
apistrat %>%
  count(stype)

# Specify the design
apistrat_design <- svydesign(data = apistrat, weights = ~pw, fpc = ~fpc, id = ~1, strata = ~stype)

# Look at the summary information stored in the design object
summary(apistrat_design)

# Glimpse the data
glimpse(apiclus2)

# Specify the design
apiclus_design <- svydesign(id = ~dnum + snum, data = apiclus2, weights = ~pw, fpc = ~fpc1 + fpc2)

#Look at the summary information stored in the design object
summary(apiclus_design)

# Construct histogram of pw
ggplot(data = apisrs,
       mapping = aes(x = pw)) + 
	geom_histogram()

# Construct histogram of pw
ggplot(data = apistrat,
       mapping = aes(x = pw)) + 
	geom_histogram()

# Construct histogram of pw
ggplot(data = apiclus2,
       mapping = aes(x = pw)) + 
	geom_histogram()

#Create table of average survey weights by race
tab_weights <- NHANESraw %>%
  group_by(Race1) %>%
  summarize(avg_wt = mean(WTMEC4YR))

#Print the table
tab_weights

# Specify the NHANES design
NHANES_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, weights = ~WTMEC4YR)

# Print summary of design
summary(NHANES_design)

# Specify the NHANES design
NHANES_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, weights = ~WTMEC4YR)

# Print summary of design
summary(NHANES_design)

# Number of clusters
NHANESraw %>%
  summarize(n_clusters = n_distinct(SDMVSTRA, SDMVPSU))

# Sample sizes in clusters
NHANESraw %>%
  count(SDMVSTRA, SDMVPSU)

# Specify the survey design
NHANESraw <- mutate(NHANESraw, WTMEC4YR = .5 * WTMEC2YR)
NHANES_design <- svydesign(data = NHANESraw, strata = ~SDMVSTRA, id = ~SDMVPSU, nest = TRUE, weights = ~WTMEC4YR)

# Determine the levels of Depressed
levels(NHANESraw$Depressed)

# Construct a frequency table of Depressed
tab_w <- svytable(~Depressed, design = NHANES_design)

# Determine class of tab_w
class(tab_w)

# Display tab_w
tab_w

# Add proportions to table
tab_w <- tab_w %>%
  as.data.frame() %>%
  mutate(Prop = Freq/sum(Freq))

# Create a barplot
ggplot(data = tab_w,
       mapping = aes(x = Depressed, y = Prop)) + 
  geom_col()

# Construct and display a frequency table
tab_D <- svytable(~Depressed,
           design = NHANES_design)
tab_D

# Construct and display a frequency table
tab_H <- svytable(~HealthGen,
           design = NHANES_design)
tab_H

# Construct and display a frequency table
tab_DH <- svytable(~Depressed + HealthGen,
           design = NHANES_design)
tab_DH

# Add conditional proportions to tab_DH
tab_DH_cond <- tab_DH %>%
    as.data.frame() %>%
    group_by(HealthGen) %>%
    mutate(n_HealthGen = sum(Freq), Prop_Depressed = Freq/n_HealthGen) %>%
    ungroup()

# Create a segmented bar graph of the conditional proportions in tab_DH_cond
ggplot(data = tab_DH_cond,
       mapping = aes(x = HealthGen, y = Prop_Depressed, fill = Depressed)) + 
  geom_col() + 
  coord_flip()

# Estimate the totals for combos of Depressed and HealthGen
tab_totals <- svytotal(x = ~interaction(Depressed, HealthGen),
                     design = NHANES_design,
                     na.rm = TRUE)

# Print table of totals
tab_totals

# Estimate the means for combos of Depressed and HealthGen
tab_means <- svymean(x = ~interaction(Depressed, HealthGen),
              design = NHANES_design,
              na.rm = TRUE)

# Print table of means
tab_means

# Run a chi square test between Depressed and HealthGen
svychisq(~Depressed+HealthGen, 
    design = NHANES_design, 
    statistic = "Chisq")

# Construct a contingency table
tab <-svytable(~HomeOwn+Education, design = NHANES_design) 

# Add conditional proportion of levels of HomeOwn for each educational level
tab_df <- as.data.frame(tab) %>%
  group_by(Education) %>%
  mutate(n_Education = sum(Freq), Prop_HomeOwn = Freq/n_Education) %>%
  ungroup()

# Create a segmented bar graph
ggplot(data = tab_df,
       mapping = aes(x = Education, y = Prop_HomeOwn, fill = HomeOwn)) + 
  geom_col() + 
  coord_flip()

# Run a chi square test
svychisq(~HomeOwn+Education, 
    design = NHANES_design, 
    statistic = "Chisq")

# Compute the survey-weighted mean
svymean(x = ~SleepHrsNight, 
        design = NHANES_design,
        na.rm = TRUE)

# Compute the survey-weighted mean by Gender
svyby(formula = ~SleepHrsNight, 
      by = ~Gender, 
      design = NHANES_design, 
      FUN = svymean, 
      na.rm = TRUE, 
      keep.names = FALSE)

# Compute the survey-weighted quantiles
svyquantile(x = ~SleepHrsNight, 
            design = NHANES_design, 
            na.rm = TRUE, 
            quantiles = c(0.01, 0.25, 0.5, 0.75, .99))

# Compute the survey-weighted quantiles by Gender
svyby(formula = ~SleepHrsNight, 
      by = ~Gender, 
      design = NHANES_design, 
      FUN = svyquantile, 
      na.rm = TRUE, 
      quantiles = 0.5, 
      keep.rows = FALSE, 
      keep.var = FALSE)

# Compute the survey-weighted mean by Gender
out <- svyby(formula = ~SleepHrsNight, 
             by = ~Gender, 
             design = NHANES_design, 
             FUN = svymean, 
             na.rm = TRUE, 
             keep.names = FALSE)
             
# Construct a bar plot of average sleep by gender
ggplot(data =out , mapping = aes(x=Gender, y=SleepHrsNight)) +
  geom_col() + 
  labs(y="Average Nightly Sleep")

# Add lower and upper columns to out
out_col <- mutate(out, 
                  lower = SleepHrsNight - 2*se, 
                  upper = SleepHrsNight + 2*se)

# Construct a bar plot of average sleep by gender with error bars
ggplot(data = out_col, 
       mapping = aes(x = Gender, y = SleepHrsNight, 
                     ymin = lower, ymax = upper)) +
  geom_col(fill = "gold") +
  labs(y = "Average Nightly Sleep") +
  geom_errorbar(width = 0.7)

# Create a histogram with a set binwidth
ggplot(data = NHANESraw,
       mapping = aes(x=SleepHrsNight,weight=WTMEC4YR)) + 
  geom_histogram(binwidth = 1,
                 color = "white") +
  labs(x = "Hours of Sleep")

# Create a histogram with a set binwidth
ggplot(data = NHANESraw,
       mapping = aes(x=SleepHrsNight,weight=WTMEC4YR)) + 
  geom_histogram(binwidth = 0.5,
                 color = "white") +
  labs(x = "Hours of Sleep")

# Create a histogram with a set binwidth
ggplot(data = NHANESraw,
       mapping = aes(x=SleepHrsNight,weight=WTMEC4YR)) + 
  geom_histogram(binwidth = 2,
                 color = "white") +
  labs(x = "Hours of Sleep")

# Density plot of sleep faceted by gender
NHANESraw %>%
    filter(!is.na(SleepHrsNight), !is.na(Gender)) %>%
    group_by(Gender) %>%
    mutate(WTMEC4YR_std = WTMEC4YR/sum(WTMEC4YR)) %>%
    ggplot(mapping = aes(x = SleepHrsNight, weight = WTMEC4YR_std)) + 
        geom_density(bw = 0.6,  fill = "gold") +
        labs(x = "Hours of Sleep") + 
        facet_wrap(~Gender, labeller = "label_both")

# Run a survey-weighted t-test
svyttest(formula = SleepHrsNight~Gender,
       design = NHANES_design)

# Find means of total cholesterol by whether or not active 
out <- svyby(formula =~TotChol,
           by = ~PhysActive, 
           design = NHANES_design,
           FUN = svymean, 
           na.rm = TRUE, 
           keep.names = FALSE)

# Construct a bar plot of means of total cholesterol by whether or not active 
ggplot(data = out,
       mapping = aes(x=PhysActive, y=TotChol)) +
  geom_col()

# Run t test for difference in means of total cholesterol by whether or not active
svyttest(formula = TotChol ~ PhysActive,
    design = NHANES_design)

# Create dataset with only 20 year olds
NHANES20 <- filter(NHANESraw,
                Age == 20)

# Construct scatter plot
ggplot(data = NHANES20, 
       mapping = aes(x=Height, y=Weight)) + 
    geom_point(alpha = 0.3) + 
    guides(size = "none")

# Construct bubble plot
ggplot(data = NHANES20, 
       mapping = aes(x=Height, y=Weight, size=WTMEC4YR)) + 
    geom_point(alpha = 0.3) + 
    guides(size = "none")

# Construct a scatter plot
ggplot(data = NHANES20,
       mapping = aes(x= Height, y=Weight, color=WTMEC4YR)) + 
    geom_point() + 
    guides(color = "none")

# Construct a scatter plot
ggplot(data = NHANES20,
       mapping = aes(x= Height, y=Weight, alpha=WTMEC4YR)) + 
    geom_point() + 
    guides(alpha = "none")

# Add gender to plot
ggplot(data = NHANES20,
       mapping = aes(x=Height, y=Weight, size=WTMEC4YR, color=Gender)) + 
    geom_point(alpha=0.3) + 
    guides(size = "none")

# Add gender to plot
ggplot(data = NHANES20,
       mapping = aes(x=Height, y=Weight,alpha=WTMEC4YR, color=Gender)) + 
    geom_point() + 
    guides(alpha = "none")

# Bubble plot with linear of best fit
ggplot(data = NHANESraw, mapping = aes(x = Height, y = Weight, size = WTMEC4YR)) + 
  geom_point(alpha = 0.1) + 
  guides(size = FALSE) + 
  geom_smooth(method = "lm", se = FALSE, mapping = aes(weight=WTMEC4YR))

# Add quadratic curve and cubic curve
ggplot(data = NHANESraw, mapping = aes(x = Height, y = Weight, size = WTMEC4YR)) + 
  geom_point(alpha = 0.1) + 
  guides(size = FALSE) + 
  geom_smooth(method = "lm", se = FALSE, mapping = aes(weight = WTMEC4YR)) +
  geom_smooth(method = "lm", se = FALSE, mapping = aes(weight = WTMEC4YR), formula = y ~ poly(x, 2), color = "orange") +
  geom_smooth(method = "lm", se = FALSE, mapping = aes(weight = WTMEC4YR), formula = y ~ poly(x, 3), color = "red")

# Add non-survey-weighted trend lines to bubble plot
ggplot(data = NHANES20, mapping = aes(x = Height, y = Weight, size = WTMEC4YR, color = Gender)) + 
  geom_point(alpha = 0.1) + 
  guides(size = "none") + 
  geom_smooth(method = "lm", se = FALSE, linetype = 2)

# Add non-survey-weighted trend lines to bubble plot
ggplot(data = NHANES20, mapping = aes(x = Height, y = Weight, size = WTMEC4YR, color = Gender)) + 
  geom_point(alpha = 0.1) + 
  guides(size = "none") + 
  geom_smooth(method = "lm", se = FALSE, linetype = 2)

# Add survey-weighted trend lines
ggplot(data = NHANES20, mapping = aes(x = Height, y = Weight, size = WTMEC4YR, color = Gender)) + 
  geom_point(alpha = 0.1) + 
  guides(size = "none") + 
  geom_smooth(method = "lm", se = FALSE, linetype = 2) + 
  geom_smooth(method = "lm", se = FALSE, mapping = aes(weight = WTMEC4YR))

# Subset survey design object to only include 20 year olds
NHANES20_design <- subset(NHANES_design, Age == 20)

# Build a linear regression model
mod <- svyglm(Weight ~ Height, design = NHANES20_design)

# Print summary of the model
summary(mod)

# Build a linear regression model same slope
mod1 <- svyglm(Weight ~ Height+Gender, design = NHANES20_design)

# Print summary of the same slope model
summary(mod1)

# Build a linear regression model different slopes
mod2 <- svyglm(Weight ~ Height*Gender, design = NHANES20_design)

# Print summary of the different slopes model
summary(mod2)

# Plot BPDiaAve and BPSysAve by Diabetes and include trend lines
drop_na(NHANESraw, Diabetes) %>%
ggplot(mapping = aes(x=BPDiaAve, y=BPSysAve, size=WTMEC4YR, color=Diabetes)) + 
    geom_point(alpha = 0.2) +  
    guides(size = FALSE) + 
    geom_smooth(method="lm", se = FALSE, mapping = aes(weight=WTMEC4YR))

# Build simple linear regression model
mod1 <- svyglm(BPSysAve ~ BPDiaAve, design = NHANES_design)

# Build model with different slopes
mod2 <- svyglm(BPSysAve ~ BPDiaAve * Diabetes, design = NHANES_design)

# Summarize models
summary(mod1)
summary(mod2)

