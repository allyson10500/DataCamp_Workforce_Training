# Load sparklyr
library(sparklyr)

# Connect to your Spark cluster
spark_conn <- spark_connect(master = "local")

# Print the version of Spark
spark_version(sc = spark_conn)

# Disconnect from Spark
spark_disconnect(sc = spark_conn)

# Load dplyr
library(dplyr)

# Explore track_metadata structure
str(track_metadata)

# Connect to your Spark cluster
spark_conn <- spark_connect(master = "local")

# Copy track_metadata to Spark
track_metadata_tbl <- copy_to(spark_conn, track_metadata)

# List the data frames available in Spark
src_tbls(spark_conn)

# Disconnect from Spark
spark_disconnect(sc = spark_conn)

# Link to the track_metadata table in Spark
track_metadata_tbl <- tbl(spark_conn, "track_metadata")

# See how big the dataset is
dim(track_metadata_tbl)

# See how small the tibble is
object_size(track_metadata_tbl)

# Print 5 rows, all columns
print(track_metadata_tbl, n = 5, width = Inf)

# Examine structure of tibble
str(track_metadata_tbl)

# Examine structure of data
glimpse(track_metadata_tbl)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year)

# Try to select columns using [ ]; tryCatch(error = print) is a nice way to see errors without them stopping the execution of your code.
tryCatch({
    # Selection code here
    track_metadata_tbl[, c("artist_name", "release", "title", "year")]
  },
  error = print

# track_metadata_tbl has been pre-defined
glimpse(track_metadata_tbl)

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year) %>%
  # Filter rows
  filter(year %in% 1960:1969)    
)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(artist_name, release, title, year) %>%
  # Filter rows
  filter(year >= 1960, year < 1970) %>%
  # Arrange rows
  arrange(artist_name, desc(year), title)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(title, duration) %>%
  # Mutate columns
  mutate(duration_minutes = duration/60)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Manipulate the track metadata
track_metadata_tbl %>%
  # Select columns
  select(title, duration) %>%
  # Mutate columns
  mutate(duration_minutes = duration/60) %>%
  # Summarize columns
  summarize(mean_duration_minutes = mean(duration_minutes))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Select columns starting with artist
  select(starts_with("artist"))

track_metadata_tbl %>%
  # Select columns ending with id
  select(ends_with("id"))

#  a: A letter means "match that letter".
#  .: A dot means "match any character, including letters, numbers, punctuation, etc.".
#  ?: A question mark means "the previous character is optional"

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Select columns containing ti
  select(contains("ti"))

track_metadata_tbl %>%
  # Select columns matching ti.?t
  select(matches("ti.?t"))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Only return rows with distinct artist_name
  distinct(artist_name)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Count the artist_name values
  count(artist_name, sort = TRUE) %>%
  # Restrict to top 20
  top_n(20)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

results <- track_metadata_tbl %>%
  # Filter where artist familiarity is greater than 0.9
  filter(artist_familiarity > 0.9)

# Examine the class of the results
class(results)

# Collect your results
collected <- results %>%
  collect()

# Examine the class of the collected results
class(collected)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

computed <- track_metadata_tbl %>%
  # Filter where artist familiarity is greater than 0.8
  filter(artist_familiarity > 0.8) %>%
  # Compute the results
  compute("familiar_artists")

# See the available datasets
src_tbls(spark_conn)

# Examine the class of the computed results
class(computed)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

duration_by_artist <- track_metadata_tbl %>%
  # Group by artist
  group_by(artist_name) %>%
  # Calc mean duration
  summarize(mean_duration = mean(duration))

duration_by_artist %>%
  # Sort by ascending mean duration
  arrange(mean_duration)

duration_by_artist %>%
  # Sort by descending mean duration
  arrange(desc(mean_duration))

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Group by artist
  group_by(artist_name) %>%
  # Calc time since first release
  mutate(time_since_first_release = year - min(year)) %>%
  # Arrange by descending time since first release
  arrange(desc(time_since_first_release))

# Write SQL query
query <- "SELECT * FROM track_metadata WHERE year < 1935 AND duration > 300"

# Run the query
(results <- dbGetQuery(spark_conn, query))

# track_metadata_tbl and artist_terms_tbl have been pre-defined
track_metadata_tbl
artist_terms_tbl

# Left join artist terms to track metadata by artist_id
joined <- left_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")

# How many rows and columns are in the joined table?
dim(joined)

# track_metadata_tbl and artist_terms_tbl have been pre-defined
track_metadata_tbl
artist_terms_tbl

# Anti join artist terms to track metadata by artist_id
joined <- anti_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")

# How many rows and columns are in the joined table?
dim(joined)

# track_metadata_tbl and artist_terms_tbl have been pre-defined
track_metadata_tbl
artist_terms_tbl

# Semi join artist terms to track metadata by artist_id
joined <- semi_join(track_metadata_tbl, artist_terms_tbl, by = "artist_id")

# How many rows and columns are in the joined table?
dim(joined)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

hotttnesss <- track_metadata_tbl %>%
  # Select artist_hotttnesss
  select(artist_hotttnesss) %>%
  # Binarize to is_hottt_or_nottt
  ft_binarizer("artist_hotttnesss", "is_hottt_or_nottt", threshold = 0.5) %>%
  # Collect the result
  collect() %>%
  # Convert is_hottt_or_nottt to logical
  mutate(is_hottt_or_nottt = as.logical(is_hottt_or_nottt))

# Draw a barplot of is_hottt_or_nottt
ggplot(hotttnesss, aes(is_hottt_or_nottt)) +
  geom_bar()

# track_metadata_tbl, decades, decade_labels have been pre-defined
track_metadata_tbl
decades
decade_labels

hotttnesss_over_time <- track_metadata_tbl %>%
  # Select artist_hotttnesss and year
  select(artist_hotttnesss, year) %>%
  # Convert year to numeric
  mutate(year = as.numeric(year)) %>%
  # Bucketize year to decade using decades vector
  ft_bucketizer("year", "decade", splits = decades) %>%
  # Collect the result
  collect() %>%
  # Convert decade to factor using decade_labels
  mutate(decade = factor(decade, labels = decade_labels))

# Draw a boxplot of artist_hotttnesss by decade
ggplot(hotttnesss_over_time, aes(decade, artist_hotttnesss)) +
  geom_boxplot()

# track_metadata_tbl, duration_labels have been pre-defined
track_metadata_tbl
duration_labels

familiarity_by_duration <- track_metadata_tbl %>%
  # Select duration and artist_familiarity
  select(duration, artist_familiarity) %>%
  # Bucketize duration
  ft_quantile_discretizer("duration", "duration_bin", n.buckets = 5) %>%
  # Collect the result
  collect() %>%
  # Convert duration bin to factor
  mutate(duration_bin = factor(duration_bin, labels = duration_labels))

# Draw a boxplot of artist_familiarity by duration_bin
ggplot(familiarity_by_duration, aes(duration_bin, artist_familiarity)) +
  geom_boxplot()

# track_metadata_tbl has been pre-defined
track_metadata_tbl

title_text <- track_metadata_tbl %>%
  # Select artist_name, title
  select(artist_name, title) %>%
  # Tokenize title to words
  ft_tokenizer("title", "word") %>%
  # Collect the result
  collect() %>%
  # Flatten the word column 
  mutate(word = lapply(word, as.character)) %>% 
  # Unnest the list column
  unnest()

# title_text_tbl, afinn_sentiments_tbl have been pre-defined
title_text_tbl
afinn_sentiments_tbl

sentimental_artists <- title_text_tbl %>%
  # Inner join with sentiments on word field
  inner_join(afinn_sentiments_tbl, by = "word") %>%
  # Group by artist
  group_by(artist_name) %>%
  # Summarize to get positivity
  summarize(positivity = sum(score))

sentimental_artists %>%
  # Arrange by ascending positivity
  arrange(positivity) %>%
  # Get top 5
  top_n(5)

sentimental_artists %>%
  # Arrange by descending positivity
  arrange(desc(positivity)) %>%
  # Get top 5
  top_n(5)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Select artist_mbid column
  select(artist_mbid) %>%
  # Split it by hyphens
  ft_regex_tokenizer("artist_mbid","artist_mbid_chunks", pattern = "-" )

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Compare timings of arrange() and sdf_sort()
microbenchmark(
  arranged = track_metadata_tbl %>%
    # Arrange by year, then artist_name, then release, then title
    arrange(year, artist_name, release, title) %>%
    # Collect the result
    collect(),
  sorted = track_metadata_tbl %>%
    # Sort by year, then artist_name, then release, then title
    sdf_sort(c("year", "artist_name", "release", "title")) %>%
    # Collect the result
    collect(),
  times = 5
)

# track_metadata_tbl has been pre-defined
track_metadata_tbl

# Get the schema
(schema <- sdf_schema(track_metadata_tbl))

# Transform the schema
schema %>%
  lapply(function(x) do.call(data_frame, x)) %>%
  bind_rows()

# track_metadata_tbl has been pre-defined
track_metadata_tbl

track_metadata_tbl %>%
  # Sample the data without replacement
  sdf_sample(seed = 20000229, fraction = 0.01, replacement = FALSE) %>%
  # Compute the result
  compute("sample_track_metadata")

# track_metadata_tbl has been pre-defined
track_metadata_tbl

partitioned <- track_metadata_tbl %>%
  # Partition into training and testing sets
  sdf_partition(training = 0.7, testing = 0.3)

# Get the dimensions of the training set
dim(partitioned$training)

# Get the dimensions of the testing set
dim(partitioned$testing)

#ls("package:sparklyr", pattern = "^ml") to see all available ML packages

# timbre has been pre-defined
timbre

# Calculate column means
(mean_timbre <- colMeans(timbre))

# parquet_dir has been pre-defined
parquet_dir

# List the files in the parquet dir
filenames <- dir(parquet_dir, full.names = TRUE)

# Show the filenames and their sizes
data_frame(
  filename = basename(filenames),
  size_bytes = file.size(filenames)
)

# Import the data into Spark
timbre_tbl <- spark_read_parquet(spark_conn, "timbre", parquet_dir)

# track_metadata_tbl, timbre_tbl pre-defined
track_metadata_tbl
timbre_tbl

track_metadata_tbl %>%
  # Inner join to timbre_tbl
  inner_join(timbre_tbl, by = "track_id") %>%
  # Convert year to numeric
  mutate(year = as.numeric(year))

# track_data_tbl has been pre-defined
track_data_tbl

training_testing_artist_ids <- track_data_tbl %>%
  # Select the artist ID
  select(artist_id) %>%
  # Get distinct rows
  distinct(artist_id) %>%
  # Partition into training/testing sets
  sdf_partition(training = 0.7, testing = 0.3)

track_data_to_model_tbl <- track_data_tbl %>%
  # Inner join to training partition
  inner_join(training_testing_artist_ids$training, by = "artist_id")

track_data_to_predict_tbl <- track_data_tbl %>%
  # Inner join to testing partition
    inner_join(training_testing_artist_ids$testing, by = "artist_id")

# track_data_tbl has been pre-defined
track_data_to_model_tbl

feature_colnames <- track_data_to_model_tbl %>%
  # Get the column names
  colnames() %>%
  # Limit to the timbre columns
  str_subset(fixed("timbre"))

gradient_boosted_trees_model <- track_data_to_model_tbl %>%
  # Run the gradient boosted trees model
  ml_gradient_boosted_trees("year", feature_colnames)      

# training, testing sets & model are pre-defined
track_data_to_model_tbl
track_data_to_predict_tbl
gradient_boosted_trees_model

responses <- track_data_to_predict_tbl %>%
  # Select the response column
  select(year) %>%
  # Collect the results
  collect() %>%
  # Add in the predictions
  mutate(
    predicted_year = predict(
      gradient_boosted_trees_model,
      track_data_to_predict_tbl
    )
  )         

# responses has been pre-defined
responses

# Draw a scatterplot of predicted vs. actual
ggplot(responses, aes(actual, predicted)) +
  # Add the points
  geom_point(alpha = 0.1) +
  # Add a line at actual = predicted
  geom_abline(intercept = 0, slope = 1)

residuals <- responses %>%
  # Transmute response data to residuals
  transmute(residual = predicted - actual)

# Draw a density plot of residuals
ggplot(residuals, aes(residual)) +
    # Add a density curve
    geom_density() +
    # Add a vertical line through zero
    geom_vline(xintercept = 0)    

# track_data_to_model_tbl has been pre-defined
track_data_to_model_tbl

feature_colnames <- track_data_to_model_tbl %>%
  # Get the column names
  colnames() %>%
  # Limit to the timbre columns
  str_subset(fixed("timbre"))

random_forest_model <- track_data_to_model_tbl %>%
  # Run the gradient boosted trees model
  ml_random_forest("year", feature_colnames)  

# training, testing sets & model are pre-defined
track_data_to_model_tbl
track_data_to_predict_tbl
random_forest_model

# Create a response vs. actual dataset
responses <- track_data_to_predict_tbl %>%
  # Select the response column
  select(year) %>%
  # Collect the results
  collect() %>%
  # Add in the predictions
  mutate(
    predicted_year = predict(
      random_forest_model,
      track_data_to_predict_tbl
    )
  )   

# both_responses has been pre-defined
both_responses

# Draw a scatterplot of predicted vs. actual
ggplot(both_responses, aes(actual, predicted, color = model)) +
  # Add a smoothed line
  geom_smooth() +
  # Add a line at actual = predicted
  geom_abline(intercept = 0, slope = 1)

# Create a tibble of residuals
residuals <- both_responses %>%
  mutate(residual = predicted - actual)

# Draw a density plot of residuals
ggplot(residuals, aes(residual, color = model)) +
    # Add a density curve
    geom_density() +
    # Add a vertical line through zero
    geom_vline(xintercept = 0)

# both_responses has been pre-defined
both_responses

both_responses %>%
  # Add a residual column
  mutate(
    residual = predicted - actual
  ) %>%
  # Group by model
  group_by(model) %>%
  # Calculate the root mean square error
  summarize(
    rmse = sqrt(mean(residual ^ 2))
  )         