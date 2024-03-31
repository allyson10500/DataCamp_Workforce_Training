library(dplyr)
library(ggplot2)

# Summarize the number of taxi rides by pickup day
daily_count <- tx %>%
  group_by(pickup_date) %>%
  summarise(n_rides = n())

# Create a line plot
ggplot(daily_count, aes(pickup_date, n_rides)) +
  geom_line()

library(ggplot2)

# Create a histogram of total_amount
ggplot(tx, aes(total_amount)) +
  geom_histogram() +
  scale_x_log10()

library(ggplot2)

# Create a bar chart of payment_type
ggplot(tx, aes(payment_type)) +
  geom_bar()

library(ggplot2)

# Create a hexagon-binned plot of total_amount vs. trip_duration
ggplot(tx, aes(trip_duration, total_amount)) +
  geom_hex(bins = 75) +
  scale_x_log10() +
  scale_y_log10()

library(dplyr)
library(ggplot2)

# Summarize taxi rides count by payment type, pickup date, pickup day of week
daily_count <- tx %>%
  filter(payment_type %in% c("Card", "Cash")) %>%
  group_by(pickup_date, pickup_dow, payment_type) %>%
  summarise(n_rides = n())

# Plot the data
ggplot(daily_count, aes(pickup_date, n_rides)) +
  geom_point() +
  facet_grid(payment_type ~ pickup_dow) +
  coord_fixed(ratio = 0.4)

library(ggplot2)

# Histogram of the tip amount faceted on payment type
ggplot(tx, aes(tip_amount +0.01)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~ payment_type, ncol = 1, scales = "free_y")

library(ggplot2)
library(dplyr)
library(tidyr)

# Get data ready to plot
amount_compare <- tx_pop %>%
  mutate(total_no_tip = total_amount - tip_amount) %>%
  select(total_amount, total_no_tip, payment_type) %>%
  pivot_longer(!payment_type, names_to = "amount_type", values_to = "amount")

# Quantile plot
ggplot(amount_compare, aes(sample = amount, color = payment_type)) +
  geom_qq(distribution = stats::qunif, shape = 21) +
  facet_wrap(~ amount_type) +
  ylim(c(3, 20))

library(ggplot2)
library(trelliscopejs)

# Create the plot
ggplot(gapminder, aes(year, lifeExp)) +
  geom_line() +
  # Facet on country and continent
  facet_trelliscope(~ country + continent)

library(ggplot2)
library(trelliscopejs)

ggplot(gapminder, aes(year, lifeExp)) +
  geom_line() +
  facet_trelliscope(~ country + continent,
    name = "lifeExp_by_country",
    desc = "Life expectancy vs. year per country",
    nrow = 1, ncol = 2
  )

library(trelliscopejs)
library(ggplot2)

# Create the plot
ggplot(gapminder, aes(year, lifeExp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_trelliscope(~ country + continent,
    name = "lifeExp_by_country",
    desc = "Life expectancy vs. year for 142 countries.",
    nrow = 1, ncol = 2,
    # Set the scales
    scales = "sliced",
    # Specify automatic cognistics
    auto_cog = TRUE)

library(ggplot2)
library(dplyr)
library(gapminder)
library(trelliscopejs)
space_to_dash <- function(x) gsub(" ", "-", x)

# Group by country and create the two new variables
gap <- gapminder %>%
  group_by(country) %>%
  mutate(
    delta_lifeExp = tail(lifeExp, 1) - head(lifeExp, 1),
    ihme_link = paste0("http://www.healthdata.org/", space_to_dash(country)))

# Add the description
gap$delta_lifeExp <- cog(gap$delta_lifeExp, desc = "Overall change in life expectancy")
# Specify the default label
gap$ihme_link <- cog(gap$ihme_link, default_label = TRUE)

ggplot(gap, aes(year, lifeExp)) +
  geom_point() +
  facet_trelliscope(~ country + continent,
    name = "lifeExp_by_country",
    desc = "Life expectancy vs. year.",
    nrow = 1, ncol = 2,
    scales = c("same", "sliced"))

library(dplyr)
library(tidyr)

# Nest stocks by symbol
by_symbol <- stocks %>%
  group_by(symbol) %>%
  nest()

# Inspect by_symbol to report the first opening price for the first stock in the data, "TWOU"
by_symbol$data[[1]]

min_volume_fn <- function(x) min(x$volume)

# Create new column
by_symbol_min <- by_symbol %>%
  mutate(min_volume = map_dbl(data, min_volume_fn))

library(trelliscopejs)
library(dplyr)
library(purrr)
library(plotly)

ohlc_plot <- function(d) {
  plot_ly(d, x = ~date, type = "ohlc",
    open = ~open, close = ~close,
    high = ~high, low = ~low)
}

by_symbol_plot <- by_symbol %>%
  mutate(panel = map_plot(data, ohlc_plot))

by_symbol_plot$panel[[1]]

trelliscope(by_symbol_plot, name = "ohlc_top500")

library(trelliscopejs)
library(dplyr)

# Create market_cap_log
by_symbol <- mutate(by_symbol,
  market_cap_log = cog(
    val = log10(market_cap),
    desc = "log base 10 market capitalization"
  )
)

library(trelliscopejs)

# Create the trelliscope display
p <- trelliscope(by_symbol,
desc = "Anything you choose", 
name = "ohlc_top500",
width = 600,
height = 300)

# View the display
p

library(trelliscopejs)
library(dplyr)
library(purrr)

annual_return <- function(x)
  100 * (tail(x$close, 1) - head(x$open, 1)) / head(x$open, 1)

# Compute by_symbol_avg
by_symbol_avg <- mutate(by_symbol,
  stats = map(data, function(x) {
    data_frame(
      mean_close = mean(x$close),
      mean_volume = mean(x$volume),
      annual_return = annual_return(x)
    )
  }))

library(dplyr)
library(trelliscopejs)

# Create a new data frame to use for plotting
pokemon2 <- pokemon %>%
  # Reduce the variables in the dataset
  select(pokemon, type_1, attack, generation_id, url_image) %>%
  mutate(
    # Respecify pokemon
    pokemon = cog(val = pokemon, default_label = TRUE),
    # Create panel variable
    panel = img_panel(url_image)
  )

# Create the display
trelliscope(pokemon2, name = "pokemon", nrow = 3, ncol = 6)

library(dplyr)
library(ggplot2)

# Compute daily counts
daily <- bike %>%
  group_by(start_day, weekday) %>%
  summarise(n = n())

# Plot the result
ggplot(daily, aes(start_day, n, color = weekday)) +
  geom_point()

library(dplyr)
library(ggplot2)

# Compute week_hod
week_hod <- bike %>%
  group_by(start_wk, start_hod, weekday) %>%
  summarise(n = n())

# Plot the result
ggplot(week_hod, aes(start_wk, n, color = weekday)) +
  geom_point() +
  facet_grid(~ start_hod) +
  scale_y_sqrt()

library(dplyr)
library(ggplot2)

# Compute wk_memb_hod
wk_memb_hod <- bike %>%
  group_by(start_wk, start_hod, weekday, membership) %>%
  summarize(n = n())

# Plot the result
ggplot(wk_memb_hod, aes(start_wk, n, color = weekday)) +
  geom_point() +
  facet_grid( membership~start_hod ) +
  scale_y_sqrt()

library(dplyr)
library(ggplot2)

# Compute daily_may
daily_may <- bike %>%
  filter(start_mon == 5) %>%
  group_by(start_day, start_hod, membership) %>%
  summarise(n = n())

# Plot the result
ggplot(daily_may, aes(start_hod, n, color = membership)) +
  geom_point() +
   facet_wrap(~start_day, ncol = 7)

library(ggplot2)

ggplot(daily_may, aes(start_hod, n, color = membership)) +
  geom_point() +
  # Facet on start_day
  facet_trelliscope(~start_day, nrow = 3, ncol = 7)

library(trelliscopejs)
library(ggplot2)
library(dplyr)

# Function to construct a Google maps URL with cycling directions
make_gmap_url <- function(start_lat, start_lon, end_lat, end_lon) {
  paste0("https://www.google.com/maps/dir/?api=1",
    "&origin=", start_lat, ",", start_lon,
    "&destination=", end_lat, ",", end_lon,
    "&travelmode=bicycling")
}

# Compute tot_rides, weekday_diff, and map_url
route_hod_updated <- route_hod %>% ungroup() %>%
  group_by(start_station_code, end_station_code) %>%
  mutate(
    tot_rides = sum(n),
    weekday_diff = mean(n[weekday == "workweek"]) - mean(n[weekday == "weekend"]),
    map_url = make_gmap_url(start_lat, start_lon, end_lat, end_lon))

library(trelliscopejs)
library(ggplot2)

# Create the plot
ggplot(route_hod, aes(start_hod, n, color = weekday)) +
  geom_point(size = 3) +
  facet_trelliscope(~ start_station_name + end_station_name, nrow = 2, ncol = 4) +
  theme(legend.position = "none")

