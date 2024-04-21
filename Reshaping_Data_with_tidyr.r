head(netflix_df)

netflix_df %>% 
  # Split the duration column into value and unit columns
  separate(duration, into = c("value", "unit"), sep = " ", convert = TRUE) #convert = TRUE makes the string an integer

head(phone_nr_df)

phone_nr_df %>%
  # Unite the country_code and national_number columns
  unite("international_number", country_code, national_number, sep = "")

head(tvshow_df)

tvshow_df %>% 
  # Separate the actors in the cast column over multiple rows
  separate_rows(cast, sep = ", ") %>% 
  rename(actor = cast) %>% 
  count(actor, sort = TRUE) %>% 
  head(6)

head(drink_df)

drink_df %>% 
  # Separate the ingredients over rows
  separate_rows(ingredients, sep = "; ") %>% 
  # Separate ingredients into three columns
  separate(
    ingredients, 
    into = c("ingredient", "quantity", "unit"), 
    sep = " ", 
    convert = TRUE
  ) %>% 
  # Group by ingredient and unit
  group_by(ingredient, unit) %>% 
  # Calculate the total quantity of each ingredient
  summarize(quantity = sum(quantity))

director_df %>% 
  # Drop rows with NA values in the director column
  drop_na(director) %>% 
  # Spread the director column over separate rows
  separate_rows(director, sep = ", ") %>% 
  # Count the number of movies per director
  count(director, sort = TRUE)

head(sales_df)

sales_df %>% 
  # Impute the year column
  fill(year, .direction = "up") %>%
  # Create a line plot with sales per quarter colored by year.
  ggplot(aes(x = quarter, y = sales, color = year, group = year)) +
  geom_line()

country_to_continent_df %>% 
  left_join(nuke_df, by = "country_code") %>%  
  # Impute the missing values in the n_bombs column with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Group the dataset by continent
  group_by(continent) %>% 
  # Sum the number of bombs per continent
  summarize(n_bombs_continent = sum(n_bombs)) %>% 
  # Plot the number of bombs per continent
  ggplot(aes(y = n_bombs_continent, x = continent)) +
  geom_col()

nuke_df %>% 
  # Pivot the data to a longer format
  pivot_longer(
    -year, 
    # Overwrite the names of the two new columns
    names_to = "country", 
    values_to = "n_bombs"
  ) %>% 
  # Replace NA values for n_bombs with 0L
  replace_na(list(n_bombs = 0L)) %>% 
  # Plot the number of bombs per country over time
  ggplot(aes(x = year, y = n_bombs, color = country)) +
  geom_line()

obesity_df %>% 
  # Pivot the male and female columns
  pivot_longer(c(male, female),
               names_to = "sex",
               values_to = "pct_obese") %>% 
  # Create a scatter plot with pct_obese per country colored by sex
  ggplot(aes(x = pct_obese, color = sex,
             y = forcats::fct_reorder(country, both_sexes))) +
  geom_point() +
  scale_y_discrete(breaks = c("India", "Nauru", "Cuba", "Brazil",
                              "Pakistan", "Gabon", "Italy", "Oman",
                              "China", "United States of America")) +
  labs(x = "% Obese", y = "Country")

bond_df %>% 
  # Pivot the data to long format
  pivot_longer(
    -Bond, 
    # Overwrite the names of the two newly created columns
    names_to = "decade", 
    values_to = "n_movies", 
    # Drop na values
    values_drop_na = TRUE, 
    # Transform the decade column data type to integer
    names_transform = list(decade = as.integer)
  ) %>% 
  ggplot(aes(x = decade + 5, y = n_movies, fill = Bond))+
  geom_col()

bird_df %>%
  # Pivot the data to create a 2 column data frame
  pivot_longer(
    starts_with("points_"),
    names_to = "points",
    names_prefix = "points_",
    names_transform = list(points = as.integer),
    values_to = "species",
    values_drop_na = TRUE
  ) %>%
  group_by(species) %>% 
  summarize(total_points = sum(points)) %>% 
  slice_max(total_points, n = 5)

stock_df %>% 
  # Pivot the data to create 3 new columns: year, week, price
  pivot_longer(
    -company,
    names_to = c("year", "week"),
    values_to = "price",
    names_sep = "_week",
    names_transform = list(
      year = as.integer,
      week = as.integer)
  ) %>%
  # Create a line plot with price per week, color by company
  ggplot(aes(y = price, x = week, color = company)) +
  geom_line() +
  facet_grid(. ~ year)

space_dogs_df %>% 
  pivot_longer(
    # Add the columns to pivot
    c(name_1, name_2, gender_1, gender_2),
    names_sep = "_",
    # Complete the names_to argument to re-use the first part of the column headers
    names_to = c(".value",  "dog_id"),
    # Make sure NA values are dropped
    values_drop_na = TRUE
  )

who_df %>% 
  # Put each variable in its own column
  pivot_longer(
    -country,
    names_to = c("year", "sex", ".value"),
    names_sep = "_", 
    names_transform = list("year" = as.integer)
  ) %>%
  # Create a plot with life expectancy over obesity
  ggplot(aes(x = pct.obese, y = life.exp, color = sex)) + geom_point()

dog_df %>% 
  # Create one row for each participant and add the id
  uncount(n_participants, .id = "dog_id")

space_dogs_df %>% 
  # Pivot the data to a wider format
  pivot_wider(names_from = dog_id, values_from = gender, names_prefix = "gender_") %>% 
  # Drop rows with NA values
  drop_na() %>% 
  # Create a Boolean column on whether both dogs have the same gender
  mutate(same_gender = gender_1 == gender_2) %>% 
  summarize(pct_same_gender = mean(same_gender))

planet_df %>% 
  # Give each planet variable its own column
  pivot_wider(names_from = "metric", values_from = "value") %>% 
  # Plot planet temperature over distance to sun
  ggplot(aes(x = distance_to_sun, y = temperature)) +
  geom_point(aes(size = diameter)) +
  geom_text(aes(label = planet), vjust = -1) +
  labs(x = "Distance to sun (million km)", 
       y = "Mean temperature (°C)") +
  theme(legend.position = "none")

planet_df %>%
  # Pivot all columns except metric to long format
  pivot_longer(-metric, names_to = "planet") %>% 
  # Put each metric in its own column
  pivot_wider(names_from = metric, values_from = value) %>% 
  # Plot the number of moons vs. planet diameter
  ggplot(aes(x = diameter, y = number_of_moons)) +
  geom_point(aes(size = diameter)) +
  geom_text(aes(label = planet), vjust = -1) +
  labs(x = "Diameter (km)", y = "Number of moons") +
  theme(legend.position = "none")

letters <- c("A", "C", "G", "U")

# Create a tibble with all possible 3 way combinations
codon_df <- expand_grid(
  letter1 = letters,
  letter2 = letters,
  letter3 = letters
)

codon_df %>% 
  # Unite these three columns into a "codon" column
  unite("codon" , letter1, letter2, letter3, sep = "" )

# Create a tibble with all combinations of years and species
full_df <- expand_grid(
  year = 1951:1970, 
  species = c("Human", "Dog")
)

space_df %>% 
  # Join with full_df so that missing values are introduced
  right_join(full_df, by = c("year", "species")) %>% 
  # Overwrite NA values for n_in_space with 0L
  replace_na(list(n_in_space = 0L)) %>% 
  # Create a line plot with n_in_space over year, color by species
  ggplot(aes(x = year, y = n_in_space, color = species)) +
  geom_line()

# Create a tibble with all combinations of dates and reactors
full_df <- expand_grid(date = dates, reactor = reactors)

# Find the reactor - date combinations not present in reactor_df
full_df %>% 
  anti_join(reactor_df, by = c("date", "reactor"))

planets = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")

planet_df %>% 
  complete(
    # Complete the planet variable
    planet = planets,
    # Overwrite NA values for n_moons with 0L
    fill = list(n_moons = 0L))

medal_df %>% 
  # Count the medals won per team and year
  count(team, year, name = "n_medals") %>% 
  # Complete the team and year variables, fill n_medals with zeros
  complete(team, year, fill = list(n_medals = 0L)) %>% 
  # Plot n_medals over year, colored by team
  ggplot(aes(x = year, y = n_medals, color = team)) +
  geom_line() +
  scale_color_brewer(palette = "Paired")

# Generate all years from 2020 to 2030
years <- full_seq(c(2020,2030), period = 1)
years

# Generate all decades from 1980 to 2030
decades <- full_seq(c(1980, 2030), period = 10)
decades

outer_dates <- c(as.Date("1980-01-01"), as.Date("1980-12-31"))

# Generate the dates for all days in 1980
full_seq(outer_dates, period = 1)

cumul_nukes_1962_df %>% 
  # Complete the dataset
  complete(country, date = full_seq(date, period = 1)) %>% 
  # Group the data by country
  group_by(country) %>% 
  # Impute missing values with the last known observation
  fill(total_bombs) %>% 
  # Plot the number of bombs over time, color by country
  ggplot(aes(x = date, y = total_bombs, color = country)) +
  # These two lines will mark the Cuban Missile Crisis 
  geom_rect(xmin = as.Date("1962-10-16"), xmax = as.Date("1962-10-29"), ymin = -Inf, ymax = Inf, color = NA)+ 
  geom_text(x = as.Date("1962-10-22"), y = 15, label = "Cuban Missile Crisis", angle = 90, color = "white")+
  geom_line()

medal_df %>% 
  # Give each continent an observation at each Olympic event
  complete(
    continent, 
    nesting(season, year), 
    fill = list(medals_per_participant = 0L)
  ) %>%
  # Plot the medals_per_participant over time, colored by continent
  ggplot(aes(x = year, y = medals_per_participant, color = continent)) +
  geom_line() +
  facet_grid(season ~ .)

patient_df %>% 
  # Pivot the infected and recovered columns to long format
  pivot_longer(-patient, names_to = "status", values_to = "date") %>% 
  select(-status) %>% 
  # Group by patient
  group_by(patient) %>% 
  # Complete the date range per patient using full_seq()
  complete(date = full_seq(date, period = 1)) %>% 
  # Ungroup the data
  ungroup() %>% 
  # Count the dates, the count goes in the n_sick variable
  count(date, name = "n_sick") %>% 
  ggplot(aes(x = date, y = n_sick))+
  geom_line()

sensor_df %>% 
  # Complete the time column with a 20 minute interval
  complete(time = seq(min(time), max(time), by = "20 min"),
           fill = list(enter = 0L, exit = 0L)) %>%
  # Calculate the total number of people inside
  mutate(total_inside = cumsum(enter + exit)) %>% 
  # Pivot the enter and exit columns to long format
  pivot_longer(enter:exit, names_to = "direction", values_to = "n_people") %>% 
  # Plot the number of people over time, fill by direction
  ggplot(aes(x = time, y = n_people, fill = direction)) +
  geom_area() +
  geom_line(aes(y = total_inside))

# Create a movie column from the movie_list
tibble(movie = movie_list) %>% 
  # Unnest the movie column
  unnest_wider(movie)

# Create a tibble with a movie column
tibble(movie = movie_planets_list) %>% 
  # Unnest the movie column
  unnest_wider(movie) %>% 
  # Unnest the planets column
  unnest_wider(planets)

# Create a tibble from movie_planets_list
tibble(movie = movie_planets_list) %>% 
  # Unnest the movie column in the correct direction
  unnest_wider(movie) %>% 
  # Unnest the planets column in the correct direction
  unnest_longer(planets)

planet_df %>% 
  # Unnest the moons list column over observations
  unnest_longer(moons) %>% 
  # Further unnest the moons column
  unnest_wider(moons) %>% 
  # Unnest the moon_data column
  unnest_wider(moon_data) %>% 
  # Get the top five largest moons by radius
  slice_max(radius, n = 5)

character_df %>% 
  # Unnest the metadata column
  unnest_wider(metadata) %>% 
  # Unnest the films column
  unnest_longer(films)

character_df %>% 
  hoist(metadata, first_film = list("films", 1))

movie_df %>% 
  # Unnest the movie column
  unnest_wider(movie) %>% 
  select(Title, Year, Ratings) %>% 
  # Unnest the Ratings column
  unnest_wider(Ratings)

movie_df %>% 
  hoist(
    movie,
    title = "Title",
    year = "Year",
    rating = list("Ratings", "Rotten Tomatoes", 1)
  )

model <- lm(weight_kg ~ waist_circum_m 
            + stature_m, data = ansur_df)

glance(model)
tidy(model)

ansur_df %>% 
  # Group the data by branch, then nest
  group_by(branch) %>% 
  nest()

ansur_df %>% 
  # Group the data by branch and sex, then nest
  group_by(branch, sex) %>% 
  nest()

ansur_df %>%
  # Group the data by sex
  group_by(sex) %>% 
  # Nest the data
  nest() %>% 
  mutate(
    fit = map(data, function(df) lm(weight_kg ~ waist_circum_m + stature_m, data = df)),
    glanced = map(fit, glance)
  ) %>% 
  # Unnest the glanced column
  unnest(glanced)









