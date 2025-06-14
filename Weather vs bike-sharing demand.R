library(rvest)
library(dplyr)
library(janitor)
#Extract data from wikipedia##
url <- "https://en.wikipedia.org/wiki/List_of_bicycle-sharing_systems"
page <- read_html(url)
tables <- html_nodes(page, "table")
bike_table <-html_table(tables[[1]], fill = TRUE)

bike_df <- bike_table %>% clean_names()

write.csv(bike_df, "raw_bike_sharing_systems.csv", row.names = FALSE)

library(httr)
library(jsonlite)

# Set your API key and city
api_key <- "d0cd77165a8e793de065434fe16598c1"
city <- "Seoul"

response <- GET("https://api.openweathermap.org/data/2.5/weather",
                query = list(q = city, appid = api_key, units = "metric"))

print(status_code(response))
# Build request
current_weather_url <- 'https://api.openweathermap.org/data/2.5/weather'
current_query <- list(q = city, appid = api_key, units = "metric")

# API request
response <- GET(current_weather_url, query = current_query)
http_type(response)  # Should return "application/json"

# Parse JSON
json_result <- content(response, as = "parsed")

# Extract data
weather <- json_result$weather[[1]]$main
visibility <- json_result$visibility
temp <- json_result$main$temp
temp_min <- json_result$main$temp_min
temp_max <- json_result$main$temp_max
pressure <- json_result$main$pressure
humidity <- json_result$main$humidity
wind_speed <- json_result$wind$speed
wind_deg <- json_result$wind$deg

# Store in data frame
weather_data_frame <- data.frame(
  city = city,
  weather = weather, 
  visibility = visibility, 
  temp = temp, 
  temp_min = temp_min, 
  temp_max = temp_max, 
  pressure = pressure, 
  humidity = humidity, 
  wind_speed = wind_speed, 
  wind_deg = wind_deg
)

# Print result
print(weather_data_frame)

# Define function to get 5-day forecast for a list of cities
get_5_day_forecast <- function(cities, api_key) {
  forecast_url <- "https://api.openweathermap.org/data/2.5/forecast"
  
  get_city_forecast <- function(city) {
    query <- list(q = city, appid = api_key, units = "metric")
    response <- GET(forecast_url, query = query)
    
    if (http_type(response) != "application/json") {
      warning(paste("Failed for:", city))
      return(NULL)
    }
    
    json_data <- content(response, as = "parsed")
    
    # Extract forecast list
    forecast_df <- json_data$list %>%
      purrr::map_df(~{
        data.frame(
          city = city,
          datetime = .x$dt_txt,
          temperature = .x$main$temp,
          humidity = .x$main$humidity,
          weather = .x$weather[[1]]$main,
          wind_speed = .x$wind$speed,
          wind_deg = .x$wind$deg
        )
      })
    
    return(forecast_df)
  }
  
  # Loop through all cities and bind results
  result <- map_dfr(cities, get_city_forecast)
  return(result)
}

cities <- c("Seoul", "New York", "London", "Tokyo", "Paris")

forecast_df <- get_5_day_forecast(cities, api_key)

# Preview
head(forecast_df)

# Save to CSV
write.csv(forecast_df, "raw_cities_weather_forecast.csv", row.names = FALSE)


## dowload datasets as csv files from cloud###
# Download several datasets

# Download some general city information such as name and locations
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_worldcities.csv"
# download the file
download.file(url, destfile = "raw_worldcities.csv")

# Download a specific hourly Seoul bike sharing demand dataset
url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/raw_seoul_bike_sharing.csv"
# download the file
download.file(url, destfile = "raw_seoul_bike_sharing.csv")
 
##Module 2
library(readr)
library(stringr)
library(dplyr)
library(tidyr)

# Step 1: Clean column names in all CSVs
dataset_list <- c('raw_bike_sharing_systems.csv',
                  'raw_seoul_bike_sharing.csv',
                  'raw_cities_weather_forecast.csv',
                  'raw_worldcities.csv')

for (dataset_name in dataset_list) {
  dataset <- read_csv(dataset_name)
  colnames(dataset) <- toupper(colnames(dataset))
  colnames(dataset) <- str_replace_all(colnames(dataset), "\\s+", "_")
  write.csv(dataset, dataset_name, row.names = FALSE)
}

# Step 2: Read the bike-sharing dataset and preview
bike_sharing_df <- read.csv("raw_bike_sharing_systems.csv")
head(bike_sharing_df)
colnames(bike_sharing_df)

# Step 3: Select relevant columns
sub_bike_sharing_df <- bike_sharing_df %>%
  select(COUNTRY, CITY_REGION, SYSTEM, NAME)

# Step 4: Check column classes
sub_bike_sharing_df %>%
  summarize_all(class) %>%
  gather(variable, class)

# Step 5: Define helper function to detect non-numeric characters
find_character <- function(strings) grepl("[^0-9]", strings)

# Step 6: Preview non-numeric BICYCLES values
sub_bike_sharing_df %>%
  select(NAME) %>%
  filter(find_character(NAME)) %>%
  slice(1:10)

# Step 7: Define reference pattern detection and cleaning
ref_pattern <- "\\[[A-Za-z0-9]+\\]"
find_reference_pattern <- function(strings) grepl(ref_pattern, strings)

# Step 8: Check for reference links in COUNTRY, CITY, SYSTEM
sub_bike_sharing_df %>%
  select(COUNTRY) %>%
  filter(find_reference_pattern(COUNTRY)) %>%
  slice(1:10)

sub_bike_sharing_df %>%
  select(CITY_REGION) %>%
  filter(find_reference_pattern(CITY_REGION)) %>%
  slice(1:10)

sub_bike_sharing_df %>%
  select(SYSTEM) %>%
  filter(find_reference_pattern(SYSTEM)) %>%
  slice(1:10)

# Step 9: Create reference-cleaning function
remove_ref <- function(strings) {
  ref_pattern <- "\\[[A-Za-z0-9]+\\]"
  result <- str_replace_all(strings, ref_pattern, "")
  result <- str_trim(result)
  return(result)
}

# Step 10: Apply cleaning to COUNTRY, CITY, SYSTEM
cleaned_df <- sub_bike_sharing_df %>%
  mutate(
    COUNTRY = remove_ref(COUNTRY),
    CITY_REGION = remove_ref(CITY_REGION),
    SYSTEM = remove_ref(SYSTEM)
  )

# Step 11: Optional - Extract numeric part from BICYCLES column
cleaned_df <- cleaned_df %>%
  mutate(
    NAME_CLEAN = str_extract(NAME, "\\d+"),
    NAME_CLEAN = as.numeric(NAME_CLEAN)
  )

# Step 12: Verify cleanup
any(grepl(ref_pattern, cleaned_df$CITY_REGION))       # Should be FALSE
any(grepl(ref_pattern, cleaned_df$SYSTEM))     # Should be FALSE
any(grepl(ref_pattern, cleaned_df$COUNTRY))    # Should be FALSE

# Step 13: Check remaining messy rows (if any)
cleaned_df %>%
  filter(find_reference_pattern(CITY_REGION) |
           find_reference_pattern(SYSTEM) |
           find_reference_pattern(NAME)) %>%
  select(CITY_REGION, SYSTEM, NAME)

# Step 14: Save cleaned dataset
write.csv(cleaned_df, "cleaned_bike_sharing_systems.csv", row.names = FALSE)

#Extract the numeric value
library(stringr)
# Function to extract the first number and convert to numeric
extract_num <- function(columns) {
  digit_pattern <- "\\d+"  # Match one or more digits
  numeric_result <- str_extract(columns, digit_pattern)
  return(as.numeric(numeric_result))
}
cleaned_df <- cleaned_df %>%
  mutate(BICYCLES = extract_num(NAME)) 
summary(cleaned_df$BICYCLES)
write.csv(cleaned_df, "bike_sharing_systems.csv", row.names = FALSE)

##Module 3 library(readr)
library(DBI)
library(RSQLite)

# Step 1: Define the file URLs
urls <- list(
  SEOUL_BIKE_SHARING = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing.csv",
  CITIES_WEATHER_FORECAST = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/cities_weather_forecast.csv",
  BIKE_SHARING_SYSTEMS = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/bike_sharing_systems.csv",
  WORLD_CITIES = "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/world_cities.csv"
)

# Step 2: Download and load the CSVs
data_list <- list()

for (name in names(urls)) {
  # Download and read CSV into a data frame
  temp_file <- tempfile(fileext = ".csv")
  download.file(urls[[name]], destfile = temp_file, mode = "wb")
  data_list[[name]] <- read_csv(temp_file)
}

# Step 3: Create SQLite database connection
con <- dbConnect(RSQLite::SQLite(), dbname = "bike_sharing_project.db")

# Step 4: Write each data frame into the database as a table
for (name in names(data_list)) {
  dbWriteTable(con, name, data_list[[name]], overwrite = TRUE)
}

# Optional: List all tables to confirm
dbListTables(con)

# Optional: View one of the tables
head(dbReadTable(con, "SEOUL_BIKE_SHARING"))
dbListFields(con, "CITIES_WEATHER_FORECAST")
#MODule 3
#TASK 1 RECORD COUNT
dbGetQuery(con, 
           "SELECT COUNT(*) 
           FROM SEOUL_BIKE_SHARING")
#TASK2 OPERATIONAL HOURS
dbGetQuery(con, "SELECT COUNT (*)
           FROM SEOUL_BIKE_SHARING
           WHERE RENTED_BIKE_COUNT >0")
#TASK 3 WEATHER OUTLOOK
dbGetQuery(con, "
  SELECT *
  FROM CITIES_WEATHER_FORECAST
  WHERE LOWER(CITY) = 'seoul'
  ORDER BY FORECAST_DATETIME ASC
  LIMIT 1
")

#TASK 4 SEASONS IN THE SEOUL BIKE SHARING DATASET

dbGetQuery(con, "SELECT DISTINCT SEASONS
  FROM SEOUL_BIKE_SHARING
")
#TASK 5 DATA. RANGE
dbGetQuery(con, "
  SELECT MIN(DATE) AS first_date, MAX(DATE) AS last_date
  FROM SEOUL_BIKE_SHARING
")
# TASK 6 ALL-TIME HIGH RENTAL
dbGetQuery(con, "
  SELECT DATE, HOUR, RENTED_BIKE_COUNT
  FROM SEOUL_BIKE_SHARING
  ORDER BY RENTED_BIKE_COUNT DESC
  LIMIT 1
")
#TASK 7 HOURLY POPULARITY AND TEMPERATURE BY SEASON
dbGetQuery(con, "SELECT 
  SEASONS,
  HOUR,
  AVG(RENTED_BIKE_COUNT) AS avg_rentals,
  AVG(TEMPERATURE) AS avg_temp
FROM SEOUL_BIKE_SHARING
GROUP BY SEASONS, HOUR
ORDER BY avg_rentals DESC
LIMIT 10")

#TASK 8 RENTAL SEASONALITY
dbGetQuery(con, "
  SELECT 
    SEASONS,
    AVG(RENTED_BIKE_COUNT) AS avg_rentals,
    MIN(RENTED_BIKE_COUNT) AS min_rentals,
    MAX(RENTED_BIKE_COUNT) AS max_rentals,
    SQRT(AVG(RENTED_BIKE_COUNT * RENTED_BIKE_COUNT) - 
         AVG(RENTED_BIKE_COUNT) * AVG(RENTED_BIKE_COUNT)) AS stddev_rentals
  FROM SEOUL_BIKE_SHARING
  GROUP BY SEASONS;
")
#TASK 9 WEATHER SEASONALITY
dbGetQuery(con, "
  SELECT 
    SEASONS,
    AVG(TEMPERATURE) AS avg_temperature,
    AVG(HUMIDITY) AS avg_humidity,
    AVG(WIND_SPEED) AS avg_wind_speed,
    AVG(VISIBILITY) AS avg_visibility,
    AVG(DEW_POINT_TEMPERATURE) AS avg_dew_point_temp,
    AVG(SOLAR_RADIATION) AS avg_solar_radiation,
    AVG(RAINFALL) AS avg_rainfall,
    AVG(SNOWFALL) AS avg_snowfall,
    AVG(RENTED_BIKE_COUNT) AS avg_bike_count
  FROM SEOUL_BIKE_SHARING
  GROUP BY SEASONS
  ORDER BY avg_bike_count DESC
")

#TASK 10 TOTAL BIKE COUNT AND CITY INFO FOR SEOUL
colnames(WORLD_CITIES)
dbGetQuery(con, "
  SELECT 
    wc.CITY,
    wc.COUNTRY,
    wc.LAT,
    wc.LNG,
    wc.POPULATION,
    SUM(CAST(bss.BICYCLES AS INTEGER)) AS TOTAL_BIKES
  FROM 
    BIKE_SHARING_SYSTEMS bss
  JOIN 
    WORLD_CITIES wc 
  ON 
    LOWER(wc.CITY) = LOWER(bss.CITY)
  WHERE 
    LOWER(wc.CITY) = 'seoul'
  GROUP BY 
    wc.CITY, wc.COUNTRY, wc.LAT, wc.LNG, wc.POPULATION;
")
#TASK 11
dbGetQuery(con, "
  SELECT 
    wc.CITY,
    wc.COUNTRY,
    wc.LAT,
    wc.LNG,
    wc.POPULATION,
    CAST(bss.BICYCLES AS INTEGER) AS TOTAL_BIKES
  FROM 
    BIKE_SHARING_SYSTEMS bss
  JOIN 
    WORLD_CITIES wc 
  ON 
    LOWER(wc.CITY) = LOWER(bss.CITY)
  WHERE 
    CAST(bss.BICYCLES AS INTEGER) BETWEEN 15000 AND 20000;
")
# Module 4
library(tidymodels)
library(tidyverse)
library(stringr)
# Dataset URL
dataset_url <- "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBMDeveloperSkillsNetwork-RP0321EN-SkillsNetwork/labs/datasets/seoul_bike_sharing_converted_normalized.csv"
bike_sharing_df <- read_csv(dataset_url)
spec(bike_sharing_df)
bike_sharing_df <- bike_sharing_df %>% 
  select(-DATE, -FUNCTIONING_DAY)
set.seed(1234)
split <- initial_split(bike_sharing_df, prop = 0.75)

train_data <- training(split)
test_data  <- testing(split)

weather_recipe <- recipe(RENTED_BIKE_COUNT ~ TEMPERATURE + HUMIDITY + WIND_SPEED +
                           VISIBILITY + DEW_POINT_TEMPERATURE + SOLAR_RADIATION +
                           RAINFALL + SNOWFALL, data = train_data)
lm_model_weather <- linear_reg()%>%
  set_engine("lm")%>%
  set_mode("regression")
lm_workflow_weather <- workflow() %>%
  add_recipe(weather_recipe)%>%
  add_model(lm_model_weather)

fitted_weather_model <- lm_workflow_weather %>%
  fit(data=train_data)
summary(fitted_weather_model$fit)
summary(fitted_weather_model)
summary(fitted_weather_model$fit$fit$fit)

all_vars_recipe <- recipe(RENTED_BIKE_COUNT ~ ., data = train_data)
lm_model_all <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")
lm_workflow_all <- workflow() %>%
  add_recipe(all_vars_recipe) %>%
  add_model(lm_model_all)

fitted_model_all <- lm_workflow_all %>%
  fit(data = train_data)
summary(fitted_model_all$fit$fit$fit)


test_results_weather <- predict(fitted_weather_model, test_data) %>%
  bind_cols(test_data)

test_results_all <- predict(fitted_model_all, test_data) %>%
  bind_cols(test_data)

# Metrics for weather model
weather_metrics <- test_results_weather %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

# Metrics for all-variable model
all_metrics <- test_results_all %>%
  metrics(truth = RENTED_BIKE_COUNT, estimate = .pred)

# View results
weather_metrics
all_metrics

# Extract coefficients
coeffs <- coef(fitted_model_all$fit$fit$fit)

# Convert to a data frame
coeff_df <- as.data.frame(coeffs)
coeff_df$term <- rownames(coeff_df)
colnames(coeff_df)[1] <- "coefficient"

# Remove intercept, and sort by absolute value
coeff_df_sorted <- coeff_df %>%
  filter(term != "(Intercept)") %>%
  mutate(abs_coef = abs(coefficient)) %>%
  arrange(desc(abs_coef))

library(ggplot2)
ggplot(coeff_df_sorted, aes(x = reorder(term, abs_coef), y = coefficient)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance (Coefficients)",
    x = "Predictor Variable",
    y = "Coefficient"
  ) +
  theme_minimal(base_size = 13)

##basline regression models 
library(ggplot2)

ggplot(data = train_data, aes(x = TEMPERATURE, y = RENTED_BIKE_COUNT)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red") +              # linear
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "orange") +  # degree 2
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "green") +   # degree 4
  geom_smooth(method = "lm", formula = y ~ poly(x, 6), color = "blue") +    # degree 6
  labs(
    title = "Polynomial Fits for Bike Count vs. Temperature",
    x = "Temperature (°C)",
    y = "Rented Bike Count"
  ) +
  theme_minimal()
# Fit a linear model with higher order polynomial on some important variables 

# #HINT: Use ploy function to build polynomial terms, lm_poly <- RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 6) + poly(HUMIDITY, 4) .....
library(tidymodels)
lm_poly <- linear_reg() %>%
  set_engine("lm") %>%
  fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 2) + poly(HUMIDITY, 2) + poly(RAINFALL, 2), 
      data = train_data)

summary(lm_poly$fit)
# Predict using lm_poly model
test_results_poly <- predict(lm_poly, test_data) %>%
  bind_cols(test_data) %>%
  rename(.pred = .pred)
# Ensure no negative values
test_results_poly <- test_results_poly %>%
  mutate(.pred = ifelse(.pred < 0, 0, .pred))
# Evaluate model
rsq_poly <- rsq(test_results_poly, truth = RENTED_BIKE_COUNT, estimate = .pred)
rmse_poly <- rmse(test_results_poly, truth = RENTED_BIKE_COUNT, estimate = .pred)

# Print metrics
rsq_poly
rmse_poly

# Fit polynomial model with interaction term
lm_poly_interaction <- linear_reg() %>%
  set_engine("lm") %>%
  fit(RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 2) + poly(HUMIDITY, 2) + RAINFALL * HUMIDITY,
      data = train_data)
summary(lm_poly_interaction$fit)
# Load yardstick for metrics if not already loaded
library(yardstick)

# Generate predictions on test_data
test_results_interaction <- test_data %>%
  mutate(pred = predict(lm_poly_interaction, test_data)$.pred)

# Replace negative predictions with 0 (since negative bike counts aren't realistic)
test_results_interaction <- test_results_interaction %>%
  mutate(pred = ifelse(pred < 0, 0, pred))

# Calculate R-squared
rsq_interaction <- rsq(truth = RENTED_BIKE_COUNT, estimate = pred, data = test_results_interaction)

# Calculate RMSE
rmse_interaction <- rmse(truth = RENTED_BIKE_COUNT, estimate = pred, data = test_results_interaction)

# Print results
rsq_interaction
rmse_interaction
colnames(train_data)

library(tidymodels)
library(glmnet)
glmnet_spec <- linear_reg(penalty = 0.1, mixture = 0.5) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

model_formula <- RENTED_BIKE_COUNT ~ poly(TEMPERATURE, 2) + poly(HUMIDITY, 2) +
  poly(DEW_POINT_TEMPERATURE, 2) + poly(RAINFALL, 2) + 
  RAINFALL*HUMIDITY + SEASONS*HOLIDAY

lm_glmnet <- fit(glmnet_spec, model_formula, data = train_data)
test_results_glmnet <- predict(lm_glmnet, test_data) %>%
  bind_cols(test_data) %>%
  rename(.pred = .pred)
rsq_glmnet <- rsq(test_results_glmnet, truth = RENTED_BIKE_COUNT, estimate = .pred)
rmse_glmnet <- rmse(test_results_glmnet, truth = RENTED_BIKE_COUNT, estimate = .pred)

print(rsq_glmnet)
print(rmse_glmnet)

