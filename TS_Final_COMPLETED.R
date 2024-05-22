####################
#Data found here - https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results
###################



dataPath <- "/Users/benderrusso/Desktop/rstudio_stuff/TimeSeries/Olympics"
data <- read.csv(paste(dataPath,'athlete_events.csv',sep = '/'), header=TRUE)
noc <- read.csv(paste(dataPath,'noc_regions.csv',sep = '/'), header=TRUE)

library(dplyr)
library(ggplot2)
library(forecast)
#install.packages("reshape2")
library(reshape2)
library(zoo)
#install.packages("fracdiff")
library(fracdiff)
library(tseries)

#Drop the 'notes' column
noc_country <- noc %>% select(-notes)

#Rename the 'region' column to 'Country'
names(noc_country)[names(noc_country) == "region"] <- "Country"

#Merging NOC and Olympic Data 
olympics_merge <- left_join(data, noc_country, by = "NOC")

#Filter rows where 'Country' is NA and select distinct 'NOC' and 'Team'
result <- olympics_merge %>%
  filter(is.na(Country)) %>%
  select(NOC, Team) %>%
  distinct()

# Replace missing 'Country' values with 'Team' values
olympics_merge <- olympics_merge %>%
  mutate(Country = ifelse(is.na(Country), Team, Country))

# Update 'Country' based on 'NOC' values
olympics_merge <- olympics_merge %>%
  mutate(Country = ifelse(NOC == 'SGP', 'Singapore', Country),
         Country = ifelse(NOC == 'ROT', 'Refugee Olympic Athletes', Country),
         Country = ifelse(NOC == 'UNK', 'Unknown', Country),
         Country = ifelse(NOC == 'TUV', 'Tuvalu', Country))

# Drop the 'Team' column and rename 'Country' to 'Team'
olympics_merge <- olympics_merge %>%
  select(-Team) %>%
  rename(Team = Country)

unique_countries <- unique(olympics_merge$Team)
print(unique_countries)

#Remove rows with missing values in the 'Medal' column
olympics_data <- olympics_merge %>% filter(!is.na(Medal))

# Separate out Summer and Winter Olympics
summer_olympics <- olympics_data %>% filter(Season == "Summer")

# Remove Age, Weight, Height, and Name columns
summer_olympics <- summer_olympics %>% select(-c(Age, Weight, Height, Name, Sex, NOC, Games))


df <- summer_olympics %>% select(Team, Medal, Year, City)

# Create a complete sequence of specific Olympic years
all_years <- seq(1896, 2016, by = 4)

# Get unique teams
unique_teams <- unique(df$Team)

# Create a complete grid of every combination of year and team
complete_grid <- expand.grid(Year = all_years, Team = unique_teams)

# Group by Team, City, Year, and Season, count the medals
medal_counts <- df %>%
  group_by(Team, City, Year) %>%
  summarize(medal_count = n(), .groups = 'drop') %>%
  arrange(Year)

# Merge this grid with the medal counts
medal_counts_complete <- merge(complete_grid, medal_counts, by = c("Year", "Team"), all.x = TRUE)

# Create a mapping of years to cities and seasons
year_city <- df %>%
  select(Year, City) %>%
  distinct()

# Merge this mapping with the complete data frame to fill in the missing City and Season values
medal_counts_complete <- merge(medal_counts_complete, year_city, by = "Year", all.x = TRUE, suffixes = c("", ".y"))

# Use the mapped values to fill in missing City values
medal_counts_complete$City[is.na(medal_counts_complete$City)] <- medal_counts_complete$City.y[is.na(medal_counts_complete$City)]

# Drop the extra columns used for merging
medal_counts_complete <- medal_counts_complete %>% select(-City.y)

# Replace NA values in medal_count with 0
medal_counts_complete$medal_count[is.na(medal_counts_complete$medal_count)] <- 0


medal_counts_2020 <- data.frame(
  Team = c("USA", "Japan", "UK", "Russia", "Germany"),
  medal_count = c(113, 58, 65, 71, 37),
  City = "Tokyo",
  Year = 2020
)

print(medal_counts_2020)

# Append the 2020 data to the medal_counts_complete data frame
medal_counts_complete <- bind_rows(medal_counts_complete, medal_counts_2020)

#Removing duplicate medal counts
medal_counts_complete <- medal_counts_complete %>% distinct(Year, Team, .keep_all = TRUE)

# Add columns for geopolitical events
medal_counts_complete <- medal_counts_complete %>%
  mutate(
    GeopoliticalEvents = ifelse(
      (Year == 1936 & Team %in% c("USA", "Germany", "Japan", "Russia", "UK")) |
        (Year == 1948 & Team %in% c("USA", "Germany", "Japan", "Russia", "UK")) |
        (Year == 1972 & Team %in% c("USA", "Germany", "Russia", "UK")) |
        (Year == 1980 & Team %in% c("USA", "Germany", "Russia", "UK", "Japan")) |
        (Year == 2020 & Team %in% c("USA", "Germany", "Russia", "UK", "Japan")),
      1, 0)
  )


# Split train and test
medal_counts_complete_train <- medal_counts_complete %>%
  filter(!Year %in% c(2016, 2020))

medal_counts_complete_test <- medal_counts_complete %>%
  filter(Year %in% c(2016, 2020))


# List of countries to forecast
countries_to_forecast <- c("USA","Germany", "Russia", "UK", "Japan")

#Function for accuracy calculations
calculate_accuracy <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))
  mse <- mean((actual - predicted)^2)
  rmse <- sqrt(mse)
  return(list(MAE = mae, MSE = mse, RMSE = rmse))
}



#################
# Function to fit ARIMA model and forecast 2016 & 2020 for each country (TEST)
forecast_country_medals_arima <- function(country_code) {
  # Subset data for the country
  country_data_train <- medal_counts_complete_train %>% filter(Team == country_code)
  country_data_test <- medal_counts_complete_test %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data_train) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Convert to time series
  ts_data <- ts(country_data_train$medal_count, start = min(country_data_train$Year), frequency = 0.25) #changing to 0.25 due to data being every 4 years (1/4)
  
  # Convert exogenous variables to a numeric matrix
  xreg <- as.matrix(country_data_train$GeopoliticalEvents)
  
  # Fit ARIMA model with exogenous variables
  model <- auto.arima(ts_data, xreg = xreg)
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(model))
  
  # Convert future exogenous variables to a numeric matrix
  future_xreg <- as.matrix(country_data_test$GeopoliticalEvents)
  
  # Forecast future medals
  forecasted_values <- forecast(model, xreg = future_xreg)
  
  # Ensure no negative values
  forecasted_values$mean[forecasted_values$mean < 0] <- 0
  forecasted_values$lower[forecasted_values$lower < 0] <- 0
  forecasted_values$upper[forecasted_values$upper < 0] <- 0
  
  print(round(forecasted_values$mean))
  print(country_data_test$medal_count)
  
  # Plot the forecast
  plot <- autoplot(forecasted_values) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count")
  
  # Calculate accuracy separately for 2016 and 2020
  accuracy_2016 <- calculate_accuracy(
    actual = country_data_test %>% filter(Year == 2016) %>% pull(medal_count),
    predicted = forecasted_values$mean[which(country_data_test$Year == 2016)]
  )
  
  accuracy_2020 <- calculate_accuracy(
    actual = country_data_test %>% filter(Year == 2020) %>% pull(medal_count),
    predicted = forecasted_values$mean[which(country_data_test$Year == 2020)]
  )
  
  return(list(model, plot, round(forecasted_values$mean), country_data_test$medal_count, list(accuracy_2016, accuracy_2020)))
}

forecast_test_arima <- list()
actual_medal_arima <- list()
model_arima <- list()
accuracy_arima <- list()


for (country in countries_to_forecast) {
  result <- forecast_country_medals_arima(country)
  if (!is.null(result)) {
    model_arima[[country]] <- result[[1]]
    forecast_test_arima[[country]] <- result[[3]]
    actual_medal_arima[[country]] <- result[[4]]
    accuracy_arima[[country]] <- result[[5]]
    print(result[[2]])
  }
}

# Print forecast, actual values, and accuracies for 2016 and 2020
for (country in countries_to_forecast) {
  cat("\nCountry:", country)
  cat("\nForecasted Medals for 2016 & 2020:", forecast_test_arima[[country]])
  cat("\nActual Medals for 2016 & 2020:", actual_medal_arima[[country]])
  cat("\nAccuracy for 2016 - MAE:", accuracy_arima[[country]][[1]]$MAE, "MSE:", accuracy_arima[[country]][[1]]$MSE, "RMSE:", accuracy_arima[[country]][[1]]$RMSE)
  cat("\nAccuracy for 2020 - MAE:", accuracy_arima[[country]][[2]]$MAE, "MSE:", accuracy_arima[[country]][[2]]$MSE, "RMSE:", accuracy_arima[[country]][[2]]$RMSE)
}



###################################
# Function to fit ARIMA model and forecast 2024 for each country
forecast_arima_2024 <- function(country_code) {
  country_data <- medal_counts_complete %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Convert to time series
  ts_data <- ts(country_data$medal_count, start = min(country_data$Year), frequency = 0.25) #changing to 0.25 due to data being every 4 years (1/4)
  
  # Convert exogenous variables to a numeric matrix
  xreg_x <- as.matrix(country_data$GeopoliticalEvents)
  
  # Fit ARIMA model with exogenous variables
  model <- auto.arima(ts_data, xreg = xreg_x)
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(model))
  
  # Example future geopolitical events (to be used in the forecast)
  future_geo <- data.frame(
    Year = c(2024),
    GeopoliticalEvents = c(0)
  )
  
  # Convert future exogenous variables to a numeric matrix
  future_xreg <- as.matrix(future_geo$GeopoliticalEvents)
  
  # Forecast future medals
  forecasted_values <- forecast(model, xreg = future_xreg)
  
  # Plot the forecast
  plot <- autoplot(forecasted_values) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count")
  
  return(list(model, plot, round(forecasted_values$mean)))
}

forecast_2024_arima <- list()
model_2024_arima <- list()

# Loop through each country and generate forecasts
for (country in countries_to_forecast) {
  result <- forecast_arima_2024(country)
  if (!is.null(result)) {
    model_2024_arima[[country]] <- result[[1]]
    forecast_2024_arima[[country]] <- result[[3]]
    print(result[[2]])
  }
}

print(data.frame(forecast_2024_arima))

###################################


# Function to fit Arfima and forecast 2016 & 2020 for each country (TEST)
forecast_country_medals_arfima <- function(country_code) {
  # Subset data for the country
  country_data_train <- medal_counts_complete_train %>% filter(Team == country_code)
  country_data_test <- medal_counts_complete_test %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data_train) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Convert to time series
  ts_data_train <- ts(country_data_train$medal_count, start = min(country_data_train$Year), frequency = 0.25) #changing to 0.25 due to data being every 4 years (1/4)
  ts_data_test <- ts(country_data_test$medal_count, start = min(country_data_test$Year), frequency = 0.25) #changing to 0.25 due to data being every 4 years (1/4)
  
  # Convert exogenous variables to a numeric matrix
  xreg <- as.matrix(country_data_train$GeopoliticalEvents)
  
  # Fit ARIMA model with exogenous variables using fracdiff 
  d_train <- fracdiff(ts_data_train)
  st_train <- diffseries(ts_data_train, d_train$d)
  model <- auto.arima(st_train, xreg = xreg)
  
  d_test <- fracdiff(ts_data_test)
  st_test <- diffseries(ts_data_test, d_test$d) #differentiated medal count which will be used in accuracy calculations below
  
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(model))
  
  # Forecast future medals
  forecasted_values <- forecast(model, xreg = country_data_test$GeopoliticalEvents, h = 1)  
  
  # Ensure no negative values
  forecasted_values$mean[forecasted_values$mean < 0] <- 0
  forecasted_values$lower[forecasted_values$lower < 0] <- 0
  forecasted_values$upper[forecasted_values$upper < 0] <- 0
  
  # Plot the forecast
  plot <- autoplot(forecasted_values) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count")
  
  # Calculate accuracy separately for 2016 and 2020
  accuracy_2016 <- calculate_accuracy(
    actual = st_test[1],
    predicted = forecasted_values$mean[1]
  )
  
  accuracy_2020 <- calculate_accuracy(
    actual = st_test[2],
    predicted = forecasted_values$mean[2]
  )
  
  return(list(model, plot, round(forecasted_values$mean), country_data_test$medal_count, list(accuracy_2016, accuracy_2020)))
}

forecast_test_arfima <- list()
actual_medal_arfima <- list()
model_arfima <- list()
accuracy_arfima <- list()

for (country in countries_to_forecast) {
  result <- forecast_country_medals_arfima(country)
  if (!is.null(result)) {
    model_arfima[[country]] <- result[[1]]
    forecast_test_arfima[[country]] <- result[[3]]
    actual_medal_arfima[[country]] <- result[[4]]
    accuracy_arfima[[country]] <- result[[5]]
    print(result[[2]])
  }
}

# Print forecast, actual values, and accuracies for 2016 and 2020
for (country in countries_to_forecast) {
  cat("\nCountry:", country)
  cat("\nForecasted Medals for 2016 & 2020:", forecast_test_arfima[[country]])
  cat("\nActual Medals for 2016 & 2020:", actual_medal_arfima[[country]])
  cat("\nAccuracy for 2016 - MAE:", accuracy_arfima[[country]][[1]]$MAE, "MSE:", accuracy_arfima[[country]][[1]]$MSE, "RMSE:", accuracy_arfima[[country]][[1]]$RMSE)
  cat("\nAccuracy for 2020 - MAE:", accuracy_arfima[[country]][[2]]$MAE, "MSE:", accuracy_arfima[[country]][[2]]$MSE, "RMSE:", accuracy_arfima[[country]][[2]]$RMSE)
}
  

##################################
#######################################
# Function to fit ARFIMA model and forecast 2024 for each country
forecast_arfima_2024 <- function(country_code) {
  country_data <- medal_counts_complete %>% filter(Team == country_code)
  
  # Check if there are any observations
  if (nrow(country_data) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Convert to time series
  ts_data <- ts(country_data$medal_count, start = min(country_data$Year), frequency = 0.25)
  
  # Fit ARFIMA model
  fracts <- fracdiff(ts_data)
  diffts <- diffseries(ts_data, fracts$d)
  arfima_model <- auto.arima(diffts, xreg = country_data$GeopoliticalEvents)

  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(arfima_model))
  
  # Example future geopolitical events (to be used in the forecast)
  future_geo_arfima <- data.frame(
    Year = c(2024),
    GeopoliticalEvents = c(1)
  )
  
  # Convert future exogenous variables to a numeric matrix
  future_xreg <- as.matrix(future_geo_arfima$GeopoliticalEvents)
  
  # Forecast future medals
  arfima_forecast <- forecast(arfima_model, xreg = future_xreg, h = 1)

  # Ensure no negative values
  arfima_forecast$mean[arfima_forecast$mean < 0] <- 0
  arfima_forecast$lower[arfima_forecast$lower < 0] <- 0
  arfima_forecast$upper[arfima_forecast$upper < 0] <- 0
  
  # Plot the forecast
  plot <- autoplot(arfima_forecast) +
    labs(title = paste("ARFIMA Forecast of Medals for", country_code),
         x = "Year",
         y = "Medal Count")
  
  return(list(arfima_model, plot, round(arfima_forecast$mean)))
}

# Initialize lists to store results
forecast_2024_arfima <- list()
model_2024_arfima <- list()

# Loop through each country and generate forecasts
for (country in countries_to_forecast) {
  result <- forecast_arfima_2024(country)
  if (!is.null(result)) {
    model_2024_arfima[[country]] <- result[[1]]
    forecast_2024_arfima[[country]] <- result[[3]]
    print(result[[2]])
  }
}

print(data.frame(forecast_2024_arfima))


###################
# Function to fit Linear Regression model and forecast 2016 & 2020 for each country (TEST)

forecast_country_medals_lm <- function(country_code) {
  # Subset data for the country
  country_data_train <- medal_counts_complete_train %>% filter(Team == country_code)
  country_data_test <- medal_counts_complete_test %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data_train) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Fit Linear Regression model
  lm_model <- lm(medal_count ~ Year + GeopoliticalEvents, data = country_data_train)
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(lm_model))
  
  # Forecast future medals
  forecasted_values <- predict(lm_model, newdata = country_data_test)
  
  # Ensure no negative values
  forecasted_values[forecasted_values < 0] <- 0
  
  print(round(forecasted_values))
  print(country_data_test$medal_count)
  
  # Create a data frame for plotting
  forecast_df <- data.frame(
    Year = country_data_test$Year,
    Forecast = forecasted_values,
    Actual = country_data_test$medal_count
  )
  
  # Plot the forecast
  plot <- ggplot(forecast_df, aes(x = Year)) +
    geom_line(aes(y = Forecast, color = "Forecast")) +
    geom_line(aes(y = Actual, color = "Actual")) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count") +
    scale_color_manual(values = c("Forecast" = "blue", "Actual" = "red"))
  
  
  # Calculate accuracy separately for 2016 and 2020
  accuracy_2016 <- calculate_accuracy(
    actual = country_data_test %>% filter(Year == 2016) %>% pull(medal_count),
    predicted = round(forecasted_values[which(country_data_test$Year == 2016)])
  )
  
  accuracy_2020 <- calculate_accuracy(
    actual = country_data_test %>% filter(Year == 2020) %>% pull(medal_count),
    predicted = round(forecasted_values[which(country_data_test$Year == 2020)])
  )
  
  return(list(lm_model, plot, round(forecasted_values), country_data_test$medal_count, list(accuracy_2016, accuracy_2020)))
}

forecast_test_lm <- list()
actual_medal_lm <- list()
model_lm <- list()
accuracy_lm <- list()

for (country in countries_to_forecast) {
  result <- forecast_country_medals_lm(country)
  if (!is.null(result)) {
    model_lm[[country]] <- result[[1]]
    forecast_test_lm[[country]] <- result[[3]]
    actual_medal_lm[[country]] <- result[[4]]
    accuracy_lm[[country]] <- result[[5]]
    print(result[[2]])
  }
}

# Print forecast, actual values, and accuracies for 2016 and 2020
for (country in countries_to_forecast) {
  cat("\nCountry:", country)
  cat("\nForecasted Medals for 2016 & 2020:", forecast_test_lm[[country]])
  cat("\nActual Medals for 2016 & 2020:", actual_medal_lm[[country]])
  cat("\nAccuracy for 2016 - MAE:", accuracy_lm[[country]][[1]]$MAE, "MSE:", accuracy_lm[[country]][[1]]$MSE, "RMSE:", accuracy_lm[[country]][[1]]$RMSE)
  cat("\nAccuracy for 2020 - MAE:", accuracy_lm[[country]][[2]]$MAE, "MSE:", accuracy_lm[[country]][[2]]$MSE, "RMSE:", accuracy_lm[[country]][[2]]$RMSE)
}

###################################
# Function to fit Linear Regression model and forecast 2024 for each country
forecast_lm_2024 <- function(country_code) {
  country_data <- medal_counts_complete %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Fit Linear Regression model
  lm_model <- lm(medal_count ~ Year + GeopoliticalEvents, data = country_data)
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(lm_model))
  
  # Create future data for 2024
  future_data <- data.frame(
    Year = c(2024),
    GeopoliticalEvents = c(1)
  )
  
  # Forecast future medals
  forecasted_values <- predict(lm_model, newdata = future_data)
  
  # Ensure no negative values
  forecasted_values[forecasted_values < 0] <- 0
  
  # Plot the forecast
  plot <- ggplot(future_data, aes(x = Year)) +
    geom_point(aes(y = forecasted_values, color = "Forecast"), size = 4) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count") +
    scale_color_manual(values = c("Forecast" = "blue"))
  
  return(list(lm_model, plot, round(forecasted_values)))
}

forecast_2024_lm <- list()
model_2024_lm <- list()

# Loop through each country and generate forecasts
for (country in countries_to_forecast) {
  result <- forecast_lm_2024(country)
  if (!is.null(result)) {
    model_2024_lm[[country]] <- result[[1]]
    forecast_2024_lm[[country]] <- result[[3]]
    print(result[[2]])
  }
}

print(data.frame(forecast_2024_lm))


###################################

# Function to fit ARIMA model and forecast 2016 & 2020 for each country (TEST)
forecast_country_medals_arima_n <- function(country_code) {
  # Subset data for the country
  country_data_train <- medal_counts_complete_train %>% filter(Team == country_code)
  country_data_test <- medal_counts_complete_test %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data_train) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Convert to time series
  ts_data <- ts(country_data_train$medal_count, start = min(country_data_train$Year), frequency = 0.25) #changing to 0.25 due to data being every 4 years (1/4)
  
  
  # Fit ARIMA model with exogenous variables
  model <- auto.arima(ts_data)
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(model))
  
  # Forecast future medals
  forecasted_values <- forecast(model, h=2)
  
  # Ensure no negative values
  forecasted_values$mean[forecasted_values$mean < 0] <- 0
  forecasted_values$lower[forecasted_values$lower < 0] <- 0
  forecasted_values$upper[forecasted_values$upper < 0] <- 0
  
  print(round(forecasted_values$mean))
  print(country_data_test$medal_count)
  
  # Plot the forecast
  plot <- autoplot(forecasted_values) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count")
  
  # Calculate accuracy separately for 2016 and 2020
  accuracy_2016 <- calculate_accuracy(
    actual = country_data_test %>% filter(Year == 2016) %>% pull(medal_count),
    predicted = forecasted_values$mean[1]  # 1st value in forecast
  )
  
  accuracy_2020 <- calculate_accuracy(
    actual = country_data_test %>% filter(Year == 2020) %>% pull(medal_count),
    predicted = forecasted_values$mean[2]  # 2nd value in forecast
  )
  
  return(list(model, plot, round(forecasted_values$mean), country_data_test$medal_count, list(accuracy_2016, accuracy_2020)))
}

forecast_test_arima_n <- list()
actual_medal_arima_n <- list()
model_arima_n <- list()
accuracy_arima_n <- list()

for (country in countries_to_forecast) {
  result <- forecast_country_medals_arima_n(country)
  if (!is.null(result)) {
    model_arima_n[[country]] <- result[[1]]
    forecast_test_arima_n[[country]] <- result[[3]]
    actual_medal_arima_n[[country]] <- result[[4]]
    accuracy_arima_n[[country]] <- result[[5]]
    print(result[[2]])
  }
}

# Print forecast, actual values, and accuracies for 2016 and 2020
for (country in countries_to_forecast) {
  cat("\nCountry:", country)
  cat("\nForecasted Medals for 2016 & 2020:", forecast_test_arima_n[[country]])
  cat("\nActual Medals for 2016 & 2020:", actual_medal_arima_n[[country]])
  cat("\nAccuracy for 2016 - MAE:", accuracy_arima_n[[country]][[1]]$MAE, "MSE:", accuracy_arima_n[[country]][[1]]$MSE, "RMSE:", accuracy_arima_n[[country]][[1]]$RMSE)
  cat("\nAccuracy for 2020 - MAE:", accuracy_arima_n[[country]][[2]]$MAE, "MSE:", accuracy_arima_n[[country]][[2]]$MSE, "RMSE:", accuracy_arima_n[[country]][[2]]$RMSE)
}
  
  


####################

################
# Function to fit ARIMA with no regressor model and forecast 2024 for each country
forecast_arima_2024_n <- function(country_code) {
  country_data <- medal_counts_complete %>% filter(Team == country_code)
  
  # Check if there are any observations
  if(nrow(country_data) == 0) {
    print(paste("No data available for country:", country_code))
    return(NULL)
  }
  
  # Convert to time series
  ts_data <- ts(country_data$medal_count, start = min(country_data$Year), frequency = 0.25) #changing to 0.25 due to data being every 4 years (1/4)
  
  # Convert exogenous variables to a numeric matrix
  xreg <- as.matrix(country_data$GeopoliticalEvents)
  
  # Fit ARIMA model with exogenous variables
  model <- auto.arima(ts_data)
  
  # Summary of the model
  print(paste("Model summary for country:", country_code))
  print(summary(model))
  
  
  # Forecast future medals
  forecasted_values <- forecast(model, h= 1)
  
  # Ensure no negative values
  forecasted_values$mean[forecasted_values$mean < 0] <- 0
  forecasted_values$lower[forecasted_values$lower < 0] <- 0
  forecasted_values$upper[forecasted_values$upper < 0] <- 0
  
  # Plot the forecast
  plot <- autoplot(forecasted_values) +
    labs(title = paste("Forecast of Medals for", country_code, "with Geopolitical Events"),
         x = "Year",
         y = "Medal Count")
  
  return(list(model, plot, round(forecasted_values$mean)))
}

forecast_2024_arima_n <- list()
model_2024_arima_n <- list()

# Loop through each country and generate forecasts
for (country in countries_to_forecast) {
  result <- forecast_arima_2024_n(country)
  if (!is.null(result)) {
    model_2024_arima_n[[country]] <- result[[1]]
    forecast_2024_arima_n[[country]] <- result[[3]]
    print(result[[2]])
  }
}

print(data.frame(forecast_2024_arima_n))


#######
library(tseries)
for (country_code in countries_to_forecast) {
  country_data <- medal_counts_complete %>% filter(Team == country_code)
  ts_data <- ts(country_data$medal_count, start = min(country_data$Year), frequency = 0.25)
  plot(ts_data, main = country_code)
  
  kpss_result <- kpss.test(ts_data, null = "Level")  # You can also use "Trend" for null hypothesis of trend stationarity
  
  print(paste("KPSS test for", country_code, "p-value:", kpss_result$p.value))
  
  # Check stationarity based on KPSS test
  if (kpss_result$p.value < 0.05) {
    print(paste("The series for", country_code, "is non-stationary. Differencing required."))
    ts_data_diff <- diff(ts_data)
    arima_model <- auto.arima(ts_data_diff)
  } else {
    print(paste("The series for", country_code, "is stationary. No differencing required."))
    arima_model <- auto.arima(ts_data)
  }
  
  acf(ts_data)
}

######


for (country_code in countries_to_forecast) {
  country_data <- medal_counts_complete %>% filter(Team == country_code)
  ts_data <- ts(country_data$medal_count, start = min(country_data$Year), frequency = 0.25)
  plot(ts_data, main = country_code)
  
  kpss_result <- kpss.test(ts_data, null = "Level")  # You can also use "Trend" for null hypothesis of trend stationarity
  
  print(paste("KPSS test for", country_code, "p-value:", kpss_result$p.value))
  
  # Check stationarity based on KPSS test
  if (kpss_result$p.value < 0.05) {
    print(paste("The series for", country_code, "is non-stationary. Differencing required."))
    ts_data_diff <- diff(ts_data)
    arima_model <- auto.arima(ts_data_diff)
  } else {
    print(paste("The series for", country_code, "is stationary. No differencing required."))
    arima_model <- auto.arima(ts_data)
  }
  
  acf(ts_data)
}
