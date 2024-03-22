# Setting seed for reproducibility
set.seed(123)

# Generating the dataset
n_rows <- 1000

# Generate Dates
dates <- sample(seq(as.Date("2019-01-01"), as.Date("2019-12-31"), by="day"), n_rows, replace = TRUE)

# Generate Patient_ID
patient_ids <- sample(100000:999999, n_rows, replace = FALSE)

# Generate Age (assuming age is between 0 and 100, adjust as needed)
ages <- sample(0:100, n_rows, replace = TRUE)

# Generate Sex
sexes <- sample(c("Male", "Female"), n_rows, replace = TRUE)

# Generate Covid_Positivity
covid_positivity <- sample(c("Positive", "Negative"), n_rows, replace = TRUE)

# Creating the dataframe
df <- data.frame(Date = dates, Patient_ID = patient_ids, Age = ages, Sex = sexes, Covid_Positivity = covid_positivity)

# Printing first few rows of the dataset
head(df)

# Generate Covid_Positivity as boolean (1 for Positive, 0 for Negative)
covid_positivity <- sample(c(1, 0), n_rows, replace = TRUE)

# Creating the dataframe
df <- data.frame(Date = dates, Patient_ID = patient_ids, Age = ages, Sex = sexes, Covid_Positivity = covid_positivity)

# Printing first few rows of the dataset
head(df)

# Write the dataframe to a CSV file
write.csv(df, "dummy_data.csv", row.names = FALSE)

# Load required libraries
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

# Load the dataset (assuming it's in a CSV format)
dataset <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-17-2020.csv")

# Print the first few rows of the dataset
cat("Head of the dataset:\n")
head(dataset)

# Check and print the number of missing values in each column
cat("\nMissing values in each column:\n")
missing_values <- sapply(dataset, function(x) sum(is.na(x)))
print(missing_values)

# Filter rows where both Latitude and Longitude are NA
missing_lat_lon <- subset(dataset, is.na(Latitude) & is.na(Longitude))

# Print the rows with missing Latitude and Longitude values
print(missing_lat_lon)

library(dplyr)
library(tidyr)

# Step 1: Grab the first not-null 'Latitude' and 'Longitude' for each 'Country/Region' from the dataset
lat_lon_first_not_null <- dataset %>%
  group_by(Country.Region) %>%
  arrange(Last.Update) %>%
  summarise(
    Latitude = first(Latitude[!is.na(Latitude)]),
    Longitude = first(Longitude[!is.na(Longitude)])
  )

# Step 2: Fill the null values of 'Latitude' and 'Longitude' based on the same 'Country/Region' values stored in Step 1
dataset_filled <- dataset %>%
  left_join(lat_lon_first_not_null, by = "Country.Region", suffix = c("", "_fill")) %>%
  mutate(
    Latitude = ifelse(is.na(Latitude), Latitude_fill, Latitude),
    Longitude = ifelse(is.na(Longitude), Longitude_fill, Longitude)
  ) %>%
  select(-Latitude_fill, -Longitude_fill)

# Print the dataset with filled values
head(dataset_filled)

# Convert Last.Update to Date-Time if it's not already
dataset$Last.Update <- as.POSIXct(dataset$Last.Update, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")

# If the Last.Update column is in String format, sort by Country.Region and then by Last.Update
dataset <- dataset %>% arrange(Country.Region, desc(Last.Update))

# Group by Country/Region and get the latest data for each group
latest_data <- dataset %>%
  group_by(Country.Region) %>%
  slice_head(n = 1) %>%
  summarise(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Active = sum(Confirmed - Deaths - Recovered, na.rm = TRUE),
    Last.Update = first(Last.Update)
  )

# View the resulting dataframe
print(latest_data)

# Compute active cases and summarize the data
summary_data <- dataset %>%
  mutate(Active = Confirmed - Deaths - Recovered) %>%
  group_by(Country.Region) %>%
  summarise(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Active = sum(Active, na.rm = TRUE)
  )

# View the resulting dataframe
print(summary_data)

# Filtering for rows where 'Country/Region' is 'US'
us_stats <- summary_data %>%
  filter(Country.Region == "US")

# View the resulting dataframe
print(us_stats)

# Order the data by 'Confirmed' cases
summary_data <- summary_data %>% arrange(desc(Confirmed))

# Create a bar plot
ggplot(summary_data, aes(x = reorder(Country.Region, -Confirmed), y = Confirmed, fill = Country.Region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(
    title = "Country/Region wise Confirmed Cases",
    x = "Country/Region",
    y = "Confirmed Cases"
  )

ggplot(summary_data, aes(x = reorder(Country.Region, -Confirmed), y = Confirmed, fill = Country.Region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Adjust size and angle as needed
    axis.text.y = element_text(size = 8)
  ) +
  labs(
    x = "Country/Region",
    y = "Confirmed Cases",
    title = "Country/Region wise Confirmed Cases"
  )

# Order the data by 'Confirmed' cases in ascending order for horizontal bars
summary_data <- summary_data %>% arrange(Confirmed)

# Set the plot size
options(repr.plot.width=12, repr.plot.height=25) # You can adjust these as needed

# Plotting with ggplot2
ggplot(summary_data, aes(x = reorder(Country.Region, Confirmed), y = Confirmed)) +
  geom_bar(stat = "identity", fill = "skyblue", show.legend = FALSE) +
  coord_flip() +  # To make the bars horizontal
  theme_minimal() +
  geom_text(aes(label = scales::comma(Confirmed), y = Confirmed), hjust = -0.1, size = 3.5) +  # To add data labels on top of the bars
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    axis.ticks.y = element_blank()  # To remove the ticks on the y-axis
  ) +
  labs(
    title = "Country/Region wise Confirmed Cases",
    y = "Confirmed Cases",
    x = "Country/Region"
  ) +
  scale_y_continuous(labels = scales::comma)  # To format y axis labels with commas

library(forecast)
library(ggplot2)
library(dplyr)
library(lubridate)

# Step 1: Read the CSV file
df <- read.csv('./Hands-on/time_series_data.csv')

# Step 2: Convert 'date_column' to Date type and set it as the index
df$date_column <- dmy(df$date_column)
df <- df %>% arrange(date_column)

# Step 3: Train an ARIMA model
model <- auto.arima(df$value_column, stepwise=FALSE, approximation=FALSE)

# Step 4: Get the last date in the 'date_column' and generate dates for the next 7 days
last_date <- max(df$date_column)
next_7_dates <- seq(from = last_date + days(1), by = "day", length.out = 7)

# Step 5: Forecast the 'value_column' for the next 7 days
forecast_results <- forecast(model, h=7)
forecast <- forecast_results$mean
conf_int <- forecast_results$lower %>% cbind(forecast_results$upper)

# Step 6: Create a new DataFrame with the predictions for the next 7 days
predictions <- data.frame(
  date_column = next_7_dates,
  value_column = forecast
)

# Step 7: Print the predictions
print(predictions)

# Optional: Plot the historical data and the forecast
ggplot() +
  geom_line(data = df, aes(x = date_column, y = value_column), color = 'blue', size = 1) +
  geom_line(data = predictions, aes(x = date_column, y = value_column), color = 'red', size = 1) +
  geom_ribbon(aes(x = predictions$date_column, ymin = conf_int[,1], ymax = conf_int[,2]), fill = 'pink', alpha = 0.3) +
  labs(x = 'Date', y = 'Value') +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
model <- Arima(df$value_column, order=c(5,1,0))
