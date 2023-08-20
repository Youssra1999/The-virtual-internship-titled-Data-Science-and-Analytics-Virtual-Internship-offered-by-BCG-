install.packages("patchwork")

# Load required libraries
library(tidyverse)

# Load data
client_data <- read.csv("C:/Users/USER/Downloads/client_data.csv")
price_data <- read.csv("C:/Users/USER/Downloads/price_data.csv")

# Display basic information about the datasets
str(client_data)
str(price_data)

# Summary statistics for numeric columns
summary(client_data)
summary(price_data)

# Explore missing values
missing_client <- client_data %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, missing_count)

missing_price <- price_data %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(variable, missing_count)

# Plot missing value summary
ggplot(missing_client, aes(x = variable, y = missing_count)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Missing Values in Client Data", x = "Variable", y = "Missing Count") +
  theme_minimal()

ggplot(missing_price, aes(x = variable, y = missing_count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Missing Values in Price Data", x = "Variable", y = "Missing Count") +
  theme_minimal()

# Explore distribution of churn
ggplot(client_data, aes(x = churn, fill = churn)) +
  geom_bar() +
  labs(title = "Distribution of Churn", x = "Churn", y = "Count") +
  scale_fill_manual(values = c("Yes" = "red", "No" = "blue")) +
  theme_minimal()

# Explore relationships between numeric variables
numeric_vars <- c("cons_12m", "cons_gas_12m", "cons_last_month", "forecast_cons_12m", "forecast_cons_year", "forecast_meter_rent_12m", "imp_cons", "margin_gross_pow_ele", "margin_net_pow_ele", "nb_prod_act", "net_margin", "num_years_antig", "pow_max")

correlation_matrix <- client_data[numeric_vars] %>%
  cor()

corrplot::corrplot(correlation_matrix, method = "color")

# Explore price trends over time
price_data_long <- price_data %>%
  gather(price_type, price, -id, -price_date)

ggplot(price_data_long, aes(x = price_date, y = price, color = price_type)) +
  geom_line() +
  labs(title = "Price Trends Over Time", x = "Date", y = "Price") +
  theme_minimal() +
  theme(legend.position = "top")

# Convert price_date to a Date object
price_data$price_date <- as.Date(price_data$price_date)

# Melt the data for plotting
melted_price_data <- reshape2::melt(price_data, id.vars = c("price_date", "id"), variable.name = "price_type", value.name = "price_value")

# Create the plot
ggplot(melted_price_data, aes(x = price_date, y = price_value, color = price_type)) +
  geom_line() +
  labs(title = "Price Changes Over Time", x = "Date", y = "Price") +
  theme_minimal() +
  theme(legend.position = "top")

# Load required libraries
library(ggplot2)
library(dplyr)


# Calculate churn rate by gas consumption category
gas_churn_rate <- client_data %>%
  mutate(gas_consumption_category = cut(cons_gas_12m, breaks = c(0, 5000, 10000, 15000, Inf), labels = c("0-5000", "5001-10000", "10001-15000", "15001+"), include.lowest = TRUE)) %>%
  group_by(gas_consumption_category) %>%
  summarise(churn_rate = mean(churn == "Yes", na.rm = TRUE))

# Create the bar plot
ggplot(gas_churn_rate, aes(x = gas_consumption_category, y = churn_rate)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Churn Rate by Gas Consumption Category",
       x = "Gas Consumption Category",
       y = "Churn Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load required libraries
library(dplyr)

# Assuming client_data and price_data are loaded into data frames

# Merge the data frames based on the 'id' column
merged_data <- inner_join(client_data, price_data, by = "id")

# Explore the merged dataset
head(merged_data)

# Load required libraries
library(ggplot2)


# Create the box plot for price variations by churn status
ggplot(merged_data, aes(x = churn, y = price_mid_peak_var, fill = churn)) +
  geom_boxplot() +
  labs(x = "Churn", y = "Price of Energy (Mid Peak)", title = "Price Variation vs. Churn") +
  theme_minimal()

# Load required libraries
library(ggplot2)

# Assuming merged_data is loaded into a data frame

# Create the box plot for price variations by churn status
ggplot(merged_data, aes(x = churn, y = price_off_peak_var, fill = churn)) +
  geom_boxplot() +
  labs(x = "Churn", y = "Price of Energy (Off Peak)", title = "Price Variation vs. Churn") +
  theme_minimal()


# Assuming merged_data is loaded into a data frame

# Create the box plot for price variations by churn status
ggplot(merged_data, aes(x = churn, y = price_peak_var, fill = churn)) +
  geom_boxplot() +
  labs(x = "Churn", y = "Price of Energy (Peak)", title = "Price Variation vs. Churn") +
  theme_minimal()

# Load required libraries
library(ggplot2)

# Load required libraries
library(ggplot2)

# Assuming client_df is loaded into a data frame

# Convert date columns to Date format
client_data$date_activ <- as.Date(client_data$date_activ)
# ... similar conversions for other date columns ...

# Plot time series of energy prices (off peak and peak)
plt1 <- ggplot(client_data, aes(x = date_activ, y = forecast_price_energy_off_peak, color = "Energy Price (Off Peak)")) +
  geom_line() +
  labs(x = "Date", y = "Energy Price", title = "Time Series Analysis: Energy Price (Off Peak)") +
  theme_minimal() +
  theme(legend.position = "top")

plt2 <- ggplot(client_data, aes(x = date_activ, y = forecast_price_energy_peak, color = "Energy Price (Peak)")) +
  geom_line() +
  labs(x = "Date", y = "Energy Price", title = "Time Series Analysis: Energy Price (Peak)") +
  theme_minimal() +
  theme(legend.position = "top")

# Combine the two plots using patchwork package
library(patchwork)
combined_plot <- plt1 + plt2

# Plot time series of churn rates
churn_plot <- ggplot(client_data, aes(x = date_activ, y = churn, color = "Churn Rate")) +
  geom_line() +
  labs(x = "Date", y = "Churn Rate", title = "Time Series Analysis: Churn Rates") +
  theme_minimal() +
  theme(legend.position = "top")

# Display the plots
combined_plot / churn_plot





