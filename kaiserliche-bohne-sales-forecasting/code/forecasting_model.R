# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(ggplot2)
library(tidyr)
library(widyr)
library(forecast)
library(tseries)

#file path
file_path <- "~/Desktop/CS_96/3yr_coffee_data.csv"

#load dataset
coffee_data <- read_csv(file_path, locale = locale(encoding = "ISO-8859-1"))

#preview
glimpse(coffee_data)

#convert Posting.date and Posting.time to usable formats
library(lubridate)

coffee_data <- coffee_data %>%
  mutate(
    Posting.date = dmy(Posting.date),      #convert to d/m/y format
    Posting.time = hms(Posting.time)       #convert to h/m/s
  )

#rows where posting.date is NA (failed to parse)
coffee_data %>%
  filter(is.na(Posting.date)) %>%
  select(receipt, Posting.date, Invoice.number, Product, Quantity) %>%
  head(10)

#remove rows where posting.date couldn't be parsed 
coffee_data <- coffee_data %>%
  filter(!is.na(Posting.date))

#convert to lowercase
coffee_data <- coffee_data %>%
  mutate(
    Product = str_trim(str_to_lower(Product)),
    Product.group = str_trim(str_to_lower(Product.group))
  )

#change misspellings
coffee_data <- coffee_data %>%
  mutate(Product = case_when(
    Product %in% c("expresso", "espreso") ~ "espresso",
    Product %in% c("tee", "tea") ~ "tea",  
    Product %in% c("flatwhite") ~ "flat white",
    Product %in% c("pain chocolat") ~ "pain au chocolat",
    Product %in% c("double espresso", "espresso double") ~ "espresso double",
    TRUE ~ Product  #leave everything else unchanged
  ))

#check how many rows are missing values in key fields
colSums(is.na(coffee_data[, c("Product", "Quantity", "Gross.price", "Net.price", "Invoice.number")]))

#quantity <= 0
coffee_data %>% filter(Quantity <= 0)

#Negative prices
coffee_data %>% filter(Gross.price < 0 | Net.price < 0)

#zero price with no quantity
coffee_data %>% filter(Gross.price == 0 & Quantity > 0)

#create new column for year month
coffee_data <- coffee_data %>%
  mutate(month = floor_date(Posting.date, unit = "month"))

#group by month and calculate total gross revenue
monthly_sales <- coffee_data %>%
  group_by(month) %>%
  summarise(total_gross_sales = sum(Gross.price, na.rm = TRUE))

#line chart of monthly sales
ggplot(monthly_sales, aes(x = month, y = total_gross_sales)) +
  geom_line(color = "green", size = 1.2) +
  labs(title = "Total Gross Sales Over Time",
       x = "Month",
       y = "Gross Sales (€)") +
  theme_minimal()

#create transaction count by month
monthly_transactions <- coffee_data %>%
  group_by(month) %>%
  summarise(transaction_count = n_distinct(Invoice.number))

#plot
ggplot(monthly_transactions, aes(x = month, y = transaction_count)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(title = "Number of Transactions Per Month",
       x = "Month",
       y = "Transaction Count") +
  theme_minimal()

#create summary table 10 products by quantity sold
top_products_qty <- coffee_data %>%
  group_by(Product) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(total_quantity)) %>%
  slice_head(n = 10)

#plot
ggplot(top_products_qty, aes(x = reorder(Product, total_quantity), y = total_quantity)) +
  geom_bar(stat = "identity", fill = "red") +
  coord_flip() +
  labs(title = "Top 10 Products by Quantity Sold",
       x = "Product",
       y = "Total Quantity") +
  theme_minimal()

#-------------------------------------------------------------------------------
#top 10 products by revenue
#-------------------------------------------------------------------------------

#create summary table top 10 products by gross revenue
top_products_revenue <- coffee_data %>%
  group_by(Product) %>%
  summarise(total_revenue = sum(Gross.price, na.rm = TRUE)) %>%
  arrange(desc(total_revenue)) %>%
  slice_head(n = 10)

#plot
ggplot(top_products_revenue, aes(x = reorder(Product, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Top 10 Products by Gross Revenue",
       x = "Product",
       y = "Total Gross Sales (€)") +
  theme_minimal()

#look at all variants of poppy seed brioche (from ed discussion)
coffee_data %>%
  filter(str_detect(Product, "brioche")) %>%
  count(Product, sort = TRUE)

#sample poppy seed brioche rows
coffee_data %>%
  filter(Product == "poppy seed brioche") %>%
  select(Posting.date, Invoice.number, Quantity, Gross.price, Product.group) %>%
  head(20)

#-------------------------------------------------------------------------------
#totasl revenue by product group drinks and food
#-------------------------------------------------------------------------------
#summarize total gross revenue by product group
product_group_sales <- coffee_data %>%
  group_by(Product.group) %>%
  summarise(total_revenue = sum(Gross.price, na.rm = TRUE)) %>%
  arrange(desc(total_revenue))

#plot
ggplot(product_group_sales, aes(x = reorder(Product.group, total_revenue), y = total_revenue)) +
  geom_bar(stat = "identity", fill = "green") +
  coord_flip() +
  labs(title = "Total Gross Sales by Product Group",
       x = "Product Group",
       y = "Total Gross Sales (€)") +
  theme_minimal()


#-------------------------------------------------------------------------------
#gorss sales by day of week
#-------------------------------------------------------------------------------

#add weekday column Mon Tues...
coffee_data <- coffee_data %>%
  mutate(weekday = wday(Posting.date, label = TRUE, abbr = TRUE, week_start = 1))  # week_start = 1 means Mon

#summarize gross revenue by weekday
weekday_sales <- coffee_data %>%
  group_by(weekday) %>%
  summarise(total_gross_sales = sum(Gross.price, na.rm = TRUE)) %>%
  arrange(match(weekday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))  #keep in order

#plot
ggplot(weekday_sales, aes(x = weekday, y = total_gross_sales)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Gross Sales by Day of the Week",
       x = "Weekday",
       y = "Total Gross Sales (€)") +
  theme_minimal()


#-------------------------------------------------------------------------------
#gross sales by hour
#-------------------------------------------------------------------------------

#extract hour of the day from Posting.time
coffee_data <- coffee_data %>%
  mutate(hour = hour(Posting.time))  

#summarize gross sales by hour
hourly_sales <- coffee_data %>%
  group_by(hour) %>%
  summarise(total_gross_sales = sum(Gross.price, na.rm = TRUE))

#plot
ggplot(hourly_sales, aes(x = hour, y = total_gross_sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Gross Sales by Hour of the Day",
       x = "Hour (24-hour format)",
       y = "Total Gross Sales (€)") +
  theme_minimal()

# bundle items
#get only necessary columns
pair_data <- coffee_data %>%
  select(Invoice.number, Product) %>%
  distinct()  # Remove rows with same product listed twice

#create co-occurrence matrix
product_pairs <- pair_data %>%
  group_by(Invoice.number) %>%
  summarise(product_list = list(Product)) %>%
  unnest_longer(product_list) %>%
  pairwise_count(product_list, Invoice.number, sort = TRUE, upper = FALSE)

#view
head(product_pairs, 10)

#by season and hourly sale
coffee_data <- coffee_data %>%
  mutate(
    hour = hour(Posting.time),
    month = month(Posting.date, label = TRUE, abbr = TRUE),  # Jan, Feb
    year = year(Posting.date)
  )
#summarize gross sales by hour and month
seasonal_hourly_sales <- coffee_data %>%
  group_by(month, hour) %>%
  summarise(
    avg_gross_sales = sum(Gross.price, na.rm = TRUE) / n_distinct(Posting.date)
  ) %>%
  ungroup()
#line plot
ggplot(seasonal_hourly_sales, aes(x = hour, y = avg_gross_sales)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ month, ncol = 3) +
  labs(
    title = "Average Hourly Sales by Month",
    x = "Hour (24h)",
    y = "Avg Gross Sales per Day (€)"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------
#Cappuccino Forecasting
#-------------------------------------------------------------------------------
#load data and parse
coffee_data <- read.csv("~/Desktop/CS_96/3yr_coffee_data.csv", 
                        encoding = "ISO-8859-1", 
                        colClasses = c("Posting.date" = "character"))
coffee_data$Posting.date <- dmy(coffee_data$Posting.date)

#fix encoding for special characters in product names
coffee_data$Product <- iconv(coffee_data$Product, from = "", to = "UTF-8", sub = "byte")

#filter for cappuccino and aggregate monthly gross sales
cappuccino_data <- coffee_data %>%
  filter(tolower(trimws(Product)) == "cappuccino")

monthly_cappuccino <- cappuccino_data %>%
  mutate(month = floor_date(Posting.date, unit = "month")) %>%
  group_by(month) %>%
  summarise(gross_sales = sum(Gross.price, na.rm = TRUE))

#preview
head(monthly_cappuccino)

#create time series object
ts_cappuccino <- ts(monthly_cappuccino$gross_sales, start = c(2022, 1), frequency = 12)

#visualize raw time series
autoplot(ts_cappuccino) +
  labs(title = "Monthly Gross Sales of Cappuccino", y = "Gross Sales (€)", x = "Month")

#decompose the time series
decomp <- decompose(ts_cappuccino, type = "multiplicative")
autoplot(decomp) +
  labs(title = "Cappuccino Sales Decomposition")
time(ts_cappuccino)

#train/test split (train until 2023 middle test July to Dec)
train_ts <- window(ts_cappuccino, end = c(2023, 6))
test_ts <- window(ts_cappuccino, start = c(2023, 7))

#Fit Holt-Winters model (multiplicative seasonality) needed at least 2 seasons but lacking that so switch to ets
#hw_model <- HoltWinters(train_ts, seasonal = "multiplicative")
#hw_forecast <- forecast(hw_model, h = length(test_ts))

#re train ets with bpx cox transformation to improve accuracy and it did!
ets_model_bc <- ets(train_ts, lambda = "auto")

#forecast on the test set
ets_forecast_bc <- forecast(ets_model_bc, h = length(test_ts))

#plot forecast vs actual
autoplot(ets_forecast_bc) +
  autolayer(test_ts, series = "Actual", linetype = "dashed") +
  labs(title = "ETS Forecast with Box-Cox vs Actual",
       y = "Gross Sales (€)", x = "Month")

#model performance
accuracy(ets_forecast_bc, test_ts)

#train holt-winters model on all 24 months of data
hw_final_model <- HoltWinters(ts_cappuccino, seasonal = "multiplicative")

#forecast next 6 months (Jan–Jun 2024)
hw_final_forecast <- forecast(hw_final_model, h = 6)

#plot the final forecast
autoplot(hw_final_forecast) +
  labs(title = "Final 6-Month Holt-Winters Forecast for Cappuccino Sales",
       y = "Gross Sales (€)", x = "Month")
