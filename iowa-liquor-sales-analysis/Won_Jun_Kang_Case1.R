#install packages
install.packages(c("data.table", "ggplot2", "dplyr", "lubridate"))

# Load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

# main directory containing all year folders
data_dir <- "/Users/juny910/Desktop/CS_96/CS-96/Cases/Spring/I Retail_Transactions_EDA/data"

#load library
library(data.table)

#list csv files from data folder
file_list <- list.files(data_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

#merge csv into dataset
liquor_data <- rbindlist(lapply(file_list, fread))

#check if loaded
head(liquor_data)

#column names contain spaces so fix(ex store name -> store.name)
colnames(liquor_data) <- make.names(colnames(liquor_data), unique = TRUE)

#check for missing values
colSums(is.na(liquor_data))

#zip code column is missing values 
# will analyze based on store name and address remove row of zip codes first
liquor_data <- liquor_data[!is.na(Zip.Code), ]
colnames(liquor_data)

#sale (dollars) has two .. so change name
setnames(liquor_data, old = "Sale..Dollars.", new = "Sale.Dollars")


#Main goals
#1 overall sales trends over time 
library(ggplot2)
library(dplyr)

# summarize total sales per month 
#will be using pipe operator for clarity
sales_trend <- liquor_data %>%
mutate(Year = as.integer(format(Date, "%Y"))) %>%
group_by(Year, Month) %>%
summarize(Total_Sales = sum(Sale.Dollars, na.rm = TRUE), .groups = "drop")

# plot sales trend
ggplot(sales_trend, aes(x = Month, y = Total_Sales, group = Year, color = as.factor(Year))) +
geom_line(size=1.2) +
labs(title=" Monthly Sales Trend (2019-2023)", x="Month", y="Total Sales ($)") +
theme_minimal()

#2 top performing stores
# summarize total sales per store
top_stores <- liquor_data %>%
group_by(Store.Name) %>%
summarize(Total_Sales = sum(Sale.Dollars, na.rm = TRUE)) %>%
arrange(desc(Total_Sales)) %>%
head(10)

# plot top stores
ggplot(top_stores, aes(x=reorder(Store.Name, Total_Sales), y=Total_Sales)) +
geom_bar(stat="identity", fill="blue") +
coord_flip() +
labs(title=" Top 10 Performing Stores", x="Store Name", y="Total Sales ($)") +
theme_minimal()

#3 most popular Liquor categories
# summarize total sales per liquor category
top_categories <- liquor_data %>%
group_by(Category.Name) %>%
summarize(Total_Sales = sum(Sale.Dollars, na.rm = TRUE)) %>%
arrange(desc(Total_Sales)) %>%
head(10)

# plot top liquor categories
ggplot(top_categories, aes(x=reorder(Category.Name, Total_Sales), y=Total_Sales)) +
geom_bar(stat="identity", fill="red") +
coord_flip() +
labs(title=" Top 10 Selling Liquor Categories", x="Category", y="Total Sales ($)") +
theme_minimal()

#4 price distribution
#check for outliers
summary(liquor_data$State.Bottle.Retail)
#removed outliers 20k and above and manually fit
liquor_data_filtered <- liquor_data %>%filter(State.Bottle.Retail < 500)
ggplot(liquor_data_filtered, aes(x=State.Bottle.Retail)) +
geom_histogram(binwidth=5, fill="green", color="white") +
labs(title=" Distribution of Bottle Prices", x="Price ($)", y="Count") +
xlim(0, 100) +  # adjusting to show common prices
theme_minimal()


#5 geographical Analysis
# summarize total sales per city
city_sales <- liquor_data %>%
group_by(City) %>%
summarize(Total_Sales = sum(Sale.Dollars, na.rm = TRUE)) %>%
arrange(desc(Total_Sales)) %>%
head(10)

# plot sales by city
ggplot(city_sales, aes(x=reorder(City, Total_Sales), y=Total_Sales)) +
geom_bar(stat="identity", fill="purple") + coord_flip() +
labs(title="Top 10 Cities by Liquor Sales", x="City", y="Total Sales ($)") +
theme_minimal()



















