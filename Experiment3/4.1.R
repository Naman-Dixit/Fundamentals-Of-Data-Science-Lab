# Install necessary packages (if you haven't already)

# Load libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyr)


# Option 2: Use full file path
sales_data <- read.csv("C:\\Users\\naman\\OneDrive\\Desktop\\Data science lab\\Experiment4\\sales_data_sample.csv", stringsAsFactors = FALSE)

# Data Cleaning and Preparation (handle missing values, data types)
# Check for missing values
print(paste("Missing values in the dataset:", sum(is.na(sales_data))))

# Assuming 'ORDERDATE' needs to be converted to Date type
sales_data$ORDERDATE <- as.Date(sales_data$ORDERDATE)

# If you have a 'CUSTOMER_AGE' column, ensure it's numeric:
sales_data$CUSTOMER_AGE <- as.numeric(sales_data$CUSTOMER_AGE)

# Correct column name to avoid error
sales_data <- sales_data %>%
  rename(
    SALES = PRICEEACH
  )

# 1. Bar Chart: Total Sales by Category
sales_by_category <- sales_data %>%
  group_by(CATEGORY) %>%
  summarise(Total_Sales = sum(SALES))

ggplot(sales_by_category, aes(x = CATEGORY, y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Total Sales by Category", x = "Category", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate x-axis labels

# 2. Line Graph: Sales Trends Over Time
# Assuming you have a date column called ORDERDATE
sales_over_time <- sales_data %>%
  group_by(ORDERDATE) %>%
  summarise(Sales = sum(SALES))

ggplot(sales_over_time, aes(x = ORDERDATE, y = Sales)) +
  geom_line(color = "blue") +
  labs(title = "Sales Trends Over Time", x = "Date", y = "Sales") +
  theme_minimal()

# 3. Heatmap: Correlation between Numerical Variables
# Select numerical columns for correlation analysis (adjust column names if needed)
numerical_data <- sales_data %>%
  select(SALES, QUANTITYORDERED, PRICEEACH)

# Calculate the correlation matrix
correlation_matrix <- cor(numerical_data)

# Plot the correlation heatmap
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# 4. Box Plot: Profit Distribution Across Regions
# Assuming you have a 'REGION' and 'PROFIT' column
ggplot(sales_data, aes(x = TERRITORY, y = PRICEEACH)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Profit Distribution Across Regions", x = "Region", y = "Profit") +
  theme_minimal()

# 5. Histogram: Distribution of Customer Ages
# Assuming you have a 'CUSTOMER_AGE' column

# 6. Scatter Plot: Relationship between Sales and Profit
# Assuming you have 'SALES' and 'PROFIT' columns (adjust if needed)
ggplot(sales_data, aes(x = QUANTITYORDERED, y = SALES)) +
  geom_point(color = "purple") +
  labs(title = "Relationship between Sales and Profit", x = "Sales", y = "Profit") +
  theme_minimal()

# 7. Pie Chart: Proportion of Sales from Each Region
sales_by_region <- sales_data %>%
  group_by(TERRITORY) %>%
  summarise(Total_Sales = sum(SALES))

ggplot(sales_by_region, aes(x = "", y = Total_Sales, fill = TERRITORY)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Proportion of Sales from Each Region", fill = "Region") +
  theme_void()

# 8. Violin Plot: Customer Rating Distribution Across Categories
# Assuming you have 'CATEGORY' and 'CUSTOMER_RATING' columns
ggplot(sales_data, aes(x = CATEGORY, y = CUSTOMER_RATING)) +
  geom_violin(fill = "orange") +
  labs(title = "Customer Rating Distribution Across Categories", x = "Category", y = "Rating") +
  theme_minimal()

# 9. Multiple Line Plots: Sales Trends for Different Categories Over Time
# Assuming you have 'ORDERDATE' and 'CATEGORY' columns
 sales_trends_by_category <- sales_data %>%
  group_by(ORDERDATE, CATEGORY) %>%
  summarise(Sales = sum(SALES))

 ggplot(sales_trends_by_category, aes(x = ORDERDATE, y = Sales, color = CATEGORY)) +
  geom_line() +
  labs(title = "Sales Trends for Different Categories Over Time", x = "Date", y = "Sales") +
  theme_minimal()

# 10. Stacked Bar Chart: Total Sales Across Categories, Segmented by Region
sales_by_category_region <- sales_data %>%
  group_by(CATEGORY, TERRITORY) %>%
  summarise(Total_Sales = sum(SALES))

ggplot(sales_by_category_region, aes(x = CATEGORY, y = Total_Sales, fill = TERRITORY)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Total Sales Across Categories, Segmented by Region", x = "Category", y = "Total Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 11. Multiple Variable Correlation Heatmap
# Assuming you have 'Sales', 'Profit', 'Customer_Age', and 'Rating'
numerical_data <- sales_data %>%
  select(SALES, QUANTITYORDERED, PRICEEACH)

correlation_matrix <- cor(numerical_data)

corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# 12. Dual-Axis Plot: Sales and Profit Trends
 sales_trends <- sales_data %>%
  group_by(ORDERDATE) %>%
  summarise(Sales = sum(SALES), Profit = sum(PROFIT))

 sales_trends_long <- sales_trends %>%
  gather(key = "Variable", value = "Value", Sales, Profit)

 ggplot(sales_trends_long, aes(x = ORDERDATE, y = Value, color = Variable)) +
  geom_line() +
  labs(title = "Sales and Profit Trends Over Time", x = "Date", y = "Value") +
  theme_minimal()
