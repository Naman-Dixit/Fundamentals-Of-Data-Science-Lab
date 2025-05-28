# Load required libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)

# Load Dataset
df <- read.csv("C:/Users/naman/OneDrive/Desktop/Data science lab/Experiment4/Ecommerce_Delivery_Analytics_New.csv")

# Check the column names to confirm structure
colnames(df)

# Since 'Order Date & Time' is not a date, assume a hypothetical 'Order_Date' column exists
# Check the first few rows of the assumed 'Order_Date' column
head(df$Order_Date)

# Convert 'Order_Date' to Date format (assuming it's in "dd-mm-yyyy" format)
df$Order_Date <- as.Date(df$Order_Date, format="%d-%m-%Y")

# Verify conversion worked
head(df$Order_Date)

# 1. Bar Chart: Total Order Value Across Product Categories
p1 <- ggplot(df, aes(x = `Product Category`, y = `Order Value (INR)`, fill = `Product Category`)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() +
  labs(title = "Total Order Value by Product Category", x = "Product Category", y = "Total Order Value (INR)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
print(p1)

# 2. Line Graph: Order Value Trends Over Time
# Group by Order_Date and summarize total Order Value
order_value_by_date <- df %>% 
  group_by(Order_Date) %>% 
  summarize(TotalOrderValue = sum(`Order Value (INR)`, na.rm = TRUE))

p2 <- ggplot(order_value_by_date, aes(x = Order_Date, y = TotalOrderValue, group = 1)) +
  geom_line(color = "blue") + 
  theme_minimal() +
  labs(title = "Order Value Trend Over Time", x = "Date", y = "Total Order Value (INR)")
print(p2)

# 3. Heatmap: Correlation Between Numerical Variables
# Select numerical columns: Order Value (INR), Delivery Time (Minutes), Service Rating
cor_matrix <- cor(df %>% select(`Order Value (INR)`, `Delivery Time (Minutes)`, `Service Rating`), use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         title = "Correlation Heatmap of Numerical Variables", 
         mar = c(0, 0, 1, 0))  # Adjust margins for title