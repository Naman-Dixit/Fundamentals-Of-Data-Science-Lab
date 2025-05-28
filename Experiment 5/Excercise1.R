
# %%
# Advertising Budget (in $1000) Monthly Sales (in $1000)
data <- data.frame(
    AdvertisingBudget = c(1,2,3,4,5),
    MonthlySales = c(4,5,7,8,11)
)
# Takign AdvertisingBudget as Independent cause vibes also cause more budget = more sales

# %% md
Perform a linear regression to predict sales based on the advertising budget using R
# %%
n = length(data$AdvertisingBudget)
y_sum = sum(data$MonthlySales)
x_sum = sum(data$AdvertisingBudget)

print(n * sum(data$AdvertisingBudget * data$AdvertisingBudget))
# %%
slope = (n * sum(data$AdvertisingBudget * data$MonthlySales) - x_sum * y_sum) / (n * sum(data$AdvertisingBudget * data$AdvertisingBudget) - x_sum * x_sum)
intercept = (y_sum   - slope * x_sum) / n


# %%
print(intercept)
print(slope)
# %%
model <- lm(MonthlySales ~ AdvertisingBudget, data = data)
print(model)
# %%
library(ggplot2)
ggplot(data, aes(x = data$AdvertisingBudget, y = data$MonthlySales)) +
  geom_point(color="red", size=2) +
  geom_abline(mapping=NULL,slope=slope, intercept=intercept)
