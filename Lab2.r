####### Data Analytics Fall 2025 Lab 2 ######

library(ggplot2)

# read in data
ny <- read.csv("Downloads/NY-House-Dataset.csv", header=TRUE)

View(ny)

# Convert variables to log so they can be more easily compared
ny$log_price <- log(ny$PRICE)
ny$log_sqft <- log(ny$PROPERTYSQFT)

# First model: Log of the price vs. beds, baths, and log of square feet
model1 <- lm(log_price~BEDS + BATH + log_sqft,ny)

summary(model1)

ggplot(ny, aes(x = log_sqft, y = log_price)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Model 1: Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

# Second model: Log of the price vs. beds and baths
model2 <- lm(log_price~BEDS + BATH,ny)

summary(model2)

ggplot(ny, aes(x = BATH, y = log_price)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Model 2: Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

# Third model: Log of the price vs. beds and log of square feet
model3 <- lm(log_price~BEDS + log_sqft,ny)

summary(model3)

ggplot(ny, aes(x = BEDS, y = log_price)) +
  geom_point() +
  stat_smooth(method = "lm")

ggplot(model1, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Model 3: Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')