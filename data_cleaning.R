#loading the dataset
library(readxl)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(DMwR2)
library(FactoMineR)

data <- read.csv("incom2024_delay_example_dataset.csv")
str(data)

#change the datatype of order_date and shipping_date to Date
#NOTE: data's date included time after conversation it does not
data <- data %>%
  mutate(across(c(order_date, shipping_date), as.Date))

#handle missing values
colSums(is.na(data))
#no missing data

#check if our data set is imbalanced or not
#delivery outcomes: -1 early arrival, 0 on time, 1 delayed
data %>% count(label)
str(data)
#our data is imbalanced so need to apply an appropriate technique 
ggplot(data, aes(x = label)) + geom_bar() + ggtitle("Class Distribution")

# RANDOM SAMPLE
# Find the minimum number of samples among the labels
min_samples <- min(table(data$label))
min_samples

# Randomly sample min_samples from each class
undersampled_data <- data %>%
  group_by(label) %>%
  sample_n(min_samples) %>%
  ungroup()
str(undersampled_data)

#converting categorical to numerical


#1 PAYMENT_TYPE
undersampled_data %>% count(payment_type)

#Perform one-hot encoding
dummies <- dummyVars(" ~ payment_type", data = undersampled_data)
data_encoded <- predict(dummies, newdata = undersampled_data)
data_encoded <- data.frame(data_encoded)

undersampled_data <- cbind(undersampled_data[, -which(names(undersampled_data) == "payment_type")], data_encoded)
str(undersampled_data)

# customer_city we will do frequency encoding
frequency_cities <- table(undersampled_data$customer_city)
frequency_cities
length(frequency_cities)

undersampled_data$customer_city_freq <- frequency_cities[as.character(undersampled_data$customer_city)]
str(undersampled_data)

# to ensure that our frequency encoding applied correctly we recheck it
missing_categories <- setdiff(undersampled_data$customer_city, names(frequency_cities))
missing_categories


# CUSTOMER_SEGMENT
undersampled_data %>% count(customer_segment)

# Group by `customer_segment` and `label`, then summarize the count for each group
result <- undersampled_data %>%
  group_by(customer_segment, label) %>%
  summarize(count = n()) %>%
  arrange(customer_segment, label)

result

#so we will consider this feature and do one hot encoding
dummies <- dummyVars(" ~ customer_segment", data = undersampled_data)
data_encoded <- predict(dummies, newdata = undersampled_data)
data_encoded <- data.frame(data_encoded)

undersampled_data <- cbind(undersampled_data[, -which(names(undersampled_data) == "customer_segment")], data_encoded)
str(undersampled_data)


#ORDER COUNTRY
undersampled_data %>% count(order_country)

#since around 137 order countries so we will apply frequency encoding
frequency_country <- table(undersampled_data$order_country)
frequency_country
length(frequency_country)

undersampled_data$frequency_country <- frequency_country[undersampled_data$order_country]
str(undersampled_data)

#DEPARTMENT NAME
undersampled_data %>% count(department_name)

#frequency encoding
freq_dep_name <- table(undersampled_data$department_name)
freq_dep_name
length(freq_dep_name)

undersampled_data$frequency_dep_name <- freq_dep_name[undersampled_data$department_name]
str(undersampled_data)

#order_profit_per_day
# Count values less than 0
count_less_than_0 <- sum(undersampled_data$order_profit_per_order < 0)

# Count values greater than 0
count_greater_than_0 <- sum(undersampled_data$order_profit_per_order > 0)

count_greater_than_0
count_less_than_0

range <- max(undersampled_data$order_profit_per_order) - min(undersampled_data$order_profit_per_order)
range
summary(undersampled_data$order_profit_per_order)

#we will apply z-score normalization
undersampled_data$normalized_profit <- scale(undersampled_data$order_profit_per_order)
str(undersampled_data)

range <- max(normalized_profit) - min(normalized_profit)
range
summary(range)

#ORDER STATUS
undersampled_data %>% count(order_status)
#try with one hot encoding fist and let us see the result
dummies <- dummyVars(" ~ order_status", data = undersampled_data)
data_encoded <- predict(dummies, newdata = undersampled_data)
data_encoded <- data.frame(data_encoded)

undersampled_data <- cbind(undersampled_data[, -which(names(undersampled_data) == "order_status")], data_encoded)

#SHIPPING MODE
undersampled_data %>% count(shipping_mode)
# only 4 values so one hot encoding

dummies <- dummyVars(" ~ shipping_mode", data = undersampled_data)
data_encoded <- predict(dummies, newdata = undersampled_data)
data_encoded <- data.frame(data_encoded)

undersampled_data <- cbind(undersampled_data[, -which(names(undersampled_data) == "shipping_mode")], data_encoded)
str(undersampled_data)

# ORDER DATE AND SHIPPING DATE
# Calculate the difference and add as a new column
date_dif <- undersampled_data %>%
  mutate(date_diff = as.numeric(difftime(shipping_date, order_date, units = "days")))
date_dif

count_positive_diff <- sum(date_dif$date_dif > 0)
print(count_positive_diff)

count_negative_diff <- sum(date_dif$date_diff < 0)
print(count_negative_diff)

date_data <- undersampled_data %>%
  select(order_date, shipping_date) %>%
  arrange(order_date)  # Optional: Arrange by ordered_date if needed 

#get the dates where the difference is negative
tail(date_data)
