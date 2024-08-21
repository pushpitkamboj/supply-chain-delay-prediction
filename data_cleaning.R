#loading the dataset
library(readxl)
library(dplyr)
library(stringr)
library(readxl)
library(writexl)
library(dplyr)

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

#our data is imbalanced so need to apply an appropriate technique 
