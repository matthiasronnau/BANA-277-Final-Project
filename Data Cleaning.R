#Matthias Ronnau
#Customer and Social Analytics-277
#Professor Dewan
#March 28, 2021

#Final Project

#Set Working Directory
setwd("~/The University of California, Irvine/Winter Quarter/Customer and Social Analytics-277/Final Project")

#Set options
options(scipen = 999)
set.seed(277)

###Load Packages
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

#Read the Data into R
data <- read_csv("Data/2020-Apr.csv")
head(data)

#Take a sample for logistic regression
samp <- sample_frac(data, 0.001)
write.table(samp, row.names = FALSE, col.names = colnames(samp), sep = ",", file = "Data/Cleaned Data/sample.csv")
rm(samp)

#Data Cleaning
purchases <- subset(data, data$event_type == "purchase")
rm(data)
purchases$event_time <- as_datetime(purchases$event_time)
head(purchases)

purchases_no_missing <- na.omit(purchases)
rm(purchases)
head(purchases_no_missing)

#Make a column for the weekday
purchases_no_missing$weekday <- wday(purchases_no_missing$event_time, label = TRUE)

#Capitalize the brand names
purchases_no_missing$brand <- str_to_title(purchases_no_missing$brand)

#Make a column for the general category a column is in
unique(purchases_no_missing$category_code)
length(unique(purchases_no_missing$category_code))

purchases_no_missing$category <- str_to_title(sub("[[:punct:]].+", "", purchases_no_missing$category_code))

unique(purchases_no_missing$category)
length(unique(purchases_no_missing$category))

#Export cleaned data file into a csv
write.table(purchases_no_missing, row.names = FALSE, col.names = colnames(purchases_no_missing), sep = ",", file = "Data/Cleaned Data/purchases_no_missing.csv")



