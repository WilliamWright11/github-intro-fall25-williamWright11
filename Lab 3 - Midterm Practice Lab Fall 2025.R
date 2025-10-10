#Lab 3 - Midterm Practice Lab

traindir  <- "C:/Users/willi/OneDrive/Special Topics in SIE/Data/"
sourcedir <- "C:/Users/willi/OneDrive/Special Topics in SIE/Source/"

housing <- read.csv("housing.csv")

#Q21 Use a box plot indicate whether the price variable has any outliers. If, so how many does it have?
boxplot(housing$price, main="Boxplot of Housing Prices", ylab="Price")
outliers <- boxplot.stats(housing$price)$out
num_outliers <- length(outliers)
num_outliers

#Q22 Use a box plot to determine the value of the upper whisker of Price. What is this value?
boxplot_stats <- boxplot.stats(housing$price)
upper_whisker <- boxplot_stats$stats[5]
upper_whisker

#Q23 Use a scatter plot matrix to identify the variable in the data set that has the strongest linear relationship with price. (bedrooms, baths, City, sqft)
pairs(housing[, c("price", "bedrooms", "baths", "sqft")], main="Scatter Plot Matrix")
# The variable that has the strongest linear relationship with price appears to be sqft.

#Q24 Use a scatter plot matrix to identify the correlation coefficient between price and the variable in the data set that has the strongest linear relationship with price (no rounding).
cor(housing$price, housing$sqft)
# The correlation coefficient between price and sqft is approximately 0.704.

#Q25 Which of the following statements are true about the scatter plot matrix of all variables in the houses data set? (All features have a Gaussian distribution, At least one of the variables is left skewed, There is a greater than 0.5 correlation between bedrooms and baths, The regression line through the observations is flat for all variable pairs)
# At least one of the variables is left skewed
# There is a greater than 0.5 correlation between bedrooms and baths
# What is the right answer
cor(housing$bedrooms, housing$baths)
# The correlation coefficient between bedrooms and baths is approximately 0.576.

#Q26 Generate a boxplot of bedrooms. Which of the following statements are true? (The upper whisker of the box plot is 4 bedrooms, The lower whisker of the box plot is 2, There is atleast one outlier in bedrooms, Fewer than 5 houses have 6 bedrooms.)
boxplot(housing$bedrooms, main="Boxplot of Bedrooms", ylab="Number of Bedrooms")
boxplot_stats_bedrooms <- boxplot.stats(housing$bedrooms)
upper_whisker_bedrooms <- boxplot_stats_bedrooms$stats[5]
lower_whisker_bedrooms <- boxplot_stats_bedrooms$stats[1]
outliers_bedrooms <- boxplot_stats_bedrooms$out
num_outliers_bedrooms <- length(outliers_bedrooms)
num_outliers_bedrooms
num_six_bedroom_houses <- sum(housing$bedrooms == 6)
upper_whisker_bedrooms
lower_whisker_bedrooms
num_six_bedroom_houses

#Q27 Which of the following statements are true about the house(s) with highest price in the data set? (Hint:  Use tidyverse and filter by City) (These homes have more than 1 bathroom, The square footage is greater than 5000, One of the observation numbers is 53, There are 3 houses in 3 different locations with the highest price.)
library(dplyr)
max_price <- max(housing$price)
most_expensive_houses <- filter(housing, price == max_price)
most_expensive_houses
num_most_expensive_houses <- nrow(most_expensive_houses)
num_most_expensive_houses
num_bathrooms <- most_expensive_houses$baths
num_bathrooms
square_footage <- most_expensive_houses$sqft
square_footage
observation_numbers <- rownames(most_expensive_houses)
observation_numbers
num_locations <- length(unique(most_expensive_houses$City))
num_locations
num_locations

#Q28 What is the total sum of price for all homes in Oxnard the data set?
oxnard_houses <- filter(housing, City == "Oxnard")
total_price_oxnard <- sum(oxnard_houses$price)
total_price_oxnard

#Q29 Now look at the total price for all homes in the data set, but organized by City.  Which city has the highest total dollar amount of homes? (Hint:  Use tidyverse and filter by City)
total_price_by_city <- housing %>%
  group_by(City) %>%
  summarise(total_price = sum(price)) %>%
  arrange(desc(total_price))
total_price_by_city
city_with_highest_total_price <- total_price_by_city$City[1]
city_with_highest_total_price

#Q30 How many levels are there for the City variable?
num_levels_city <- n_distinct(housing$City)
num_levels_city

library(tidyverse)
library(janitor)

# 1) Read data (adjust the path to your file)
pima <- read_csv("pima-indians-diabetes.csv")

#Q31 Which of the variables in this data set can we reasonably assume have missing values (hint: think carefully about what each of the columns represents)? (class, age, diabetespd, bmi, insulin, tricep, dbp, plasmaglucose, pregancies)
# The variables that can reasonably be assumed to have missing values are: bmi, insulin, tricep, dbp, plasmaglucose.

#Q32 How many missing values are there across the entire dataset?
total_missing_values <- sum(is.na(pima))
total_missing_values

#Q33 The total percentage of missing data rounded to the nearest % is:
total_values <- nrow(pima) * ncol(pima)
percentage_missing <- (total_missing_values / total_values) * 100
rounded_percentage_missing <- round(percentage_missing)
rounded_percentage_missing

#Q34 The most frequently missing variable is: (class, age, diabetesped, bmi, insulin, tricep, dbp, plasmaglucose, pregnancies)
missing_counts <- colSums(is.na(pima))
most_frequently_missing_variable <- names(which.max(missing_counts))
most_frequently_missing_variable
most_frequently_missing_variable
max_missing_count <- max(missing_counts)
max_missing_count
max_missing_count
# The most frequently missing variable is insulin with 374 missing values.

#Q35 The second most frequently missing variable is: (class, age, diabetesped, bmi, insulin, tricep, dbp, plasmaglucose, pregnancies)
sorted_missing_counts <- sort(missing_counts, decreasing = TRUE)
second_most_frequently_missing_variable <- names(sorted_missing_counts)[2]
second_most_frequently_missing_variable
second_most_frequently_missing_count <- sorted_missing_counts[2]
second_most_frequently_missing_count
second_most_frequently_missing_count

#Q36 If we were to remove observations with missing variables, we would still have how observations remaining?
pima_complete <- na.omit(pima)
num_remaining_observations <- nrow(pima_complete)
num_remaining_observations
num_remaining_observations
# If we were to remove observations with missing variables, we would still have 392 observations remaining.


