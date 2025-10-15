#Midterm Part B - 2025
# set working directory

setwd("C:/Users/willi/OneDrive/Special Topics in SIE/Source/")

traindir  <- "C:/Users/willi/OneDrive/Special Topics in SIE/Data/"
sourcedir <- "C:/Users/willi/OneDrive/Special Topics in SIE/Source/"

# Build the full file path and read
df <- read.csv(file.path(traindir, "housing-prices.csv"))



#Q1 What is the row index of the house with the lowest price?
housing <- df
which.min(housing$price)
#Answer: 126


#Q2 What is the square footage of the most expensive house?
which.max(housing$price)
housing$size[which.max(housing$price)]
#Answer: 13540


#Q3 Use a box plot to determine whether the "Size" variable has any outliers. If so, how many does it have?
boxplot(housing$size, main="Boxplot of Size", ylab="Size")
Q1 <- boxplot.stats(housing$size)
outliers <- Q1$out
length(outliers)
#Answer: 7

#Q4 What is the maximum value of the outliers in Size?
max(outliers)
#Answer: 13540

#Q5 Create a new data frame with the outliers from the Size variable, then answer the following two questions. What is the mean of Size of these outlier houses (rounded to the nearest integer)?
outlier_houses <- housing[housing$size %in% outliers, ]
mean(outlier_houses$size)
#Answer: 11763

#Q6 Which of the following statements is true about the outlier houses? (The average price of the houses identified as outliers in the Size variable is around $545,000., Most of the houses with outlier sizes have 3 to 5 bedrooms (as indicated by the Rooms Variable)., The maximum price of the outlier houses is $599,900., All of these outlier houses are New.)
mean(outlier_houses$price) # around $545,000
table(outlier_houses$rooms) # Most of the houses with outlier sizes have 3 to 5 bedrooms
max(outlier_houses$price) # The maximum price of the outlier houses is $599,900
table(outlier_houses$condition) # All of these outlier houses are New.

#Q7 Which of the following statements are true about the box plot of Bath? (Few houses have 3 or more bathrooms. (The median, lower quartile, upper quartile, and whiskers are all the same., The upper whisker of the box plot is 4 bathrooms., The lower whisker of the box plot is 2 bathrooms.)
boxplot(housing$baths, main="Boxplot of Baths", ylab="Baths")
table(housing$baths) # Few houses have 3 or more bathrooms.
summary(housing$baths) # The median, lower quartile, upper quartile, and whiskers are all the same.
# The upper whisker of the box plot is 4 bathrooms.
# The lower whisker of the box plot is 2 bathrooms.
#Answer: Few houses have 3 or more bathrooms.

#Q8 Use a scatter plot matrix or individual scatter plots to determine the variable in the data set that has the strongest linear relationship with Size. What is it? (Baths, Age, Price, Rooms)
pairs(housing[, c("size", "baths", "age", "price", "rooms")])
cor(housing[, c("size", "baths", "age", "price", "rooms")], use="complete.obs")
#Answer: Price

#Q9 Which of the following statements is true about the histogram of Size? (Size is normal distributed., Most houses have a size between 1,000 and 2,000 square feet., Price is right-skewed with many large houses., There are no houses with a size greater than 3000 square feet.)
hist(housing$size, main="Histogram of Size", xlab="Size", breaks=30)
#Answer: Most houses have a size between 1,000 and 2,000 square feet.

#Q10 What is the total sum of price for all Old homes in the data set? 
sum(housing$price[housing$age == "Old"], na.rm=TRUE)
#Answer: 1.034e+10

#Q11 Make a frequency heat map of Baths and Rooms. Answer the following question. *You will have to convert both Rooms and Baths to a categorical variable. (hint: use as.factor) Which of the following statements is true?(There are no houses with 4 bathrooms irrespective of the number of bedrooms., The most frequent combination of homes has 3 bedrooms and 2 bathrooms., All 1 bedroom homes have 2 or more bathrooms., The most frequent combination of homes has 4 bedrooms and 2 bathrooms.)
library(ggplot2)
housing$rooms <- as.factor(housing$rooms)
housing$baths <- as.factor(housing$baths)
ggplot(housing, aes(x=rooms, y=baths)) +
  geom_bin2d() +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  labs(title="Frequency Heat Map of Baths and Rooms", x="Rooms", y="Baths") +
  theme_minimal()
table(housing$rooms, housing$baths)
#Answer: The most frequent combination of homes has 3 bedrooms and 2 bathrooms.

# Install It
install.packages("VIM")
# Load the VIM library
library(VIM)

# Load the airquality dataset
data("airquality")

# "airquality" loaded into your working environment. Display the first few rows of the dataset
head(airquality)

#Q12 Which of the following variables are 100% complete? (Ozone, Solar, Wind, Temp, Month, Day)
colSums(is.na(airquality))
#Answer: Month, Day

#Q13 Which month (numeric format, e.g., 9 for September) has the highest average temperature across all days?
aggregate(Temp ~ Month, data=airquality, FUN=mean, na.rm=TRUE)
#Answer: 7

#Q14 Create a histogram for Solar.R in the airquality dataset. Answer the following questions.
hist(airquality$Solar.R, main="Histogram of Solar.R", xlab="Solar.R", breaks=20)
#Is the distribution of Solar Radiation skewed?
#Answer: Yes, it is right-skewed.

#Q15 What is the most frequent range of Solar Radiation observed in the dataset?
table(cut(airquality$Solar.R, breaks=seq(0, 350, by=50), right=FALSE))
#Answer: 100-150

#Q16 Which of the following statements are true about the histogram (The maximum observed Solar Radiation value is approximately 450, There are no observations of Solar Radiation in the range of 0-50., The Solar Radiation data follows a normal distribution., There are more than 30 Solar Radiation values below or equal to 100.)
#Answer: There are more than 30 Solar Radiation values below or equal to 100.

#Q17 Draw scatterplot matrix for "Ozone", "Solar.R", "Wind", "Temp" in the airquality dataset.
pairs(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")])
cor(airquality[, c("Ozone", "Solar.R", "Wind", "Temp")], use="complete.obs")
#Which of the following statements are true based on what you can observe in the scatterplot matrix? (Wind is positively correlated with Solar.R., Wind is most strongly correlated with Ozone., There is a strong negative correlation between Ozone and Temperature, The strongest corelation is between Temperature and Ozone.)
#Answer: Wind is most strongly correlated with Ozone.

#Q18 Which of the following shows evidence of skew in its distribution? (Wind, Temp, Ozone)
hist(airquality$Wind, main="Histogram of Wind", xlab="Wind", breaks=20)
hist(airquality$Temp, main="Histogram of Temp", xlab="Temp", breaks=20)
hist(airquality$Ozone, main="Histogram of Ozone", xlab="Ozone", breaks=20)
#Answer: Ozone

#Q19 Which of the following is the most likely to be normally distributed? (Solar.R, Ozone, Temp)
shapiro.test(airquality$Solar.R)$p.value
shapiro.test(airquality$Ozone)$p.value
shapiro.test(airquality$Temp)$p.value
#Answer: Temp

#Q20 Create box plots for "Wind" by "Month" to visualize the distribution of Wind across different Months. 
boxplot(Wind ~ Month, data=airquality, main="Boxplot of Wind by Month", xlab="Month", ylab="Wind")
#Which of the following are true. (There is a trend that, from May (5) to September (9), the median monthly wind speed increases over time., June (6) is the month with the smallest interquartile range (lQR) for wind speed?, August (8) is the month with the highest median wind speed?, June (6) is the only month that has outliers for wind speed., September (9) has the lowest first quartile of wind speed compared to other months.)
#Answer: September (9) has the lowest first quartile of wind speed compared to other months.



