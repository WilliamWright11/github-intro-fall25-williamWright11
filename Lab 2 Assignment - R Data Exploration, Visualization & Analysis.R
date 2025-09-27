#Lab 2 Assignment - R Data Exploration, Visualization & Analysis

#Using R, read in the entire train accident data set. Then, write code to save separate CSV files containing all the data for each year in a new sub-folder: create one file for each year. Give the files names that make them easy to identify based on their year. Based on the resulting files, which of the following are true?
# --- Setup ---------------------------------------------------------------
sessionInfo()

traindir  <- "C:/Users/willi/OneDrive/Special Topics in SIE/Data/"
sourcedir <- "C:/Users/willi/OneDrive/Special Topics in SIE/Source/"
setwd(traindir)
getwd()

# File name (edit if yours is different)
big_csv <- "Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv"
df <- read_csv(big_csv)

# convert year column 21 -> 2021
df$YEAR
df$YEAR <- ifelse(as.integer(df$IYR) <= 25,
                  as.integer(df$IYR) + 2000L,
                  as.integer(df$IYR) + 1900L)

df$YEAR

# Q10: Save each year file in a new subfolder
unique_year_list <- unique(df$YEAR)


for (year in unique_year_list){
  year_df <- df[df$YEAR == year, ]
  write.csv(year_df, file = paste0("data_by_year/train_", year, ".csv"), row.names = FALSE)
}


# Q11
df$HIGHSPD <- as.integer(df$HIGHSPD)
df$HIGHSPD
summary(df$HIGHSPD)

df50 <- df[df$HIGHSPD > 50, ]
dim(df50)
df30 <- df[df$HIGHSPD > 30, ]
df10 <- df[df$HIGHSPD < 10, ]
dim(df10)
print(dim(df)[1]/2)

#Q12
# Assuming your dataset is in the data frame train
summary(train$TEMP)

# To count extreme values
sum(train$TEMP > 100, na.rm = TRUE)   # how many accidents above 100°F
sum(train$TEMP < -50 | train$TEMP > 130, na.rm = TRUE)  # implausible outliers

#Q18
# Find the row index of the maximum injuries
idx <- which.max(train$TOTINJ)

#Q19
# Get the incident number and details
train[idx, c("INCDTNO", "YEAR", "TOTINJ")]

library(ggplot2)
library(dplyr)

# Filter data for 2021 and later
recent <- train %>% filter(YEAR >= 2021)

# QQ plots
qqplot_fun <- function(x, title){
  ggplot(data.frame(x=x), aes(sample=x)) +
    stat_qq() + stat_qq_line(color="red") +
    ggtitle(title)
}

qqplot_fun(recent$TONS, "QQ Plot of TONS (>=2021)")
qqplot_fun(recent$TOTINJ, "QQ Plot of TOTINJ (>=2021)")
qqplot_fun(recent$TEMP, "QQ Plot of TEMP (>=2021)")

# Exact value ggplot would use (same rule as Tukey)
uw <- boxplot.stats(train$ACCDMG[!is.na(train$ACCDMG)])$stats[5]
uw
uw <- boxplot.stats(train$ACCDMG)$stats[5]
uw
library(dplyr)

train %>%
  mutate(YEAR4 = ifelse(YEAR <= 24, YEAR + 2000L, YEAR + 1900L)) %>%
  filter(YEAR4 >= 2001, YEAR4 <= 2023) %>%
  summarise(n_outliers = sum(ACCDMG > uw, na.rm = TRUE))


