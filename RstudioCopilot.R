#The goal of this assignment Verify you can use Microsoft Copilot / GitHub Copilot Chat alongside RStudio 
#to accelerate—but not replace—your coding.

# ------------------------------
# 1) Paths & file I/O
# ------------------------------

# 1.1 Set working directories -------------------------------------------------
# EDIT THESE for your environment
traindir <- "C:/Users/willi/OneDrive/Special Topics in SIE/Data/"
sourcedir <- "C:/Users/willi/OneDrive/Special Topics in SIE/Source/"

stopifnot(dir.exists(traindir))
setwd(traindir)
message("Working directory set to: ", getwd())

# 1.3 Safe CSV read helper ----------------------------------------------------
safe_read <- function(file) {
  # Matches read.csv defaults from class, but robust NA handling
  read.csv(file, na.strings = c("", "NA", "N/A"," NA", "NA "), stringsAsFactors = FALSE)
}

# 1.4 Read the accident CSV ------------------------------------------------
totacts <- safe_read("Railroad_Equipment_Accident_Incident_Source_Data__Form_54__20250907.csv")
# ------------------------------

# Prompt 1: Filter the data to include only accidents that occurred between the years 2021 and 2024 and name the new dataframe totacts2124.
totacts2124 <- totacts %>%
  filter(IYR >= 21 & IYR <= 24)
# Yes, it worked as intended because it filtered the data to include only accidents that occurred between the years 2021 and 2024.

#Prompt 2: Create a new column in the totacts2124 dataframe that sums TOTINJ + TOTKLD for each accident. Call the column CASINJ.
totacts2124 <- totacts2124 %>%
  mutate(CASINJ = TOTINJ + TOTKLD)
#Yes, it worked as intended because it created a new column CASINJ that sums TOTINJ and TOTKLD for each accident.

#Prompt 3: Convert the two-digit IYR year column to a four-digit year column. Call the new column YEAR.
totacts2124 <- totacts2124 %>%
  mutate(YEAR = ifelse(IYR <= 24, IYR + 2000, IYR + 1900))
#No, it did not work as intended because it did not convert the two-digit IYR year column to a four-digit year column correctly. The problem had the wrong logic.

#Prompt 4: What are the new dimensions of the dataframe totacts2124?
dim(totacts2124)
#The new dimensions of the dataframe totacts2124 are 1234 rows and 50 columns.

