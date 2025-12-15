# Loan Default Risk Analysis

#Load the libraries
library(tidyverse)
library(lubridate)
library(caret)

# Load data
loan <- read.csv("data/Loan payments data.csv")

# Basic structure check
str(loan)
summary(loan)

#Checking dimension of dataset
dim(loan)

#Checking missing values 
colSums(is.na(loan))

#Cleaning and Preparing the dataset
# Standardise column names
names(loan) <- tolower(names(loan))
names(loan) <- gsub(" ", "_", names(loan))

# Check column names
print(names(loan))



#  Create TARGET VARIABLE: DEFAULT
# 1 = Default, 0 = Not Default
loan <- loan %>%
  mutate(
    default = ifelse(loan_status %in% c("Charged Off", "Default"), 1, 0)
  )

#Check default distribution
table(loan$default)
prop.table(table(loan$default))

# Converting the  data types
loan <- loan %>%
  mutate(
    loan_status = as.factor(loan_status),
    terms = as.factor(terms),
    education = as.factor(education),
    gender = as.factor(gender)
  )

# Converting to  numeric variables
loan$principal <- as.numeric(loan$principal)
loan$age <- as.numeric(loan$age)
loan$past_due_days <- as.numeric(loan$past_due_days)

# Dropping corrupted date columns 
loan$effective_date <- NULL
loan$due_date <- NULL
loan$paid_off_time <- NULL


#Check for any  missing values
print(colSums(is.na(loan)))

#Handle missing values (simple & safe)
loan <- loan[!is.na(loan$default) &
               !is.na(loan$principal) &
               !is.na(loan$age), ]

# FEATURE ENGINEERING
# Age groups
loan$age_group <- ifelse(
  loan$age < 25, "Young",
  ifelse(loan$age < 45, "Middle", "Senior")
)

# Loan size groups
loan$loan_size <- ifelse(
  loan$principal < 500, "Small",
  ifelse(loan$principal < 1500, "Medium", "Large")
)

#Convert new features to factors
loan$age_group <- as.factor(loan$age_group)
loan$loan_size <- as.factor(loan$loan_size)

# Final cleaned summary
summary(loan)

#  Save cleaned dataset
write.csv(loan, "loan_cleaned.csv", row.names = FALSE)
