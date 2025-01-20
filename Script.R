
# =================================================== #
# Assignment 1
# Author: Tareq Al Hossain Tomal
# Email: tah.tomaal@gmail.com
# Date of submission: 20th January 2025
# =================================================== #


# Task 1 ------------------------------------------------------------------
# Read the data from the folder using suitable function into a variable named "df1"
# You will find details of the variables in the excel sheet named "Data Dictionary"
#___________________________________Solution____________________________________

#install.packages("readxl")
library(readxl)

setwd("C:/Users/IT BD/Desktop/Programming/L07 - Assignment 1 on Class 1 to Class 7/Data")
df1 <- read_xlsx("StudentSurveyData.xlsx")
head(df1)


# Task 2 ------------------------------------------------------------------
# Check structure of the data
#___________________________________Solution____________________________________

str(df1)

# Task 3 ------------------------------------------------------------------
# See summary of the data frame
#___________________________________Solution____________________________________
summary(df1)

# Task 4 ------------------------------------------------------------------
# (1) Use the function unique() to see the unique categories in the Major column of the data frame.
#___________________________________Solution____________________________________
unique(df1$Major)


# (2) Use the function table() to see the frequency of each category in Major column in the data frame.
#___________________________________Solution____________________________________
table(df1$Major)


# Task 5 ------------------------------------------------------------------
# Create a new data frame named df2, where the "IS" category in Major column is replaced by "Information Systems"
#___________________________________Solution____________________________________

#install.packages("dplyr")
library(dplyr)

df2 <- df1 %>% 
  mutate(Major = ifelse(Major == "IS", "Information System", Major))
df2

# Check frequency of categories in Major column again.
#___________________________________Solution____________________________________
table(df2$Major)



# Task 6 ------------------------------------------------------------------
# Run the following code ONLY ONCE (DO NOT CHANGE ANYTHING).
set.seed(2025)
df2$Spending[sample(nrow(df2), nrow(df2) * 0.05)] <- NA  # introduces 5% missing values
df2$`Text Messages`[sample(nrow(df2), nrow(df2) * 0.1)] <- NA  # introduces 10% missing values

# Create a function called 'data_na_count' with one argument (data) that returns 
# a vector with number of missing values in each columns
#___________________________________Solution____________________________________
any(is.na(df2))
sum(any(is.na(df2)))

data_na_count <- sapply(df2, function(df2) sum(is.na(df2)))

data_na_count



# Task 7 ------------------------------------------------------------------
# Create a new data frame named df_male that contains data for male students only.
#___________________________________Solution____________________________________

df_male <- df2[df2$Gender == "Male", ]
df_male

# How many missing values are there in the Spending column of the df_male data frame?
# Your answer: 
any(is.na(df_male$Spending))
sum(any(is.na(df_male$Spending)))


# Task 8 ------------------------------------------------------------------
# From the df2 data frame find the top 3 students having highest GPA who are 
# unemployed and are currently in 'Senior' class

#___________________________________Solution____________________________________
df2
 
top_3 <- df2 %>% 
  filter(Employment == "Unemployed", Class == "Senior") %>%
  arrange(desc(GPA)) %>% 
  slice_head(n = 3) 
top_3

#head(top_3,3)





# Task 9 ------------------------------------------------------------------
# In the df2 data frame, change data type of all the character columns to factor columns
# Do not create a new data frame
# Check the data type of the columns by inspecting the data frame using str() 

#___________________________________Solution____________________________________
df2

df2 <- df2 %>%
  mutate(across(where(is.character), as.factor))

str(df2)

# See the summary of the new data frame
summary(df2)


# Task 10 -----------------------------------------------------------------
# Select only those students who use Laptop for study. 
#___________________________________Solution____________________________________
df3_laptop <- df2 %>% 
  filter(Computer == "Laptop")
df3_laptop

# Export this data into an Excel file.
#install.packages("writexl")
library(writexl)

write_xlsx(df3_laptop, "students_with_laptop.xlsx")

