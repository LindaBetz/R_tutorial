library(tidyverse)

data_cobre <- read_csv("data_cobre.csv")

# ------ 1: EXERCISES ------ 
# Find all participants who are 19 years and younger. How many are there?
filter(data_cobre, age <= 19) # 5

# Find all healthy control participants who are 19 years and younger. How many are there?
filter(data_cobre, study_group == "control" & age <= 19) # 2

# Find all men who are 22, 25 or 27 years old. How many are there?
filter(data_cobre, gender == "male" & age %in% c(22, 25, 27)) # 11

# How many patients have a missing value in the variable `age_onset_psychiatric_illness`?
filter(data_cobre, study_group == "schizophrenia" & is.na(age_onset_psychiatric_illness)) # 1


# ------ 2: EXERCISES ------ 
# Select all variables which contain the words "study" or "age".
select(data_cobre, matches("study|age"))

# Rename all variables such that they contain upper case letters only.
rename_with(data_cobre, toupper)


# ------ 3: EXERCISES ------ 
# Z-transform the variable `age`, and store the result in a new variable called `age_z_score`.
mutate(data_cobre, age_z_score = (age - mean(age)) / sd(age))

