library(tidyverse)

data_cobre <- read_csv("data_cobre.csv")

# ------ 4: EXERCISES ------
# Change the levels of of the variable `first_degree_relative_psychosis` from `TRUE` and `FALSE` to "yes" and "no".
data_cobre %>%
  mutate(
    first_degree_relative_psychosis = if_else(
      first_degree_relative_psychosis == TRUE,
      true = "yes",
      false = "no"
    )
  )

# Create a new variable `young_participant` that indicates if a participant is younger than the median age in the sample.
data_cobre %>%
  transmute(age, young_participant = if_else(age < median(age),
                                             true = "yes", # you can replace "yes"/"no" with "young"/"old", etc.
                                             false = "no"))

# cam also use mutate (will add variable at the end of your data frame)
data_cobre %>%
  mutate(young_participant = if_else(age < median(age),
                                     true = "yes", # you can replace "yes"/"no" with "young"/"old", etc.
                                     false = "no"))

# Compute the mean (sd) age for patients who have a first degree relative with a psychotic disorder and those who do not.
data_cobre %>%
  filter(study_group == "schizophrenia" & # only retain patients
           !is.na(first_degree_relative_psychosis)) %>% # remove those with "NA" in variable "first_degree_relative_psychosis"
  group_by(first_degree_relative_psychosis) %>%
  summarize(age_mean = mean(age),
            age_sd = sd(age))

# Recode the values in the variables `age_onset_psychiatric_illness` and `age_first_hospitalization` such that they can be analyzed properly.
# Overwrite the existing variables in `data_cobre`.

# age_onset_psychiatric_illness: just copy code from slides
# age_first_hospitalization: first, check out what's wrong here
table(data_cobre$age_first_hospitalization) 

data_cobre <- data_cobre %>% # overwrite with "<-" operator  - now data_cobre contains the recoded features
  mutate(
    age_onset_psychiatric_illness = if_else(
      age_onset_psychiatric_illness == 9999,
      true = NA_real_,
      false = age_onset_psychiatric_illness
    ),
    age_first_hospitalization = if_else(
      age_first_hospitalization %in% c("9999", "0", "05"), # we replace "0", "05", "9999" with NA under the assumption that these are coding errors
      true = NA_real_,
      false = as.numeric(age_first_hospitalization) # we have to convert to numeric to make sure our final variable is numeric
    )
  )

# Create a new variable that gives the time since the onset of psychiatric illness in years
# Then compute the mean time since onset of psychiatric illness for men and women.
data_cobre %>%
  mutate(illness_duration = age - age_onset_psychiatric_illness) %>%
  group_by(gender) %>%
  summarize(mean = mean(illness_duration, na.rm = TRUE),
            .groups = "keep") # can also leave out ".groups" argument