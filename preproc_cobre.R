# code to preprocess data used in the tutorial

library(tidyverse)

# load data (available via https://coins.trendscenter.org/)
X1139_Demographics_20201209 <-
  read_csv("1139_Demographics_20201209.csv")


# preprocessing: select/rename variable, transform some vals to NA
X1139_Demographics_20201209 %>% filter(`Anonymized ID` != "ID") %>%
  transmute(
    ID = `Anonymized ID`,
    Study_group = `Subject Type`,
    Study = `Sub Study Label`,
    Age = `Current Age`,
    Gender,
    Age_onset_psychiatric_illness = `Age at first psychiatric illness`,
    Age_first_hospitalization = `Age at first psychiatric hospitalization`
  ) %>%
  mutate(
    Study_group = case_when(
      Study_group %in% c("Chronic Sz", "Early Sz") ~  "Schizophrenia",
      Study_group %in% c("Old Control", "Young Control") ~ "Control",
      TRUE ~ Study_group
    ),
    Age_first_hospitalization = case_when(
      Age_first_hospitalization %in% c("md",     "n/a" ,    "N/A", "unknown") ~ NA_character_,
      TRUE ~ Age_first_hospitalization
    ),
    Age_onset_psychiatric_illness = case_when(
      Age_onset_psychiatric_illness %in% c("md",     "n/a" ,    "N/A", "unknown") ~ NA_character_,
      Age_onset_psychiatric_illness == "05" ~
        "5",
      TRUE ~ Age_onset_psychiatric_illness
    ),
    Gender = as.factor(case_when(Gender == 1 ~ "male", TRUE ~ "female")),
    Age = as.numeric(Age)
  ) -> data_cobre

# write out data as .csv
write.table(
  data_cobre,
  "data_cobre.csv",
  row.names = F,
  col.names = T,
  sep = ","
)