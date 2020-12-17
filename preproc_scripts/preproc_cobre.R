# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     Code for    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                                                                                     #
#                                                                                                     #
#                            Multivariate and Neuroimaging Methods in Psychiatry                      #
#                                                                                                     #
#                                    R software hands on session                                      #
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# code to preprocess data used in the tutorial
# ---------------------------------- 0: load libraries  -----------------------------------
library(tidyverse)

# ---------------------------------- 1: load & preprocess data  -----------------------------------

# load data (available via https://coins.trendscenter.org/)
demographics_cobre_raw <-
  read_csv("raw_data/1139_Demographics_20201215.csv")
neuropsych_cobre_raw <-
  read_csv("raw_data/1139_Cobre_Neuropsych_V2_20201215.csv")

neuropsych_cobre <- neuropsych_cobre_raw %>%
  .[2:nrow(.), ] %>% # first row has duplicated column headings
  transmute(id = `Anonymized ID`, select(
    .,
    matches("^id|RawScore|Value|HVLT_Trials_Sum|BVMT_Trials_Sum")
  )) 


# ensure that variable names comply with R's conventions
names(neuropsych_cobre) <-
  gsub("\\V.5_", "", names(neuropsych_cobre))
names(neuropsych_cobre) <-
  gsub("\\-", "_", names(neuropsych_cobre))

# preprocessing: select/rename variable, transform some vals to NA
# I've left some problematic coding unchanged on purpose (this will be an exercise later on)
demographics_cobre <- demographics_cobre_raw %>%
  .[2:nrow(.), ] %>% # first row has duplicated column headings
  transmute(
    id = `Anonymized ID`,
    study_group = `Subject Type`,
    study = `Sub Study Label`,
    age = `Current Age`,
    gender = Gender,
    age_onset_psychiatric_illness = `Age at first psychiatric illness`,
    age_first_hospitalization = `Age at first psychiatric hospitalization`,
    first_degree_relative_psychosis = `First degree relative with psychosis`
  ) %>%
  filter(study == "COBRE") %>%
  mutate(
    study_group = as.factor(
      case_when(
        study_group %in% c("Chronic Sz", "Early Sz") ~  "Schizophrenia",
        study_group %in% c("Old Control", "Young Control") ~ "Control",
        TRUE ~ study_group
      )
    ),
    age_first_hospitalization = case_when(
      age_first_hospitalization %in% c("md",     "n/a" ,    "N/A", "unknown") ~ NA_character_,
      TRUE ~ age_first_hospitalization
    ),
    first_degree_relative_psychosis = case_when(
      first_degree_relative_psychosis == "0" ~ FALSE,
      first_degree_relative_psychosis == "1" ~ TRUE,
      TRUE ~ NA
    ),
    age_onset_psychiatric_illness = case_when(
      age_onset_psychiatric_illness %in% c("md",     "n/a" ,    "N/A", "unknown") ~ NA_character_,
      age_onset_psychiatric_illness == "05" ~
        "5",
      TRUE ~ age_onset_psychiatric_illness
    ),
    gender = as.factor(case_when(gender == 1 ~ "male", TRUE ~ "female")),
    age = as.numeric(age)
  ) %>% distinct(id, .keep_all = TRUE) %>% # there are some duplicated IDs
  filter(id %in% neuropsych_cobre$id) %>%
  select(-study)


neuropsych_cobre <-
  neuropsych_cobre %>% bind_cols(demographics_cobre %>% select(study_group, age, gender)) %>%
  select(id, study_group, age, gender, everything())

# ---------------------------------- 2: export data  -----------------------------------
# write out data as .csv
write.table(
  demographics_cobre,
  "data_cobre.csv",
  row.names = F,
  col.names = T,
  sep = ","
)

write.table(
  neuropsych_cobre,
  "data_cobre_neuropsych.csv",
  row.names = F,
  col.names = T,
  sep = ","
)
