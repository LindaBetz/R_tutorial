# code to preprocess data used in the tutorial

library(tidyverse)

# load data (available via https://coins.trendscenter.org/)
demographics_cobre_raw <-
  read_csv("1139_Demographics_20201215.csv")
neuropsych_cobre_raw <- read_csv("1139_Cobre_Neuropsych_V2_20201215.csv")


# preprocessing: select/rename variable, transform some vals to NA
# I've left some problematic coding unchanged on purpose (this will be an exercise later on)
neuropsych_cobre_raw %>%
  .[2:nrow(.),] %>% # first row has duplicated column headings
  transmute(ID = `Anonymized ID`, select(., matches("^ID|RawScore|_Value|Trials_Sum"))) -> neuropsych_cobre

# ensure that variable names comply with R's conventions
names(neuropsych_cobre) <- gsub("\\V.5_", "", names(neuropsych_cobre))
names(neuropsych_cobre) <- gsub("\\-", "_", names(neuropsych_cobre))


demographics_cobre_raw %>% filter(`Anonymized ID` != "ID") %>%
  .[2:nrow(.),] %>% # first row has duplicated column headings
  transmute(
    ID = `Anonymized ID`,
    Study_group = `Subject Type`,
    Study = `Sub Study Label`,
    Age = `Current Age`,
    Gender,
    Age_onset_psychiatric_illness = `Age at first psychiatric illness`,
    Age_first_hospitalization = `Age at first psychiatric hospitalization`,
    First_degree_relative_psychosis = `First degree relative with psychosis`
  ) %>%
  filter(Study == "COBRE") %>%
  mutate(
    Study_group = as.factor(case_when(
      Study_group %in% c("Chronic Sz", "Early Sz") ~  "Schizophrenia",
      Study_group %in% c("Old Control", "Young Control") ~ "Control",
      TRUE ~ Study_group
    )),
    Age_first_hospitalization = case_when(
      Age_first_hospitalization %in% c("md",     "n/a" ,    "N/A", "unknown") ~ NA_character_,
      TRUE ~ Age_first_hospitalization
    ),
    First_degree_relative_psychosis = case_when(
      First_degree_relative_psychosis == "0" ~ FALSE,
      First_degree_relative_psychosis == "1" ~ TRUE,
      TRUE ~ NA
    ),
    Age_onset_psychiatric_illness = case_when(
      Age_onset_psychiatric_illness %in% c("md",     "n/a" ,    "N/A", "unknown") ~ NA_character_,
      Age_onset_psychiatric_illness == "05" ~
        "5",
      TRUE ~ Age_onset_psychiatric_illness
    ),
    Gender = as.factor(case_when(Gender == 1 ~ "male", TRUE ~ "female")),
    Age = as.numeric(Age)
  ) %>% distinct(ID, .keep_all = TRUE) %>% # there are some duplicated IDs
  filter(ID %in% neuropsych_cobre$ID) %>%
  select(-Study) -> demographics_cobre


neuropsych_cobre %>% bind_cols(demographics_cobre %>% select(Study_group, Age, Gender)) %>%
  select(ID, Study_group, Age, Gender, everything()) -> neuropsych_cobre


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
  "data_cobre_ML.csv",
  row.names = F,
  col.names = T,
  sep = ","
)

