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
  read_csv("raw_data/COBRE_demographics.csv")


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
  mutate(
    study_group = as.factor(
      case_when(
        study_group %in% c("Chronic Sz") ~  "chronic_schizophrenia",
        study_group %in% c("Early Sz") ~  "early_schizophrenia",
        study_group %in% c("Old Control", "Young Control") ~ "control",
        TRUE ~ tolower(study_group))
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
  select(-study) %>%
  filter(id %in% c("A00000300",
                   "A00000368",
                   "A00000456",
                   "A00000541",
                   "A00000541",
                   "A00000838",
                   "A00000838",
                   "A00000909",
                   "A00001181",
                   "A00001181",
                   "A00001243",
                   "A00001251",
                   "A00001452",
                   "A00002198",
                   "A00002480",
                   "A00003150",
                   "A00004087",
                   "A00004087",
                   "A00006754",
                   "A00007409",
                   "A00009280",
                   "A00010150",
                   "A00010684",
                   "A00011265",
                   "A00011725",
                   "A00012767",
                   "A00012767",
                   "A00012995",
                   "A00013140",
                   "A00013216",
                   "A00013363",
                   "A00013816",
                   "A00014120",
                   "A00014225",
                   "A00014522",
                   "A00014590",
                   "A00014607",
                   "A00014607",
                   "A00014636",
                   "A00014719",
                   "A00014804",
                   "A00014804",
                   "A00014830",
                   "A00014839",
                   "A00014898",
                   "A00015518",
                   "A00015518",
                   "A00015648",
                   "A00015826",
                   "A00016197",
                   "A00016720",
                   "A00016720",
                   "A00016723",
                   "A00016723",
                   "A00017147",
                   "A00017294",
                   "A00018129",
                   "A00018317",
                   "A00018317",
                   "A00018403",
                   "A00018434",
                   "A00018434",
                   "A00018716",
                   "A00018979",
                   "A00018979",
                   "A00019293",
                   "A00019293",
                   "A00019349",
                   "A00019349",
                   "A00019888",
                   "A00020414",
                   "A00020602",
                   "A00020787",
                   "A00020805",
                   "A00020805",
                   "A00020895",
                   "A00020968",
                   "A00020984",
                   "A00020984",
                   "A00021058",
                   "A00021072",
                   "A00021081",
                   "A00021085",
                   "A00021591",
                   "A00021598",
                   "A00022400",
                   "A00022490",
                   "A00022500",
                   "A00022500",
                   "A00022509",
                   "A00022592",
                   "A00022619",
                   "A00022653",
                   "A00022687",
                   "A00022727",
                   "A00022729",
                   "A00022729",
                   "A00022773",
                   "A00022773",
                   "A00022810",
                   "A00022835",
                   "A00022837",
                   "A00022915",
                   "A00022915",
                   "A00023095",
                   "A00023120",
                   "A00023120",
                   "A00023131",
                   "A00023143",
                   "A00023158",
                   "A00023243",
                   "A00023246",
                   "A00023246",
                   "A00023330",
                   "A00023337",
                   "A00023337",
                   "A00023590",
                   "A00023750",
                   "A00023750",
                   "A00023800",
                   "A00023800",
                   "A00023848",
                   "A00023866",
                   "A00024160",
                   "A00024160",
                   "A00024198",
                   "A00024198",
                   "A00024228",
                   "A00024301",
                   "A00024446",
                   "A00024535",
                   "A00024546",
                   "A00024568",
                   "A00024663",
                   "A00024684",
                   "A00024820",
                   "A00024932",
                   "A00024953",
                   "A00024953",
                   "A00024955",
                   "A00024955",
                   "A00024959",
                   "A00025969",
                   "A00026907",
                   "A00026907",
                   "A00026945",
                   "A00027391",
                   "A00027537",
                   "A00027787",
                   "A00028052",
                   "A00028404",
                   "A00035485",
                   "A00035485",
                   "A00035751",
                   "A00036049",
                   "A00036555",
                   "A00036844",
                   "A00036897",
                   "A00036916",
                   "A00037007",
                   "A00037034",
                   "A00037224",
                   "A00037238",
                   "A00037318",
                   "A00037495",
                   "A00037564",
                   "A00037619",
                   "A00037649",
                   "A00037665",
                   "A00037854",
                   "A00038441",
                   "A00038624")
  )


# ---------------------------------- 2: export data  -----------------------------------
# write out data as .csv
write.table(
  demographics_cobre,
  "data_cobre.csv",
  row.names = F,
  col.names = T,
  sep = ","
)

