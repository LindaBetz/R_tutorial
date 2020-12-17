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

# ---------------------------------- 0: load libraries  -----------------------------------
# check if required packages are installed, if not, install them
packages <- c("mlr", "tidyverse", "e1071")
install.packages(setdiff(packages, rownames(installed.packages())))  

library(mlr)
library(tidyverse)

# ---------------------------------- 1: load & preprocess data  -----------------------------------

# load data
data_cobre_neuropsych <- read_csv("data_cobre_neuropsych.csv")

# first, some basic preprocessing
data_cobre_neuropsych <- data_cobre_neuropsych %>%
  mutate(across(
    # we recode "8888" to NA
    .cols = -c("id", "study_group", "age", "gender"),
    ~ if_else(. %in% c(8888, 9999), NA_real_, .)
  )) %>%
  mutate(outcome = if_else(study_group == "Control", "Control", "Patient")) %>%
  # then, we convert our outcome to a factor
  mutate(outcome = as.factor(outcome),
         gender = as.factor(gender)) %>%
  select(-c(id, study_group)) # we don't need ID & Study_group anymore, so we drop it


# ---------------------------------- 2: check missingness in data -----------------------------------

# check missingness => we may need to impute data
data_cobre_neuropsych %>%
  summarize(across(everything(), ~ sum(is.na(.x)))) # returns number of missing values per variable
# in general, we do not have many missing values, but we still need to impute data


# ---------------------------------- 3: set up machine learning pipeline (nested resampling) -----------------------------------
# Nested resampling is computationally expensive
# Therefore, in the examples shown below we use a low number of optimization/resampling iterations

# --------- 3.1: make a learning task ---------
# Learning tasks encapsulate the data set and other relevant information about a machine learning problem
# for example, the name of the target variable for supervised problems

task_cobre_neuropsych <-
  makeClassifTask(
    "cobre_neuropsych_task",
    data = data_cobre_neuropsych,
    target = "outcome",
    positive = "Patient"
  )

# let's check out the ML task we've created
task_cobre_neuropsych

# 189 observations: 90 control, 99 patients => good news: our task is balanced

# --------- 3.2: construct a learner ---------
# a learner is the specific machine learning algorithm we want to use
# here, we use a linear SVM as implemented the package e1071

lrn <- makeLearner("classif.svm", kernel = "linear")

# In order to tune a machine learning algorithm, you have to specify:

# a) the search space
# b) the optimization algorithm (aka tuning method)
# c) an evaluation method, i.e., a resampling strategy and a performance measure


# a) search space: define hyperparameters of SVM + search space we are going to cover
ps <- makeParamSet(makeNumericParam(
  "cost",
  lower = -5,
  upper = 5,
  trafo = function(x)
    2 ^ x,
  default = 0.01
))

# b) the optimization algorithm (aka tuning method)
# we use random search for hyperparameter optimization (this is not ideal and may be substituted by more advanced methods)
ctrl <-
  makeTuneControlRandom(budget = 50) # budget should at least be 50, better around 250

# c) an evaluation method, i.e., a resampling strategy
# we define the set up of the inner resampling scheme: repeated CV
inner <-
  makeResampleDesc("RepCV",
                   folds = 5,
                   reps = 2,
                   stratify = TRUE)

# --------- 3.3: impute missing values ---------
# also within our inner resampling scheme, missing values will be imputed
# here: via median, could also be mean, kNN-imputation...
lrn <-
  makeImputeWrapper(lrn, classes = list(numeric = imputeMedian()))

# --------- 3.4: feature preprocessing ---------
# here, we include our preprocessing - 2 alternatives (run only one!):
# a) a z-transformation on every variable
lrn <-
  makePreprocWrapperCaret(lrn,
                          ppc.scale = TRUE, # ensures variables are scaled, is necessary for SVM
                          ppc.center = TRUE)

# b) PCA
lrn <-
  makePreprocWrapperCaret(
    lrn,
    ppc.pca = TRUE,
    ppc.thresh = 0.9,
    ppc.scale = TRUE,
    # ensures variables are scaled, is necessary for meaningful results in PCA
    ppc.center = TRUE
  )

# --------- 3.5: initialize inner resampling scheme ---------
# initialize inner resampling scheme: missing value imputation, feature preprocessing, hyperparameter tuning
lrn <- makeTuneWrapper(
  lrn,
  resampling = inner,
  par.set = ps,
  measures = list(bac, tpr, tnr),
  # we use BAC as our perfomance measure (see 3.5), and TPR/TNR are returned in addition
  control = ctrl,
  show.info = FALSE
)

# --------- 3.5: initialize outer resampling scheme ---------
# initialize outer resampling scheme: repeated CV
outer <-
  makeResampleDesc(
    "RepCV",
    folds = 5,
    reps = 2,
    predict = "both",
    # save results from both train/test folds
    stratify = TRUE
  )


set.seed(1)
neuropsych_cobre_resampling <- resample(
  lrn,
  task_cobre_neuropsych,
  resampling = outer,
  measures = list(bac, tpr, tnr),
  # generalization performance will be evaluated with BAC, and TPR/TNR are returned in addition
  extract = getTuneResult,
  show.info = TRUE,
  models = TRUE
)

# ---------------------------------- 4: plot resuls  -----------------------------------
# plot train vs. test bac for each iteration of resampling
neuropsych_cobre_resampling$measures.train %>% mutate(set = "train") %>%
  bind_rows(neuropsych_cobre_resampling$measures.test %>% mutate(set = "test")) %>%
  mutate(set = factor(set, levels = c("train", "test"))) %>%
  ggplot(., aes(
    x = iter,
    y = bac,
    group = set,
    fill = set
  )) +
  geom_bar(stat = "identity",
           width = 0.75,
           position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c("lightgrey", "steelblue")) +
  coord_cartesian(ylim = c(0.5, 0.9)) +
  scale_x_continuous(breaks = seq(1, 10)) +
  ylab("BAC \n") +
  xlab("\n Resampling Iteration") +
  guides(fill = guide_legend(title = "Set")) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 14),
    legend.title = element_text(size = 14),
    axis.text = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 12, color = "black")
  )

# ---------------------------------- 5: feature importance  -----------------------------------
# --------- 5.1: retrain on whole dataset ---------
set.seed(1)
res <- tuneParams(
  lrn,
  task = task_cobre_neuropsych,
  resampling = inner,
  par.set = ps,
  control = ctrl,
  measures = list(bac, tpr, tnr)
)

# --------- 5.2: extract best hyperparameter value ---------
lrn <-
  setHyperPars(lrn,
               cost = res$x$cost)

# --------- 5.3: compute feature importance
# this takes a while, ~ 20-40 min, depending on your computer
feat_imp <- generateFeatureImportanceData(
  task_cobre_neuropsych,
  method = "permutation.importance",
  lrn,
  nmc = 1000,
  features = getTaskFeatureNames(task_cobre_neuropsych),
  interaction = F,
  measure = list(bac),
  local = F
)