## ----setup, message=FALSE, warning=FALSE---------------------------------
library(kableExtra)
library(knitr)
library(skimr)
library(tidyverse)
library(data.table)
library(sl3)
library(SuperLearner)
library(origami)

set.seed(7194)

# load data set and take a peek
washb_data <- fread("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv",
                    stringsAsFactors = TRUE)

head(washb_data) %>%
  kable(digits = 4) %>%
  kable_styling(fixed_thead = T, font_size = 10) %>%
  scroll_box(width = "100%", height = "250px")


## ----task----------------------------------------------------------------
# specify the outcome and covariates
outcome <- "whz"
covars <- colnames(washb_data)[-which(names(washb_data) == outcome)]

# create the sl3 task
washb_task <- make_sl3_Task(
  data = washb_data,
  covariates = covars,
  outcome = outcome
)

# examine the task
washb_task


## ----list-properties-----------------------------------------------------
sl3_list_properties()


## ----list-learners-------------------------------------------------------
sl3_list_learners(c("continuous"))


## ----baselearners--------------------------------------------------------
# choose base learners
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_glmnet <- make_learner(Lrnr_glmnet)


## ----extra-lrnr----------------------------------------------------------
lrnr_ranger100 <- make_learner(Lrnr_ranger, num.trees = 100)
lrnr_hal_simple <- make_learner(Lrnr_hal9001, degrees = 1, n_folds = 2)
lrnr_gam <- Lrnr_pkg_SuperLearner$new("SL.gam")
lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")


## ----stack---------------------------------------------------------------
stack <- make_learner(
  Stack,
  lrnr_glm, lrnr_mean, lrnr_ranger100, lrnr_glmnet,
  lrnr_gam, lrnr_bayesglm
)


## ----metalearner---------------------------------------------------------
metalearner <- make_learner(Lrnr_nnls)


## ----screener------------------------------------------------------------
screen_cor <- Lrnr_pkg_SuperLearner_screener$new("screen.corP")
# which covariates are selected on the full data?
screen_cor$train(washb_task)


## ----screener-pipe-------------------------------------------------------
cor_pipeline <- make_learner(Pipeline, screen_cor, stack)


## ----screeners-stack-----------------------------------------------------
fancy_stack <- make_learner(Stack, cor_pipeline, stack)
# we can visualize the stack
dt_stack <- delayed_learner_train(fancy_stack, washb_task)
plot(dt_stack, color = FALSE, height = "400px", width = "100%")


## ----make-sl-------------------------------------------------------------
sl <- make_learner(Lrnr_sl,
  learners = fancy_stack,
  metalearner = metalearner
)
# we can visualize the super learner
dt_sl <- delayed_learner_train(sl, washb_task)
plot(dt_sl, color = FALSE, height = "400px", width = "100%")


## ----sl-basic------------------------------------------------------------
sl_fit <- sl$train(washb_task)


## ----sl-basic-summary----------------------------------------------------
sl_preds <- sl_fit$predict()
head(sl_preds)
sl_fit$print()


## ----CVsl----------------------------------------------------------------
washb_task_new <- make_sl3_Task(
  data = washb_data,
  covariates = covars,
  outcome = outcome,
  folds = make_folds(washb_data, fold_fun = folds_vfold, V = 2)
)
CVsl <- CV_lrnr_sl(sl_fit, washb_task_new, loss_squared_error)
CVsl %>%
  kable(digits = 4) %>%
  kable_styling(fixed_thead = T, font_size = 10) %>%
  scroll_box(width = "100%", height = "250px")


## ----varimp--------------------------------------------------------------
washb_varimp <- varimp(sl_fit, loss_squared_error)
washb_varimp %>%
  kable(digits = 4) %>%
  kable_styling(fixed_thead = T, font_size = 10) %>%
  scroll_box(width = "100%", height = "250px")


## ----ex-setup, message=FALSE, warning=FALSE------------------------------
# load the data set
db_data <-
 url("https://raw.githubusercontent.com/benkeser/sllecture/master/chspred.csv")
chspred <- read_csv(file = db_data, col_names = TRUE)
# take a quick peek
head(chspred) %>%
  kable(digits = 4) %>%
  kable_styling(fixed_thead = T, font_size = 10) %>%
  scroll_box(width = "100%", height = "200px")

