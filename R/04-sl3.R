## ----cv_fig, fig.show="hold", echo = FALSE------------------------------------
knitr::include_graphics("img/misc/vs.pdf")


## ----cv_fig2, echo = FALSE----------------------------------------------------
knitr::include_graphics("img/misc/SLKaiserNew.pdf")


## ----cv_fig3, echo = FALSE----------------------------------------------------
knitr::include_graphics("img/misc/ericSL.pdf")


## ----setup, message=FALSE, warning=FALSE--------------------------------------
library(here)
library(data.table)
library(knitr)
library(kableExtra)
library(tidyverse)
library(origami)
library(SuperLearner)
library(sl3)

set.seed(7194) 
# my lucky seed! or is it 9174? or 4917? many lucky seeds, thanks lysdexia!

# load data set and take a peek
washb_data <- fread("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv",
                    stringsAsFactors = TRUE)
head(washb_data) %>%
  kable(digits = 4) %>%
  kableExtra:::kable_styling(fixed_thead = T) %>%
  scroll_box(width = "100%", height = "300px")


## ----task---------------------------------------------------------------------
# specify the outcome and covariates
outcome <- "whz"
covars <- colnames(washb_data)[-which(names(washb_data) == outcome)]

# create the sl3 task
washb_task <- make_sl3_Task(
  data = washb_data,
  covariates = covars,
  outcome = outcome
)


## ----task-examine-------------------------------------------------------------
washb_task


## ----list-properties----------------------------------------------------------
sl3_list_properties()


## ----list-learners------------------------------------------------------------
sl3_list_learners("continuous")


## ----baselearners-------------------------------------------------------------
# choose base learners
lrnr_glm <- make_learner(Lrnr_glm)
lrnr_mean <- make_learner(Lrnr_mean)


## ----extra-lrnr-awesome, message=FALSE, warning=FALSE-------------------------
lrnr_ranger50 <- make_learner(Lrnr_ranger, num.trees = 50)
lrnr_hal_simple <- make_learner(Lrnr_hal9001, max_degree = 2, n_folds = 2)
lrnr_lasso <- make_learner(Lrnr_glmnet) # alpha default is 1
lrnr_ridge <- make_learner(Lrnr_glmnet, alpha = 0)
lrnr_elasticnet <- make_learner(Lrnr_glmnet, alpha = .5)


## ----extra-lrnr-woah, message=FALSE, warning=FALSE----------------------------
lrnr_bayesglm <- Lrnr_pkg_SuperLearner$new("SL.bayesglm")


## ----extra-lrnr-svm, eval = FALSE---------------------------------------------
## # I like to crock pot my super learners
## grid_params <- list(cost = c(0.01, 0.1, 1, 10, 100, 1000),
##                     gamma = c(0.001, 0.01, 0.1, 1),
##                     kernel = c("polynomial", "radial", "sigmoid"),
##                     degree = c(1, 2, 3))
## grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
## params_default <- list(nthread = getOption("sl.cores.learners", 1))
## svm_learners <- apply(grid, MARGIN = 1, function(params_tune) {
##   do.call(Lrnr_svm$new, c(params_default, as.list(params_tune)))})

## ----extra-lrnr-xgboost-------------------------------------------------------
grid_params <- list(max_depth = c(2, 4, 6, 8),
                    eta = c(0.001, 0.01, 0.1, 0.2, 0.3),
                    nrounds = c(20, 50))
grid <- expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
params_default <- list(nthread = getOption("sl.cores.learners", 1))
xgb_learners <- apply(grid, MARGIN = 1, function(params_tune) {
  do.call(Lrnr_xgboost$new, c(params_default, as.list(params_tune)))})


## ----carotene, eval = FALSE---------------------------------------------------
## # I have no idea how to tune a neural net (or BART machine..)
## lrnr_caret_nnet <- make_learner(Lrnr_caret, algorithm = "nnet")
## lrnr_caret_bartMachine <- make_learner(Lrnr_caret, algorithm = "bartMachine",
##                                        method = "boot", metric = "RMSE",
##                                        tuneLength = 10)


## ----stack--------------------------------------------------------------------
stack <- make_learner(
  Stack,
  lrnr_glm, lrnr_mean, lrnr_ridge, lrnr_lasso, xgb_learners[[10]]
)


## ----screener-----------------------------------------------------------------
screen_rf <- make_learner(Lrnr_screener_randomForest, nVar = 5, ntree = 20)
# which covariates are selected on the full data?
screen_rf$train(washb_task)


## ----screener-pipe------------------------------------------------------------
screen_rf_pipeline <- make_learner(Pipeline, screen_rf, stack)


## ----screeners-stack, message=FALSE, warning=FALSE----------------------------
fancy_stack <- make_learner(Stack, screen_rf_pipeline, stack)
# we can visualize the stack
dt_stack <- delayed_learner_train(fancy_stack, washb_task)
plot(dt_stack, color = FALSE, height = "400px", width = "100%")


## ----make-sl, message=FALSE, warning=FALSE------------------------------------
sl <- make_learner(Lrnr_sl,
  learners = fancy_stack
)


## ----make-sl-plot, message=FALSE, warning=FALSE-------------------------------
dt_sl <- delayed_learner_train(sl, washb_task)
plot(dt_sl, color = FALSE, height = "400px", width = "100%")


## ----sl-----------------------------------------------------------------------
sl_fit <- sl$train(washb_task)


## ----sl-predictions-----------------------------------------------------------
# we did it! now we have super learner predictions
sl_preds <- sl_fit$predict()
head(sl_preds)


## ---- plot-predvobs-woohoo----------------------------------------------------

df_plot <- data.frame(Observed = washb_data$whz, 
                      Predicted = sl_preds,
                      count = c(1:nrow(washb_data)))
                      
df_plot_melted <- melt(df_plot, 
                       id.vars = "count", 
                       measure.vars = c("Observed", "Predicted"))

ggplot(df_plot_melted, aes(value, count, color = variable)) + geom_point() 


## ---- sl-summary--------------------------------------------------------------
sl_fit$print()


## ----CVsl---------------------------------------------------------------------
washb_task_new <- make_sl3_Task(
  data = washb_data,
  covariates = covars,
  outcome = outcome,
  folds = make_folds(washb_data, fold_fun = folds_vfold, V = 2)
)
CVsl <- CV_lrnr_sl(sl_fit, washb_task_new, loss_squared_error)
CVsl %>%
  kable(digits = 4) %>%
  kableExtra:::kable_styling(fixed_thead = T) %>%
  scroll_box(width = "100%", height = "300px")


## ----varimp-------------------------------------------------------------------
washb_varimp <- varimp(sl_fit, loss_squared_error)
washb_varimp %>%
  kable(digits = 4) %>%
  kableExtra:::kable_styling(fixed_thead = T) %>%
  scroll_box(width = "100%", height = "300px")


## ----ex-setup, warning=FALSE, message=FALSE-----------------------------------
# load the data set
db_data <-
 url("https://raw.githubusercontent.com/benkeser/sllecture/master/chspred.csv")
chspred <- read_csv(file = db_data, col_names = TRUE)
# take a quick peek
head(chspred) %>%
  kable(digits = 4) %>%
  kableExtra:::kable_styling(fixed_thead = T) %>%
  scroll_box(width = "100%", height = "300px")


## ----ex-setup2, warning=FALSE, message=FALSE----------------------------------
ist_data <- data.table(read.csv("https://raw.githubusercontent.com/tlverse/tlverse-handbook/master/data/ist_sample.csv"))

# number 3 help
ist_task_CVsl <- make_sl3_Task(
  data = ist_data,
  outcome = "DRSISC",
  covariates = colnames(ist_data)[-which(names(ist_data) == "DRSISC")],
  drop_missing_outcome = TRUE,
  folds = make_folds(
    n = sum(!is.na(ist_data$DRSISC)), 
    fold_fun = folds_vfold,
    V = 5
    )
  )


## ----ex-key, eval=FALSE, message=FALSE, warning=FALSE-------------------------
## # make task
## chspred_task <- make_sl3_Task(
##   data = chspred,
##   covariates = head(colnames(chspred), -1),
##   outcome = "mi"
## )
## 
## # make learners
## glm_learner <- Lrnr_glm$new()
## lasso_learner <- Lrnr_glmnet$new(alpha = 1)
## ridge_learner <- Lrnr_glmnet$new(alpha = 0)
## enet_learner <- Lrnr_glmnet$new(alpha = 0.5)
## curated_glm_learner <- Lrnr_glm_fast$new(formula = "mi ~ smoke + beta + waist")
## mean_learner <- Lrnr_mean$new() # That is one mean learner!
## glm_fast_learner <- Lrnr_glm_fast$new()
## ranger_learner <- Lrnr_ranger$new()
## svm_learner <- Lrnr_svm$new()
## xgb_learner <- Lrnr_xgboost$new()
## screen_cor <- make_learner(Lrnr_screener_corP)
## glm_pipeline <- make_learner(Pipeline, screen_cor, glm_learner)
## 
## # stack learners together
## stack <- make_learner(
##   Stack,
##   glm_pipeline, glm_learner,
##   lasso_learner, ridge_learner, enet_learner,
##   curated_glm_learner, mean_learner, glm_fast_learner,
##   ranger_learner, svm_learner, xgb_learner
## )
## 
## # choose metalearner
## metalearner <- make_learner(Lrnr_nnls)
## 
## sl <- Lrnr_sl$new(
##   learners = stack,
##   metalearner = metalearner
## )
## sl_fit <- sl$train(chspred_task)
## sl_fit$print()
## 
## CVsl <- CV_lrnr_sl(sl_fit, chspred_task, loss_squared_error)
## CVsl

