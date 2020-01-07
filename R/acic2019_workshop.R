## ----set-options, include=FALSE------------------------------------------
# Set output options
if (knitr:::is_html_output()) {
  options(width = 80)
}
if (knitr:::is_latex_output()) {
  options(width = 65)
}
options(digits = 7, bookdown.clean_book = TRUE, knitr.kable.NA = "NA")
knitr::opts_chunk$set(
  tidy = FALSE,
  out.width = "\textwidth",
  fig.align = "center",
  comment = NA
)


## ----pkg-bib, include=FALSE----------------------------------------------
# automatically create a bib database for R packages
knitr::write_bib(c(
  .packages(), "bookdown", "knitr", "rmarkdown"
), "packages.bib")


## ----installation, eval=FALSE--------------------------------------------
## install.packages("devtools")
## devtools::install_github("tlverse/tlverse")


## ---- echo=F, eval=T-----------------------------------------------------
library(visNetwork)
nodes <- data.frame(id=c("W","A","Y"))
nodes$label <- nodes$id
edges <- data.frame(from=c("W","W","A"),to=c("A","Y","Y"))
network <- visNetwork(nodes,edges, height="300px",width="200px") %>%
  visEdges(arrows=list(to=TRUE))  %>%
  visLayout(randomSeed=25)
network


## ----nature_slides, fig.show="hold"--------------------------------------
knitr::include_graphics("img/misc/NatureSlides.pdf")


## ----cv_fig, fig.show="hold"---------------------------------------------
knitr::include_graphics("img/misc/vs.pdf")


## ----cv_fig2, fig.show="hold"--------------------------------------------
knitr::include_graphics("img/misc/SLKaiserNew.pdf")


## ----cv_fig3, fig.show="hold"--------------------------------------------
knitr::include_graphics("img/misc/ericSL.pdf")


## ----cv_fig4, fig.show="hold"--------------------------------------------
knitr::include_graphics("img/misc/TMLEimage.pdf")


## ----load_washb_data_intro, message=FALSE, warning=FALSE-----------------
library(tidyverse)

# read in data
dat <- read_csv("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv")
dat


## ----skim_washb_data, message=FALSE, warning=FALSE-----------------------
library(skimr)
skim(dat)


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
washb_data <- fread("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv", stringsAsFactors = TRUE)

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


## ----ex-setup------------------------------------------------------------
# load the data set
db_data <-
 url("https://raw.githubusercontent.com/benkeser/sllecture/master/chspred.csv")
chspred <- read_csv(file = db_data, col_names = TRUE)
# take a quick peek
head(chspred) %>%
  kable(digits = 4) %>%
  kable_styling(fixed_thead = T, font_size = 10) %>%
  scroll_box(width = "100%", height = "250px")


## ----tmle3-load-data-----------------------------------------------------
library(data.table)
library(tmle3)
library(sl3)
washb_data <- fread("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv", stringsAsFactors = TRUE)


## ----tmle3-node-list-----------------------------------------------------
node_list <- list(
  W = c(
    "month", "aged", "sex", "momage", "momedu",
    "momheight", "hfiacat", "Nlt18", "Ncomp", "watmin",
    "elec", "floor", "walls", "roof", "asset_wardrobe",
    "asset_table", "asset_chair", "asset_khat",
    "asset_chouki", "asset_tv", "asset_refrig",
    "asset_bike", "asset_moto", "asset_sewmach",
    "asset_mobile"
  ),
  A = "tr",
  Y = "whz"
)


## ----tmle3-process_missing-----------------------------------------------
processed <- process_missing(washb_data, node_list)
washb_data <- processed$data
node_list <- processed$node_list


## ----tmle3-ate-spec------------------------------------------------------
ate_spec <- tmle_ATE(
  treatment_level = "Nutrition + WSH",
  control_level = "Control"
)


## ----tmle3-learner-list--------------------------------------------------
# choose base learners
lrnr_mean <- make_learner(Lrnr_mean)
lrnr_xgboost <- make_learner(Lrnr_xgboost)

# define metalearners appropriate to data types
ls_metalearner <- make_learner(Lrnr_nnls)
mn_metalearner <- make_learner(
  Lrnr_solnp, metalearner_linear_multinomial,
  loss_loglik_multinomial
)
sl_Y <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_xgboost),
  metalearner = ls_metalearner
)
sl_A <- Lrnr_sl$new(
  learners = list(lrnr_mean, lrnr_xgboost),
  metalearner = mn_metalearner
)

learner_list <- list(A = sl_A, Y = sl_Y)


## ----tmle3-spec-fit------------------------------------------------------
tmle_fit <- tmle3(ate_spec, washb_data, node_list, learner_list)


## ----tmle3-spec-summary--------------------------------------------------
print(tmle_fit)

estimates <- tmle_fit$summary$psi_transformed
print(estimates)


## ----tmle3-spec-task-----------------------------------------------------
tmle_task <- ate_spec$make_tmle_task(washb_data, node_list)


## ----tmle3-spec-task-npsem-----------------------------------------------
tmle_task$npsem


## ----tmle3-spec-initial-likelihood---------------------------------------
initial_likelihood <- ate_spec$make_initial_likelihood(
  tmle_task,
  learner_list
)
print(initial_likelihood)


## ----tmle3-spec-initial-likelihood-estimates-----------------------------
initial_likelihood$get_likelihoods(tmle_task)


## ----tmle3-spec-targeted-likelihood--------------------------------------
targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood)


## ----tmle3-spec-targeted-likelihood-no-cv--------------------------------
targeted_likelihood_no_cv <-
  Targeted_Likelihood$new(initial_likelihood,
    updater = list(cvtmle = FALSE)
  )


## ----tmle3-spec-params---------------------------------------------------
tmle_params <- ate_spec$make_params(tmle_task, targeted_likelihood)
print(tmle_params)


## ----tmle3-manual-fit----------------------------------------------------
tmle_fit_manual <- fit_tmle3(
  tmle_task, targeted_likelihood, tmle_params,
  targeted_likelihood$updater
)
print(tmle_fit_manual)


## ----tmle3-tsm-all-------------------------------------------------------
tsm_spec <- tmle_TSM_all()
targeted_likelihood <- Targeted_Likelihood$new(initial_likelihood)
all_tsm_params <- tsm_spec$make_params(tmle_task, targeted_likelihood)
print(all_tsm_params)


## ----tmle3-delta-method-param--------------------------------------------
ate_param <- define_param(
  Param_delta, targeted_likelihood,
  delta_param_ATE,
  list(all_tsm_params[[1]], all_tsm_params[[4]])
)
print(ate_param)


## ----tmle3-tsm-plus-delta------------------------------------------------
all_params <- c(all_tsm_params, ate_param)

tmle_fit_multiparam <- fit_tmle3(
  tmle_task, targeted_likelihood, all_params,
  targeted_likelihood$updater
)

print(tmle_fit_multiparam)


## ----data, message=FALSE, warning=FALSE----------------------------------
# load the data set
data(cpp)
cpp <- cpp[!is.na(cpp[, "haz"]), ]
cpp$parity01 <- as.numeric(cpp$parity > 0)
cpp[is.na(cpp)] <- 0
cpp$haz01 <- as.numeric(cpp$haz > 0)


## ---- fig.cap="Illustration of a Dynamic Treatment Regime in a Clinical Setting", echo=FALSE, eval=TRUE, out.width='60%'----
knitr::include_graphics(path = "img/image/DynamicA_Illustration.png")


## ----setup-mopttx, message=FALSE, warning=FALSE--------------------------
library(here)
library(data.table)
library(sl3)
library(tmle3)
library(tmle3mopttx)
library(devtools)
set.seed(111)


## ----load sim_bin_data---------------------------------------------------
data("data_bin")


## ----data_nodes2-mopttx--------------------------------------------------
# organize data and nodes for tmle3
data <- data_bin
node_list <- list(
  W = c("W1", "W2", "W3"),
  A = "A",
  Y = "Y"
)


## ----mopttx_sl3_lrnrs2---------------------------------------------------
# Define sl3 library and metalearners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner:
Q_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_50, lrn_xgboost_100,
                  lrn_xgboost_500,lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

## Define the g learner:
g_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_100, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

## Define the B learner:
b_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_50, lrn_xgboost_100,
                  lrn_xgboost_500,lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)


## ----mopttx_make_lrnr_list-----------------------------------------------
# specify outcome and treatment regressions and create learner list
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)


## ----mopttx_spec_init_complex--------------------------------------------
# initialize a tmle specification
tmle_spec <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3"), type = "blip1",
  learners = learner_list,
  maximize = TRUE, complex = TRUE,
  realistic = FALSE
)


## ----mopttx_fit_tmle_auto_blip_revere_complex, eval=T--------------------
# fit the TML estimator
fit <- tmle3(tmle_spec, data, node_list, learner_list)
fit


## ----load sim_cat_data---------------------------------------------------
data("data_cat_realistic")


## ----data_nodes-mopttx---------------------------------------------------
# organize data and nodes for tmle3
data <- data_cat_realistic
node_list <- list(
  W = c("W1", "W2", "W3", "W4"),
  A = "A",
  Y = "Y"
)


## ----sl3_lrnrs-mopttx----------------------------------------------------
# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new()
lrn_glm <- Lrnr_glm_fast$new()

## Define the Q learner, which is just a regular learner:
Q_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)

# Define the g learner, which is a multinomial learner:
#specify the appropriate loss of the multinomial learner:
mn_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_loglik_multinomial,
                               learner_function = metalearner_linear_multinomial)
g_learner <- make_learner(Lrnr_sl, list(lrn_xgboost_100,lrn_xgboost_500,lrn_mean), mn_metalearner)

# Define the Blip learner, which is a multivariate learner:
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner <- create_mv_learners(learners = learners)


## ----cat_learners--------------------------------------------------------
# See which learners support multi-class classification:
sl3_list_learners(c("categorical"))


## ----make_lrnr_list-mopttx-----------------------------------------------
# specify outcome and treatment regressions and create learner list
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)


## ----spec_init-----------------------------------------------------------
# initialize a tmle specification
tmle_spec <- tmle3_mopttx_blip_revere(
  V = c("W1", "W2", "W3", "W4"), type = "blip2",
  learners = learner_list, maximize = TRUE, complex = TRUE,
  realistic = FALSE
)


## ----fit_tmle_auto, eval=T-----------------------------------------------
# fit the TML estimator
fit <- tmle3(tmle_spec, data, node_list, learner_list)
fit

# How many individuals got assigned each treatment?
table(tmle_spec$return_rule)


## ----mopttx_spec_init_noncomplex-----------------------------------------
# initialize a tmle specification
tmle_spec <- tmle3_mopttx_blip_revere(
  V = c("W4", "W3", "W2", "W1"), type = "blip2",
  learners = learner_list,
  maximize = TRUE, complex = FALSE, realistic = FALSE
)


## ----mopttx_fit_tmle_auto_blip_revere_noncomplex, eval=T-----------------
# fit the TML estimator
fit <- tmle3(tmle_spec, data, node_list, learner_list)
fit


## ----mopttx_spec_init_realistic------------------------------------------
# initialize a tmle specification
tmle_spec <- tmle3_mopttx_blip_revere(
  V = c("W4", "W3", "W2", "W1"), type = "blip2",
  learners = learner_list,
  maximize = TRUE, complex = TRUE, realistic = TRUE
)


## ----mopttx_fit_tmle_auto_blip_revere_realistic, eval=T------------------
# fit the TML estimator
fit <- tmle3(tmle_spec, data, node_list, learner_list)
fit

# How many individuals got assigned each treatment?
table(tmle_spec$return_rule)


## ----data_vim-nodes-mopttx-----------------------------------------------
# bin baseline covariates to 3 categories:
data$W1<-ifelse(data$W1<quantile(data$W1)[2],1,ifelse(data$W1<quantile(data$W1)[3],2,3))

node_list <- list(
  W = c("W3", "W4", "W2"),
  A = c("W1", "A"),
  Y = "Y"
)


## ----mopttx_spec_init_vim------------------------------------------------
# initialize a tmle specification
tmle_spec <- tmle3_mopttx_vim(
  V=c("W2"),
  type = "blip2",
  learners = learner_list,
  contrast = "multiplicative",
  maximize = FALSE,
  method = "SL",
  complex = TRUE,
  realistic = FALSE
)


## ----mopttx_fit_tmle_auto_vim, eval=TRUE---------------------------------
# fit the TML estimator
vim_results <- tmle3_vim(tmle_spec, data, node_list, learner_list,
  adjust_for_other_A = TRUE
)

print(vim_results)


## ----load-washb-data, message=FALSE, warning=FALSE, cache=FALSE, eval=FALSE----
## washb_data <- fread("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv", stringsAsFactors = TRUE)
## washb_data <- washb_data[!is.na(momage), lapply(.SD, as.numeric)]
## head(washb_data, 3)


## ----washb-data-npsem-mopttx, message=FALSE, warning=FALSE, cache=FALSE, eval=FALSE----
## node_list <- list(W = names(washb_data)[!(names(washb_data) %in% c("whz", "tr"))],
##                   A = "tr", Y = "whz")


## ----summary_WASH, eval=FALSE--------------------------------------------
## #V1, V2 and V3:
## table(washb_data$momedu)
## table(washb_data$floor)
## table(washb_data$asset_refrig)
## 
## #A:
## table(washb_data$tr)
## 
## #Y:
## summary(washb_data$whz)


## ----sl3_lrnrs-WASH-mopttx, eval=FALSE-----------------------------------
## # Initialize few of the learners:
## grid_params = list(nrounds = c(100, 500),
##                      eta = c(0.01, 0.1))
## grid = expand.grid(grid_params, KEEP.OUT.ATTRS = FALSE)
## xgb_learners = apply(grid, MARGIN = 1, function(params_tune) {
##     do.call(Lrnr_xgboost$new, c(as.list(params_tune)))
##   })
## lrn_mean <- Lrnr_mean$new()
## 
## ## Define the Q learner, which is just a regular learner:
## Q_learner <- Lrnr_sl$new(
##   learners = list(xgb_learners[[1]], xgb_learners[[2]], xgb_learners[[3]],
##                   xgb_learners[[4]], lrn_mean),
##   metalearner = Lrnr_nnls$new()
## )
## 
## # Define the g learner, which is a multinomial learner:
## #specify the appropriate loss of the multinomial learner:
## mn_metalearner <- make_learner(Lrnr_solnp, loss_function = loss_loglik_multinomial,
##                                learner_function = metalearner_linear_multinomial)
## g_learner <- make_learner(Lrnr_sl, list(xgb_learners[[4]], lrn_mean), mn_metalearner)
## 
## # Define the Blip learner, which is a multivariate learner:
## learners <- list(xgb_learners[[1]], xgb_learners[[2]], xgb_learners[[3]],
##                   xgb_learners[[4]], lrn_mean)
## b_learner <- create_mv_learners(learners = learners)
## 
## learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)


## ----spec_init_WASH, eval=FALSE------------------------------------------
## ## Question 2:
## 
## #Initialize a tmle specification
## tmle_spec <- tmle3_mopttx_blip_revere(
##   V = c("momedu", "floor", "asset_refrig"), type = "blip2",
##   learners = learner_list, maximize = TRUE, complex = TRUE,
##   realistic = FALSE
## )
## 
## #Fit the TML estimator.
## fit <- tmle3(tmle_spec, data=washb_data, node_list, learner_list)
## fit
## 
## #Which intervention is the most dominant?
## table(tmle_spec$return_rule)


## ----spec_init_WASH_simple_q3, eval=FALSE--------------------------------
## ## Question 3:
## 
## #Initialize a tmle specification with "realistic=TRUE":
## tmle_spec <- tmle3_mopttx_blip_revere(
##   V = c("momedu", "floor", "asset_refrig"), type = "blip2",
##   learners = learner_list, maximize = TRUE, complex = TRUE,
##   realistic = TRUE
## )
## 
## #Fit the TML estimator.
## fit <- tmle3(tmle_spec, data=washb_data, node_list, learner_list)
## fit
## 
## table(tmle_spec$return_rule)


## ---- fig.cap="Animation of how a counterfactual outcome changes as the natural treatment distribution is subjected to a simple stochastic intervention", echo=FALSE, eval=TRUE, out.width='60%'----
knitr::include_graphics(path = "img/gif/shift_animation.gif")


## ----setup-shift, message=FALSE, warning=FALSE---------------------------
library(tidyverse)
library(data.table)
library(condensier)
library(sl3)
library(tmle3)
library(tmle3shift)
set.seed(429153)


## ----sl3_lrnrs-Qfit-shift, message=FALSE, warning=FALSE------------------
# learners used for conditional expectation regression
lrn_mean <- Lrnr_mean$new()
lrn_fglm <- Lrnr_glm_fast$new()
lrn_xgb <- Lrnr_xgboost$new(nrounds = 200)
sl_lrn <- Lrnr_sl$new(
  learners = list(lrn_mean, lrn_fglm, lrn_xgb),
  metalearner = Lrnr_nnls$new()
)


## ----sl3_density_lrnrs_search-shift, message=FALSE, warning=FALSE--------
sl3_list_learners("density")


## ----sl3_lrnrs-gfit-shift, message=FALSE, warning=FALSE------------------
# learners used for conditional density regression (i.e., propensity score)
lrn_haldensify <- Lrnr_haldensify$new(
  n_bins = 5, grid_type = "equal_mass",
  lambda_seq = exp(seq(-1, -13, length = 500))
)
lrn_rfcde <- Lrnr_rfcde$new(
  n_trees = 1000, node_size = 5,
  n_basis = 31, output_type = "observed"
)
sl_lrn_dens <- Lrnr_sl$new(
  learners = list(lrn_haldensify, lrn_rfcde),
  metalearner = Lrnr_solnp_density$new()
)


## ----learner-list-shift, message=FALSE, warning=FALSE--------------------
Q_learner <- sl_lrn
g_learner <- sl_lrn_dens
learner_list <- list(Y = Q_learner, A = g_learner)


## ----sim_data, message=FALSE, warning=FALSE------------------------------
# simulate simple data for tmle-shift sketch
n_obs <- 1000 # number of observations
tx_mult <- 2 # multiplier for the effect of W = 1 on the treatment

## baseline covariates -- simple, binary
W <- replicate(2, rbinom(n_obs, 1, 0.5))

## create treatment based on baseline W
A <- rnorm(n_obs, mean = tx_mult * W, sd = 1)

## create outcome as a linear function of A, W + white noise
Y <- rbinom(n_obs, 1, prob = plogis(A + W))

# organize data and nodes for tmle3
data <- data.table(W, A, Y)
setnames(data, c("W1", "W2", "A", "Y"))
node_list <- list(W = c("W1", "W2"), A = "A", Y = "Y")
head(data)


## ----spec_init-shift, message=FALSE, warning=FALSE-----------------------
# initialize a tmle specification
tmle_spec <- tmle_shift(
  shift_val = 0.5,
  shift_fxn = shift_additive_bounded,
  shift_fxn_inv = shift_additive_bounded_inv
)


## ----fit_tmle-shift, message=FALSE, warning=FALSE, cache=FALSE-----------
tmle_fit <- tmle3(tmle_spec, data, node_list, learner_list)
tmle_fit


## ----vim_spec_init, message=FALSE, warning=FALSE-------------------------
# what's the grid of shifts we wish to consider?
delta_grid <- seq(-1, 1, 1)

# initialize a tmle specification
tmle_spec <- tmle_vimshift_delta(
  shift_grid = delta_grid,
  max_shifted_ratio = 2
)


## ----fit_tmle_wrapper_vimshift, message=FALSE, warning=FALSE, cache=FALSE----
tmle_fit <- tmle3(tmle_spec, data, node_list, learner_list)
tmle_fit


## ----msm_fit, message=FALSE, warning=FALSE-------------------------------
tmle_fit$summary[4:5, ]


## ----vim_targeted_msm_fit, message=FALSE, warning=FALSE, cache=FALSE-----
# initialize a tmle specification
tmle_msm_spec <- tmle_vimshift_msm(
  shift_grid = delta_grid,
  max_shifted_ratio = 2
)

# fit the TML estimator and examine the results
tmle_msm_fit <- tmle3(tmle_msm_spec, data, node_list, learner_list)
tmle_msm_fit


## ----load-washb-data-shift, message=FALSE, warning=FALSE, cache=FALSE----
washb_data <- fread("https://raw.githubusercontent.com/tlverse/tlverse-data/master/wash-benefits/washb_data.csv", stringsAsFactors = TRUE)
washb_data <- washb_data[!is.na(momage), lapply(.SD, as.numeric)]
head(washb_data, 3)


## ----washb-data-npsem-shift, message=FALSE, warning=FALSE, cache=FALSE----
node_list <- list(
  W = names(washb_data)[!(names(washb_data) %in%
    c("whz", "momage"))],
  A = "momage", Y = "whz"
)


## ----vim_spec_init_washb, message=FALSE, warning=FALSE-------------------
# initialize a tmle specification for the variable importance parameter
washb_vim_spec <- tmle_vimshift_delta(
  shift_grid = c(-2, 2),
  max_shifted_ratio = 2
)


## ----sl3_lrnrs-gfit-shift-washb, message=FALSE, warning=FALSE------------
# learners used for conditional density regression (i.e., propensity score)
lrn_rfcde <- Lrnr_rfcde$new(
  n_trees = 500, node_size = 3,
  n_basis = 20, output_type = "observed"
)
# modify learner list, using existing SL for Q fit
learner_list <- list(Y = sl_lrn, A = lrn_rfcde)


## ----fit_tmle_wrapper_washb, message=FALSE, warning=FALSE, eval=FALSE----
## washb_tmle_fit <- tmle3(washb_vim_spec, washb_data, node_list, learner_list)
## washb_tmle_fit

