# ------------------------------------------------------------------------------------------------------------------------ #
#                         Prediction of patient-specific recovery based on early clinical data                             #
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                                                                                          #
# Description:  Prediction of individualized recovery using XGBoost as compared to existing Mixed Effect Model.            #
# Authors:      Govert van der Gun                                                                                         #
# Created:      15-09-2023                                                                                                 #                                                                                            #
# R.version:    4.3.1 (2023-06)                                                                                            #
# Rstudio:      2023.09.1-494                                                                                              #
#                                                                                                                          #
#                                                                                                                          #
# ------------------------------------------------------------------------------------------------------------------------ #


# ------------------------------------------------------------------------------------------------------------------------ #
#                                               Settings & dependencies                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

# custom functions
source('Script/Auxiliary/setup.R') # general setup
source('Script/Functions/box.plot.jitter.R') # jittered box plots
source('Script/Functions/my.rep.folds.R') # create patient-wise repeated folds for CV
source('Script/Functions/Selles.MM.R') # running Selles mixed models
source('Script/Functions/predict.multi.MM.R') # running Selles MM for multiple patients
source('Script/Functions/xgb.learn.curve.R') # learn curve creation for xgb
source('Script/Functions/predict.XGB.R') # bootstrap XGB predictions and prediction intervals
source("Script/Functions/pred.viz.ann.R") # visualise annotated XGB predictions
source("Script/Functions/pred.viz.mm.R") # visualise MM predictions
source("Script/Functions/pred.vizA.R") # design A of XGB prediction visualisation
source("Script/Functions/pred.vizB.R") # design B "
source("Script/Functions/pred.vizC.R") # design C "

# seed for reproducibility
seed <- 137985

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                      Data acquisition                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

# load raw data from V
load("Data\\Data_raw.RData")
dat_raw <- data

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                      Data preprocessing                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------
# Removing invalids
# ------------------------------------------------------   

# remove outliers and/or erroneous data
summary(dat_raw)

# replace or drop erroneous data
dat <- dat_raw %>% 
  mutate(RTPA = replace(RTPA, RTPA > 1, NA)) %>%  # RTPA is binary?
  mutate(Sens = replace(Sens, Sens > 2, NA)) %>%  # NIHS scale 8 is max [2]
  distinct(across(-Index1), .keep_all = TRUE) # only keep rows that have distinct values (disregarding Index1)

# ------------------------------------------------------
# Class conversions
# ------------------------------------------------------

# convert categorical vars to factor
dat$Number <- factor(dat$Number)

labels <- list(
  GENDER = c("male", "female"),
  BAMFORD = c("LACI", "PACI", "TACI"),
  RTPA = c("no", "yes"),
  AFFECTED_BODYSIDE = c("right", "left"),
  PREFERRED_HAND = c("right", "left", "no pref."),
  SA = c("0","9","14","19","25","33"),
  FE = c("none", "partial", "full"),
  Sens = c("0","1","2"),
  Neglect = c("0","1","2")
)

for (var_name in names(labels)) {
  # convert to factor and assign correct labels
  dat[[var_name]] <- factor(dat[[var_name]], labels = labels[[var_name]])
  # keep useful metadata
  attr(dat[[var_name]], "label") <- attr(dat_raw[[var_name]],"label") 
}

# get variable names
vars <- colnames(dat)
allvars <- vars[2:length(vars)] # all variables except patient number
catvars <- names(labels) # all categorical variables
catvars <- catvars[catvars %in% allvars] # cat vars in current feature set
numvars <- setdiff(allvars, catvars) # only numerical variables

# ------------------------------------------------------
# Inclusion criteria
# ------------------------------------------------------

# number of measurements per patient
nm_per_pat <- dat %>%
  group_by(Number) %>%
  summarise(nm = n())
nm_per_pat %>%
  summarise(across(nm, list(median = median, sd = sd, min = min, max = max), .names = "nm-{fn}"))

# count
nPatients_drop <- sum(nm_per_pat$nm < 2)
cat("# Patients with less than 2 obs.:", nPatients_drop, "\n")

# drop patients with less than 2 obs / keep patients with more than 1 obs
dat <- dat %>%
  filter(Number %in% nm_per_pat$Number[nm_per_pat$nm > 1])

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Data exploration                                                   #
# ------------------------------------------------------------------------------------------------------------------------ #

# # ------------------------------------------------------
# # Missing data analysis
# # ------------------------------------------------------
# 
# # check for NA's
# colSums(is.na(dat))
# 
# # ------------------------------------------------------
# # Univariate analysis
# # ------------------------------------------------------
# 
# # numerical variables only
# dat_norm <- dat %>%
#   select(all_of(numvars)) %>%
#   na.omit()
# 
# # apply shapiro test to columns and immediately extract p value
# set.seed(seed)
# shap_p <- apply(dat_norm, 2, function(x) shapiro.test(x)$p.value)
# 
# # create data frame for plotting with p value as character
# shap_lab <- data.frame(
#   label = paste0("p = ",as.character(shap_p)),
#   Variable  = numvars
# )
# 
# dat_norm %>%
#   gather(key = Variable, value = Value) %>%
#   ggplot(aes(x=Value)) + geom_density(fill="lightgrey") + fancy +
#   facet_wrap(~Variable,ncol=3,scales="free") +
#   geom_text(
#     data    = shap_lab,
#     mapping = aes(x = Inf, y = -Inf, label = label),
#     hjust = 1.2,
#     vjust = -3,
#   ) +
#   labs(x = "",
#        y = "")
# 
# # Convert the data from wide to long format
# dat_long_cat <- dat %>%
#   pivot_longer(cols = all_of(catvars), names_to = "variable", values_to = "value", names_prefix = "cat_") %>%
#   select(Number, variable, value)
# 
# # Create separate bar plots for each categorical variable
# ggplot(dat_long_cat, aes(x = value)) +
#   geom_bar() +
#   labs(
#     x = "Category",
#     y = "Count"
#   ) +
#   fancy_multi +
#   facet_wrap(~variable, scales = "free")
# 
# # create jittered box plots for all numvars
# plots <- box.plot.jitter(dat, ID = "Number", vars = numvars)
# 
# # plot boxplots in grid
# grid.arrange(grobs = plots, ncol = 3)
# 
# # get mean and sd for numvars
# dat %>%
#   summarise(across(all_of(numvars), list(mean = mean, sd = sd, min = min, max = max), .names = "{col}-{fn}")) %>%
#   pivot_longer(cols = everything(), # pivot to long format and rename cols/rows
#                names_to = c(".value", "stat"),
#                names_sep = "-")
# 
# plot distribution of days
Tout <- 180 # median outcome is measured at 186 days...
sdout <- 14 # ... with standard deviation 13 days
Tbase <- 11 # median baseline is measured at 11 days
T6w <- 42
T3m <- 91
# 
# ggplot(dat, aes(x = Days)) +
#   geom_histogram(binwidth = 7) +
#   geom_vline(xintercept = Tout-sdout, color = "#3b528b", linetype = "dashed", linewidth = .5) +
#   geom_vline(xintercept = Tout, color = "#440154", linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = Tout+sdout, color = "#3b528b", linetype = "dashed", linewidth = .5) +
#   geom_vline(xintercept = Tbase, color = "black", linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = T6w, color = "black", linetype = "dashed", linewidth = 1) +
#   geom_vline(xintercept = T3m, color = "black", linetype = "dashed", linewidth = 1) +
#   labs(x = "Days", y = "Frequency") +
#   fancy
# 
# # ------------------------------------------------------
# # Multivariate analysis
# # ------------------------------------------------------
# 
# # multicollinearity
# 
# # correlation matrix
# # repeated measures correlation matrix
# set.seed(seed)
# corr_all <-  rmcorr_mat(participant = Number,
#                         variables = numvars,
#                         dataset = dat,
#                         CI.level = 0.95)
# corrplot(corr_all$matrix,type = 'upper',tl.cex = 0.45, method='number',number.cex=0.5)

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                     Feature engineering                                                  #
# ------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------
# OHE
# ------------------------------------------------------

# Create dummy vars model
formula <- as.formula(paste("~ ", paste(catvars, collapse = " + "))) # formula for the vars to OHE
dummies_model <- dummyVars(formula, data=dat) # exclude patient ID

# Apply dummy vars model to data
dat_ohe <- data.frame(predict(dummies_model, newdata = dat))

# get colnames of dummyvars
dummynames <- colnames(dat_ohe)

# bind encoded vars back to main modelling data
dat_mod <- cbind(dat,dat_ohe) 

# ------------------------------------------------------
# Feature selection
# ------------------------------------------------------

#  XGB features to be included in XGB model
xgb_ftrs <- c('Number',
              'Days',
              'SA.0','SA.9','SA.14','SA.19','SA.25','SA.33',
              'FE.none','FE.partial','FE.full',
              'ARAT') # pred. feature set identical to Selles MM

# select outcome observation (=obs at Tout)
obs_out <- dat_mod %>%
  group_by(Number) %>%
  mutate(Days_diff = abs(Days - Tout)) %>% # calculate the absolute difference to 6 months
  filter(Days_diff == min(Days_diff)) %>% # filter for the closest measurement
  filter(Days_diff <= sdout) %>% # drop any measurement that is outside of +- 14 days interval
  select(-Days_diff) %>% 
  ungroup()

# plot outcome obs per patient
ggplot(subset(dat_mod, Number %in% obs_out$Number)) +
  geom_line(aes(x = Days, y = ARAT, group = Number), color = "lightgrey", linewidth = 1) +
  geom_vline(xintercept = Tout-sdout, color = "#3b528b", linetype = "dashed", linewidth = .5) +
  geom_vline(xintercept = Tout, color = "#440154", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = Tout+sdout, color = "#3b528b", linetype = "dashed", linewidth = .5) +
  geom_point(data = obs_out, aes(x = Days, y = ARAT), size = 2, shape=18, color="#5ec962") +
  labs(x = "Days since onset", y = "ARAT") +
  scale_x_continuous(breaks = seq(0, 300, by = 50), expand = c(.01,.01)) +
  scale_y_continuous(breaks = seq(0, 60, by = 10), limits = c(0,57), expand = c(.01,.01)) +
  coord_cartesian(xlim= c(0,250)) +
  fancy

# create outcome var (= ARAT of at Tout)
Y <- obs_out %>% select(Number, ARAT) %>% rename(Outcome = ARAT)

# select repeated measurements of the predictor vars
X <- dat_mod %>%
  anti_join(obs_out) %>% # outcome measurement cannot be predictor
  select(all_of(xgb_ftrs))

# bind outcome back to patients
dat_mod_xgb <- merge(X, Y, by = "Number", all.y=T) %>% 
  arrange(Number)
  
# data for Mixed Effects Model
mm_ftrs <- c('Number',
             'Days',
             'SA',
             'FE',
             'ARAT')

# select relevant predictors
dat_mod_mm <- dat_mod %>% 
  select(all_of(mm_ftrs)) %>% 
  filter(Number %in% dat_mod_xgb$Number)

# bind outcome back to patients for mm
dat_mod_mm <- merge(dat_mod_mm, Y, by = "Number", all.x=T) %>% 
  arrange(Number)
  
# ------------------------------------------------------------------------------------------------------------------------ #
#                                                       Data modelling                                                     #
# ------------------------------------------------------------------------------------------------------------------------ #

# ------------------------------------------------------
# Data Partitioning
# ------------------------------------------------------

# balanced grouped based split (80-20)
set.seed(seed)
parts <- partition(dat_mod_xgb, p = 0.20, id_col = "Number", num_col = "Outcome")
test_split <- parts[[1]] %>%
  mutate(Set = "Test") # for plotting
train_split <- parts[[2]] %>%
  mutate(Set = "Train") # for plotting

# evaluate balance train-test split
dat_mod_eval <- rbind(train_split, test_split)

# number of observations
ggplot(dat_mod_eval,aes(x = Set)) +
  geom_bar() +
  labs(x = "Set",
       y = "Count") +
  fancy

# outcome and predictors
ggplot(dat_mod_eval,aes(x = Outcome, fill = factor(Set))) +
  geom_density(alpha = 0.5) +
  labs(x = "ARAT",
       y = "Density") +
  scale_fill_viridis(discrete=T) +
  fancy

# XGB train-test split
train_xgb <- select(train_split,-Set)
test_xgb <- select(test_split,-Set)

# MM train-test split (identical patients but different features)
train_mm <- dat_mod_mm %>%
  filter(Number %in% train_xgb$Number)

test_mm <- dat_mod_mm %>%
  filter(Number %in% test_xgb$Number)

# ------------------------------------------------------
# Training
# ------------------------------------------------------

# # training method and config (not required when bootstrap predictions are used)
# # create repeated cross-validation folds
# myRepFolds <- my.rep.folds(train_xgb, IDvarn = "Number", nreps = 5, nfolds = 5, seed = seed)
# 
# train_ctrl <- trainControl(
#   method = "cv", # cross-validation
#   number = length(myRepFolds), # amount of folds*repeats
#   index = myRepFolds, # fixed folds for repeated CV
#   verbose = F, # TRUE for training log
#   allowParallel = F # FALSE for reproducible results
# )

# ------------------------------------------------------
# XGBoost
# ------------------------------------------------------

# # XGB default hyperparameters
# xgb_grd <- expand.grid(
#   nrounds = 100, # number of boosting rounds
#   max_depth = 6, # max tree depth
#   eta = 0.3, # learning rate
#   gamma = 0, # regularization param. (pruning)
#   colsample_bytree = 1, # fraction of features used for each tree
#   min_child_weight = 1, # minimum sum of instance weight (prevent single obs. children)
#   subsample = 1 # fraction of training data used in each round
# )

# XGB tuned hyperparameters (see tune branch)
xgb_grd <- expand.grid(
  nrounds = 5000, # number of boosting rounds
  max_depth = 4, # max tree depth
  eta = 0.015, # learning rate
  gamma = 0, # regularization param. (pruning)
  colsample_bytree = 1, # fraction of features used for each tree
  min_child_weight = 1, # minimum sum of instance weight (prevent single obs. childs)
  subsample = 0.75 # fraction of training data used in each round
)

# # train xgb base model (not required when using bagging)
# X <- colnames(select(train_xgb,-c(Outcome,Number)))
# form_str <- paste("Outcome ~", paste(X, collapse = " + "))
# formula <- as.formula(form_str)
# 
# set.seed(seed)
# M_xgb <- train(
#   form = formula, # model formula
#   data = train_xgb, # train data set
#   trControl = train_ctrl, # train method
#   tuneGrid = xgb_grd, # hyperparameters
#   method = "xgbTree", # modelling method
#   metric = "MAE", # performance metric to optimize
#   na.action = na.pass
# )

# ------------------------------------------------------
# Selles Mixed Model
# ------------------------------------------------------

# built mixed model
M_mm <- lme(ARAT ~ SA * ns(Days, knots = c(6, 11, 19, 34, 50, 91)) + FE * ns(Days, knots = c(6, 11, 19, 34, 50, 91)),
             data = train_mm,
             random = list(Number = pdDiag(form = ~ ns(Days, knots = c(6, 11, 19, 34, 50, 91)))),
             na.action = na.exclude,
             control = lmeControl(maxIter = 1e8, msMaxIter = 1e8)
            )

# ------------------------------------------------------
# Model evaluation & comparison
# ------------------------------------------------------

# define moment post-stroke [Days] when predictions are made (day at which model is applied)
applied_at <- 7*1

# extract measurement(s) from test set to evaluate the XGB and MM on
eval_xgb <- test_xgb %>% 
  group_by(Number) %>%
  filter(Days <= applied_at) %>% 
  slice_max(Days, n=1) %>% # most recent measurement available within the current period
  ungroup()
eval_mm <- test_mm %>% 
  group_by(Number) %>% 
  filter(Days <= applied_at) %>% 
  ungroup()

alpha <- 0.2 # set significance level for CIs/PIs

# # get prediction using final best model (caret)
# preds <- predict(M_xgb, eval_xgb)
# pred_xgb <- data.frame(Number = eval_xgb$Number,
#                        AE_xgb = abs(eval_xgb$Outcome-preds)
#                        ) 

# get bagged prediction of XGB ensemble
pred_list <- predict.XGB(dat_train = train_xgb, # train data to bootstrap on
                        new_dat = eval_xgb, # new data to provide predictions + intervals for
                        X = colnames(select(train_xgb,-c(Outcome,Number))), # names of predictors
                        grd = xgb_grd, # tune grid
                        nb = 300, # number of bootstraps
                        alpha = alpha, # significance level for PIs
                        seed = seed
                        )

# calculate performance metric on unseen data
pred_xgb <- pred_list[[1]] %>%
  mutate(AE_xgb = abs(Actual-Predicted)) %>%
  mutate(PIw = abs(lb-ub))

# predict last ARAT using mixed model
pred_mm <- predict.multi.MM(M = M_mm, # lme model object
                            new_dat = eval_mm, # new data to provide predictions for
                            alpha = alpha # significance level for CIs 
                            )

# calculate performance metric on unseen data
pred_mm <- pred_mm %>%
  mutate(AE_mm = abs(Actual-Predicted)) %>% 
  mutate(lb = pmax(0, pmin(lb, 57))) %>% 
  mutate(ub = pmax(0, pmin(ub, 57))) %>% 
  mutate(PIw = abs(lb-ub))

# merge performance metrics by patient number
perf <- merge(pred_xgb[, c("Number","AE_xgb")], pred_mm[, c("Number","AE_mm")], by = "Number") %>%
  arrange(Number)

# print results on test set to console
quantile(perf$AE_xgb)
quantile(perf$AE_mm)

# median difference test (H0: there is no median difference between the two error distributions)
w <- wilcox.test(perf$AE_mm, perf$AE_xgb, paired = T)
z <- qnorm(w$p.value/2)

# eval: percentage of targets that falls outside of estimated PI
nhits <- pred_xgb %>%
  mutate(Hit = ifelse(Actual >= lb & Actual <= ub,T,F)) %>%
  pull(Hit) %>%
  sum()
phits <- round(nhits/nrow(pred_xgb)*100,2)
cat(phits,"% of targets fall within the ",(1-alpha)*100,"% prediction interval (XGB) \n")

nhits2 <- pred_mm %>%
  mutate(Hit = ifelse(Actual >= lb & Actual <= ub,T,F)) %>%
  pull(Hit) %>%
  sum()
phits2 <- round(nhits2/nrow(pred_mm)*100,2)
cat(phits2,"% of targets fall within the ",(1-alpha)*100,"% prediction interval (MM) \n") 

# ------------------------------------------------------------------------------------------------------------------------ #
#                                                    Data visualization                                                    #
# ------------------------------------------------------------------------------------------------------------------------ #

# TABLE 1: patients included in analysis
ids <- dat_mod_xgb$Number

tabone <-dat %>% 
  #filter(Number %in% ids) %>% 
  #filter(Days <= 3) %>% 
  group_by(Number) %>% 
  slice_min(Days, n=1) %>% 
  ungroup() %>%
  select(-c(Index1, Number))

#table1(~ AGE + GENDER + ARAT + SA + FE, data=tabone)
table1(~ ., data=tabone)

# ------------------------------------------------------
# Figures
# ------------------------------------------------------

# FIGURE 2: spaghetti plot with ARAT evolution
plot <- ggplot(dat, aes(x = Days, y = ARAT, group = Number)) +
  geom_line(color = "lightgrey", linewidth = 1) +
  labs(x = "Days since onset", y = "ARAT") +
  scale_x_continuous(breaks = c(0,60,120,180,240), expand = c(.01,.01)) +
  scale_y_continuous(breaks = c(0,20,40,57), limits = c(0,57), expand = c(.01,.01)) +
  coord_cartesian(xlim= c(0,250)) +
  fancy

# patients to highlight
highlight_patients <- c(32, 42, 88, 337, 136)
highlight_patients <- c(3,5,14,23,35,41,42,85,97,101,123,125,154,169,170,180,196,199,235,247) # typical middle group patients

# highlight patients
for (id in highlight_patients) {
  plot <- plot + 
    geom_line(data = subset(dat, Number == id),
              aes(color = as.factor(Number)), 
              lwd = 1.5) +
    geom_point(data = subset(dat, Number == id),
               aes(color = as.factor(Number)), 
               size = 3)
}

# output fig 2
plot + theme(legend.position = "none") + scale_color_viridis_d()

# FIGURE 3: panel plot, comparing two typical patients
plot_list_xgb1 <- setNames(lapply(plot_pats, pred.vizB),plot_pats)
plot_list_mm1 <- setNames(lapply(plot_pats, pred.viz.mm),plot_pats)

pA1xgb <- plot_list_xgb1[["257"]] + labs(x="")
pA1mm <- plot_list_mm1[["257"]] + labs(x="", y="")
pB1xgb <- plot_list_xgb1[["403"]] + labs(x="", y="")
pB1mm <- plot_list_mm1[["403"]] + labs(x="", y="")
pA2xgb <- plot_list_xgb1[["257"]]
pA2mm <- plot_list_mm1[["257"]] + labs(x="", y="")
pB2xgb <- plot_list_xgb1[["403"]] + labs(x="", y="")
pB2mm <- plot_list_mm1[["403"]] + labs(x="", y="")

grid.arrange(pA1xgb, pA1mm, pB1xgb, pB1mm, pA2xgb, pA2mm, pB2xgb, pB2mm, 
             ncol = 4, vp=viewport(width=0.97, height=0.95, x=0.52, y=0.47))
grid.text("Week 1", x = 0.015, y = 0.75, rot = 90, gp = gpar(fontsize = 14))
grid.text("Week 6", x = 0.015, y = 0.275, rot = 90, gp = gpar(fontsize = 14))
grid.text("Patient 1", x = 0.3, y = 0.98, gp = gpar(fontsize = 14))
grid.text("Patient 2", x = 0.785, y = 0.98, gp = gpar(fontsize = 14))
grid.lines(x = unit(c(0.53, 0.53), "npc"), y = unit(c(0, 1), "npc"), 
           gp = gpar(col = "black", alpha = 0.6, lwd = 1))
grid.lines(x = unit(c(0, 1), "npc"), y = unit(c(0.495, 0.495), "npc"), 
           gp = gpar(col = "black", alpha = 0.6, lwd = 1))


# FIGURE 4: overall performance boxplots
perflong <- perf %>% 
  pivot_longer(cols = -Number)

ggplot(perflong, aes(x = name, y = value)) + 
  geom_boxplot() + 
  labs(y = "Absolute Error",
       x = "") +
  scale_x_discrete(labels = c("Mixed Model", "XGBoost")) + 
  scale_y_continuous(lim =c(0,60)) +
  fancy

# FIGURE 5: XGB and MM performance over time, stratified by baseline ARAT
# note: run XGB_perf_over_time and MM_perf_over_time auxiliary script to get:
# load("Data/perf_over_wks_xgb.Rdata")
# load("Data/perf_over_wks_mm.Rdata")

sample_size <- aggregate(AE ~ ARAT_base + time, perf_wks_xgb, length) %>% rename(count = AE)
labels <- c(L = "Baseline ARAT 0-22",
            M = "Baseline ARAT 22-47",
            H = "Baseline ARAT 48-57")
perf_wks_mm <- perf_wks_mm %>% mutate(mod = "MM")
perf_wks_xgb <- perf_wks_xgb %>% mutate(mod = "XGB")
perf_wks <- rbind(perf_wks_mm, perf_wks_xgb)

perf_wks %>%
  left_join(sample_size, by = c("time", "ARAT_base")) %>%
  mutate(myaxis = factor(paste0(time, "\n", "n=", count),
                         levels = unique(paste0(time, "\n", "n=", count)))) %>%
  ggplot(aes(x = myaxis, y = AE, fill=mod)) +
  geom_boxplot() +
  facet_wrap(~ ARAT_base, scales = "free",
             labeller = labeller(ARAT_base = labels)) +
  labs(y = "Absolute Error",
       x = "",
       fill = "Model") +
  scale_y_continuous(lim =c(0,60))  +
  scale_fill_manual(values = c("#31688E","#35B779"), labels = c("Mixed Model", "XGBoost")) +
  fancy +
  theme(legend.position = "bottom")

# FIGURE 6: annotated XGB prediction visualisation
dat_plot <- dat_mod %>%
  filter(Number %in% eval_xgb$Number) %>% 
  group_by(Number) %>% 
  filter(Days <= applied_at) %>% # model applied at
  select(Number, SA, FE, ARAT, Days) %>% 
  ungroup()
plot_pats <- unique(dat_plot$Number)

plot_list <- setNames(lapply(plot_pats, pred.viz.ann),plot_pats)
grid.draw(plot_list[["257"]])

# SUPPL. FIGURE I: XGB learn curve
lctrain_xgb <-train_xgb %>%
  group_by(Number) %>% 
  slice_min(Days, n=1) %>% 
  ungroup()
lctest_xgb <- test_xgb %>%
  group_by(Number) %>% 
  slice_min(Days, n=1) %>% 
  ungroup()
lc_dat_xgb <- xgb.learn.curve(lctrain_xgb, # train data
                              lctest_xgb, # test data
                              X = colnames(select(train_xgb,-c(Outcome,Number))), # names of predictors
                              IDvarn = "Number", # name of ID var
                              grid = xgb_grd, # hyperparams
                              seed = seed
                              )

# plot 
lc_dat_xgb[[2]] + fancy

# SUPPL. FIGURE II: XGB learn curve
# done manually, see multi_measure_BP

############################################################################################################################
#                                                   End of syntax                                                          #
############################################################################################################################            