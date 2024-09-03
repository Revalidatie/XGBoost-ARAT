# Define the ranges and levels
Number <- 1:(58*181*6*3)
ARAT <- 0:57
SA <- factor(c(0, 9, 14, 19, 25, 33), levels = c(0, 9, 14, 19, 25, 33))
FE <- factor(c("none", "partial", "full"), levels = c("none", "partial", "full"))
Days <- 0:180

# Create the data frame with all combinations
df <- expand.grid(ARAT = ARAT, SA = SA, FE = FE, Days = Days)

# Create dummy vars model
formula <- as.formula(paste("~ ", paste(c("SA", "FE"), collapse = " + "))) # formula for the vars to OHE
dummies_model <- dummyVars(formula, data=df) # exclude patient ID

# Apply dummy vars model to data
dat_all <- data.frame(predict(dummies_model, newdata = df))
dat_all <- dat_mod <- cbind(Number, df,dat_all) %>% select('Number',
                                                           'Days',
                                                           'SA.0','SA.9','SA.14','SA.19','SA.25','SA.33',
                                                           'FE.none','FE.partial','FE.full',
                                                           'ARAT')
save(dat_all, file = "Output/all.Rdata")
