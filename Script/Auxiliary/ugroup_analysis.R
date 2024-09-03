# exploratory plots for patients AE > 20
ids <- subset(pred_xgb0, AE_xgb0 > 20)
ids <- ids$Number

# retrieve baseline measurement only
pats <- fdat %>% filter(Number %in% ids) %>% group_by(Number) %>% slice_min(Days, n=1) %>% 
npats <- length(ids)
cat("Number of patients",npats," \n")

# Convert the data from wide to long format
dat_long_cat <- pats %>%
  pivot_longer(cols = all_of(catvars), names_to = "variable", values_to = "value", names_prefix = "cat_") %>%
  select(Number, variable, value)

# Create separate bar plots for each categorical variable
ggplot(dat_long_cat, aes(x = value)) +
  geom_bar() +
  labs(
    x = "Category",
    y = "Count"
  ) +
  fancy_multi +
  facet_wrap(~variable, scales = "free")

# create jittered box plots for all numvars
plots <- box.plot.jitter(pats, ID = "Number", vars = numvars)

# plot boxplots in grid
grid.arrange(grobs = plots, ncol = 3)

# get mean and sd for numvars
pats %>%
  summarise(across(all_of(numvars), list(mean = mean, sd = sd, min = min, max = max), .names = "{col}-{fn}")) %>%
  pivot_longer(cols = everything(), # pivot to long format and rename cols/rows
               names_to = c(".value", "stat"),
               names_sep = "-")


