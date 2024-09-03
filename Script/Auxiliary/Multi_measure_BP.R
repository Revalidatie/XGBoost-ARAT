nm <- 1 # number of measurements to included per patient
mmn <- 7 # maximum measurement number to included per patient

# extract measurement(s) from test set to evaluate the XGB and MM on
eval_xgb <- test_xgb %>%
  group_by(Number) %>%
  slice_min(Days, n=mmn) %>% 
  slice_max(Days, n=nm) %>%
  arrange(Number, Days) %>% 
  ungroup()
eval_mm <- test_mm %>%
  filter(Days < 179) %>% 
  group_by(Number) %>%
  slice_min(Days, n=mmn) %>% 
  slice_max(Days, n=nm) %>%
  arrange(Number, Days) %>% 
  ungroup()

# ------------------------------
## Running #1 until #7 and storing:
temp <- data.frame(
  Group = rep("G"),
  XGB = perf$AE_xgb,
  MM = perf$AE_mm
)

bp_dat <- rbind(bp_dat,temp)
# A: #1
# B: #1-3
# C: #3
# D: #1-5
# E: #5
# F: #1-7
# G: #7
# ------------------------------

# rename groups
bp_dat <- bp_dat %>%
  mutate(Group = ifelse(Group == "A", "#1", Group)) %>% 
  mutate(Group = ifelse(Group == "B", "#1-3", Group)) %>% 
  mutate(Group = ifelse(Group == "C", "#3", Group)) %>% 
  mutate(Group = ifelse(Group == "D", "#1-5", Group)) %>% 
  mutate(Group = ifelse(Group == "E", "#5", Group)) %>% 
  mutate(Group = ifelse(Group == "F", "#1-7", Group)) %>% 
  mutate(Group = ifelse(Group == "G", "#7", Group))

bp_dat$Group <- factor(bp_dat$Group, levels = c("#1","#1-3","#3","#1-5","#5","#1-7","#7"))

# pivot longer
bp_dat_long <- tidyr::gather(bp_dat, key = "Variable", value = "Value", -Group)

#save(bp_dat, file = "Data/bp_dat_new_Mxgb.RData")
load("Data/bp_dat_long.RData")

# plot mm & xgb
ggplot(bp_dat_long, aes(x = Group, fill = interaction(Group, Variable))) +
  geom_boxplot(aes(y = Value), position = position_dodge(width = 0.8), width = 0.7) +
  labs(x = "Measurement #",
       y = "Absolute Error") +
  scale_y_continuous(limits = c(0,60)) +
  scale_fill_manual(breaks = c("#1.MM","#1.XGB"),
                    values = c("#1.XGB" = "#35b779", "#1.MM" = "#443983",
                               "#1-3.XGB" = "#35b779", "#1-3.MM" = "#443983",
                               "#3.XGB" = "#35b779", "#3.MM" = "#443983",
                               "#1-5.XGB" = "#35b779", "#1-5.MM" = "#443983",
                               "#5.XGB" = "#35b779", "#5.MM" = "#443983",
                               "#1-7.XGB" = "#35b779", "#1-7.MM" = "#443983",
                               "#7.XGB" = "#35b779", "#7.MM" = "#443983"),
                    name = "Models",
                    labels = c("Mixed Model", "XGB")) +
  fancy