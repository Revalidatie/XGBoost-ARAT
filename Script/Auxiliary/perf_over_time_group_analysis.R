load("Data/perf_over_wks_xgb.Rdata")
perf_wks_xgb <- perf_wks_xgb %>% mutate(mod = "XGB")
load("Data/perf_over_wks_mm.Rdata")
perf_wks_mm <- perf_wks_mm %>% mutate(mod = "MM")

perf_wks <- rbind(perf_wks_mm, perf_wks_xgb)

# high group ----
AE_H_1_xgb <- perf_wks %>% filter(ARAT_base == "H" & time == "Week 1" & mod == "XGB") %>% pull(AE)
AE_H_6_xgb <- perf_wks %>% filter(ARAT_base == "H" & time == "Week 6" & mod == "XGB") %>% pull(AE)
AE_H_13_xgb <- perf_wks %>% filter(ARAT_base == "H" & time == "Week 13" & mod == "XGB") %>% pull(AE)

AE_H_1_mm <- perf_wks %>% filter(ARAT_base == "H" & time == "Week 1" & mod == "MM") %>% pull(AE)
AE_H_6_mm <- perf_wks %>% filter(ARAT_base == "H" & time == "Week 6" & mod == "MM") %>% pull(AE)
AE_H_13_mm <- perf_wks %>% filter(ARAT_base == "H" & time == "Week 13" & mod == "MM") %>% pull(AE)

# distributions
ggplot(filter(perf_wks, ARAT_base == "H" & time == "Week 1"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 1") +
  scale_fill_viridis(discrete=T) +
  fancy

ggplot(filter(perf_wks, ARAT_base == "H" & time == "Week 6"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 6") +
  scale_fill_viridis(discrete=T) +
  fancy

ggplot(filter(perf_wks, ARAT_base == "H" & time == "Week 13"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 13") +
  scale_fill_viridis(discrete=T) +
  fancy

# correlations are not actually high, but since AE is from same patients using paired samples test
# t test is valid for small sample sizes when effect size is large (see Winters 2013)
t.test(AE_H_1_mm, AE_H_1_xgb, paired = T)
t.test(AE_H_6_mm, AE_H_6_xgb, paired = T)
t.test(AE_H_13_mm, AE_H_13_xgb, paired = T)

# medium group ----
AE_M_1_xgb <- perf_wks %>% filter(ARAT_base == "M" & time == "Week 1" & mod == "XGB") %>% pull(AE)
AE_M_6_xgb <- perf_wks %>% filter(ARAT_base == "M" & time == "Week 6" & mod == "XGB") %>% pull(AE)
AE_M_13_xgb <- perf_wks %>% filter(ARAT_base == "M" & time == "Week 13" & mod == "XGB") %>% pull(AE)

AE_M_1_mm <- perf_wks %>% filter(ARAT_base == "M" & time == "Week 1" & mod == "MM") %>% pull(AE)
AE_M_6_mm <- perf_wks %>% filter(ARAT_base == "M" & time == "Week 6" & mod == "MM") %>% pull(AE)
AE_M_13_mm <- perf_wks %>% filter(ARAT_base == "M" & time == "Week 13" & mod == "MM") %>% pull(AE)

# distributions
ggplot(filter(perf_wks, ARAT_base == "M" & time == "Week 1"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 1") +
  scale_fill_viridis(discrete=T) +
  fancy

ggplot(filter(perf_wks, ARAT_base == "M" & time == "Week 6"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 6") +
  scale_fill_viridis(discrete=T) +
  fancy

ggplot(filter(perf_wks, ARAT_base == "M" & time == "Week 13"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 13") +
  scale_fill_viridis(discrete=T) +
  fancy


wilcox.test(AE_M_1_mm, AE_M_1_xgb, paired = T)
wilcox.test(AE_M_6_mm, AE_M_6_xgb, paired = T)
wilcox.test(AE_M_13_mm, AE_M_13_xgb, paired = T)

# low group ----
AE_L_1_xgb <- perf_wks %>% filter(ARAT_base == "L" & time == "Week 1" & mod == "XGB") %>% pull(AE)
AE_L_6_xgb <- perf_wks %>% filter(ARAT_base == "L" & time == "Week 6" & mod == "XGB") %>% pull(AE)
AE_L_13_xgb <- perf_wks %>% filter(ARAT_base == "L" & time == "Week 13" & mod == "XGB") %>% pull(AE)

AE_L_1_mm <- perf_wks %>% filter(ARAT_base == "L" & time == "Week 1" & mod == "MM") %>% pull(AE)
AE_L_6_mm <- perf_wks %>% filter(ARAT_base == "L" & time == "Week 6" & mod == "MM") %>% pull(AE)
AE_L_13_mm <- perf_wks %>% filter(ARAT_base == "L" & time == "Week 13" & mod == "MM") %>% pull(AE)

# distributions
ggplot(filter(perf_wks, ARAT_base == "L" & time == "Week 1"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 1") +
  scale_fill_viridis(discrete=T) +
  fancy

ggplot(filter(perf_wks, ARAT_base == "L" & time == "Week 6"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 6") +
  scale_fill_viridis(discrete=T) +
  fancy

ggplot(filter(perf_wks, ARAT_base == "L" & time == "Week 13"),aes(x = AE, fill = factor(mod))) +
  geom_density(alpha = 0.5) +
  labs(x = "AE",
       y = "Density",
       title = "Week 13") +
  scale_fill_viridis(discrete=T) +
  fancy

wilcox.test(AE_L_1_mm, AE_L_1_xgb, paired = T)
wilcox.test(AE_L_6_mm, AE_L_6_xgb, paired = T)
wilcox.test(AE_L_13_mm, AE_L_13_xgb, paired = T)