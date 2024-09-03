# setup
load("Output/test_xgb.Rdata")
load("Output/XGBlookup.Rdata")
load("Output/preds_1wk.Rdata")
XGBlookup_pat <- XGBlookup[[1]]
XGBlookup_preds <- XGBlookup[[2]]
XGBlookup_ystar <- XGBlookup[[3]]

# define moment post-stroke [Days] when predictions are made (day at which model is applied)
applied_at <- 7*1

# extract measurement(s) from test set to evaluate the XGB and MM on
eval_xgb <- test_xgb %>% 
  group_by(Number) %>%
  filter(Days <= applied_at) %>% 
  slice_max(Days, n=1) %>% # most recent measurement available within the current period
  ungroup()

# init
correlation <- matrix(nrow = nrow(eval_xgb), ncol = 1)

for(i in 1:nrow(eval_xgb)){
  # lookup current patient
  cur_pat <- eval_xgb[i,]
  
  index <- which(XGBlookup_pat$Days == cur_pat$Days & 
                 XGBlookup_pat$ARAT == cur_pat$ARAT & 
                 XGBlookup_pat$SA == cur_pat$SA & 
                 XGBlookup_pat$FE == cur_pat$FE )
  
  preds <- XGBlookup_preds[index,] %>% mutate(Dataset = "Lookup")
  ystar <- data.frame(fARAT = XGBlookup_ystar[,index],
                      Dataset = "Lookup")
  
  # validate
  preds_v <- pred_list[[1]][i,-c(1,5)] %>% mutate(Dataset = "Test")
  ystar_v <- data.frame(fARAT = pred_list[[2]][,i],
                        Dataset = "Test")
  
  val <- rbind(preds, preds_v)
  
  p <- rbind(ystar_v, ystar) %>% ggplot(aes(x = fARAT, fill = factor(Dataset))) +
    geom_density(alpha = 0.5) +
    labs(x = "Future ARAT obs.",
         y = "Density") +
    scale_fill_viridis(discrete=T)
  print(p)
  
  correlation[i] <- cor(as.numeric(preds[,-4]),as.numeric(preds_v[,-4]))
}