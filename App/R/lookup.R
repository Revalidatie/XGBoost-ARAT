lookup <- function(cur_obs){
  
  index <- which(XGBlookup_pat$Days == cur_obs$Days & 
                   XGBlookup_pat$ARAT == cur_obs$ARAT & 
                   XGBlookup_pat$SA == cur_obs$SA & 
                   XGBlookup_pat$FE == cur_obs$FE)
  
  preds <- XGBlookup_preds[index,]
  ystar <- XGBlookup_ystar[,index]
  
  pred_dat <- list(preds,ystar)
  
  return(pred_dat)
}