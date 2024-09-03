check_data <- function(pat_dat) {
  # function that checks the required data format. Returns TRUE if data is formatted correctly.
  required_headers <- c("Days", "ARAT", "SA", "FE")
  
  # Check headers
  if (!all(required_headers %in% colnames(pat_dat))) {
    return(FALSE)
  }
  
  # Check Days
  if (!all(pat_dat$Days >= 0 & pat_dat$Days <= 194 & is.integer(pat_dat$Days))) {
    return(FALSE)
  }
  
  # Check ARAT
  if (!all(pat_dat$ARAT >= 0 & pat_dat$ARAT <= 57 & is.integer(pat_dat$ARAT))) {
    return(FALSE)
  }
  
  # Check SA
  if (!all(pat_dat$SA %in% c(0, 9, 14, 19, 25, 33))) {
    return(FALSE)
  }
  
  # Check FE
  if (!all(pat_dat$FE %in% c("none", "partial", "full"))) {
    return(FALSE)
  }
  
  return(TRUE)
}