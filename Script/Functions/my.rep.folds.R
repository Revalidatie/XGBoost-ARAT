my.rep.folds <- function (dat_train, IDvarn, nreps = 5, nfolds = 5, seed = 49789)
  # this function creates repeated folds with subject-wise split for use with caret.
  # Subject-wise split ensures that only unique patients end up in each fold and thus
  # prevent data leakage. Use for repeated measurements data, but also to create repeated
  # folds for single measurements.
  #
  # Arguments:
  # <dat_train>     a data frame containing the train data
  # <IDvarn>        a character vector containing the name of patient ID variable
  # <nreps>         an optional integer specifying the number of repeats
  # <nfolds>        an optional integer specifying the number of folds
  # <iID>           an optional integer specifying the ID col. index
  # <seed>          an optional seed
  
  ## debug
  # dat_train <- cur_train
  # IDvarn <- "Number"
  # nreps <- 5
  # nfolds <- 5
  # seed <- seed

{
  # set seed
  set.seed(seed)
  
  # create new folds for each repetition and convert to a caret-friendly format
  myRepFolds <- 1:nreps %>%
    map(
      function(x) {
        # create folds
        folds <- 1:nfolds
        train_folds <- fold(dat_train, k = length(folds), id_col = IDvarn)
        
        # Convert our folds to a caret-friendly format
        myFolds <- map(folds, ~ which(train_folds$.folds %in% .))
        
        # create fold names
        foldNames <- folds %>%
          map(
            ~ case_when(
              TRUE ~ paste0('Fold', ., '.Rep', x)
            )
          )
        unlist
        
        names(myFolds) <- foldNames
        
        myFolds
      }
    )
  
  myRepFolds <- list_flatten(myRepFolds) # caret requires flat list
  
  return(myRepFolds)
}