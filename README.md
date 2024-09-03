# XGBoost-ARAT Research Repository
We developed an XGBoost  model that predicts patient-specific upper-extremity capacity at 6 months as measured with the ARAT. Serial measurements of up to 5 months of ARAT score, finger extension, shoulder abduction, and their time of measurement were included as predictors.  Best fit was found using grid search with stratified 5-fold, 5-repeat cross-validation. We estimated prediction intervals by resampling with replacement 300 bootstrap datasets from the train set, training our model on each bootstrap sample, and calculating the prediction errors on the left-out samples. The variance in the standard errors over the bootstrapped samples provides an estimate of the prediction uncertainty. As a final prediction, we took the median of the bootstrapped predictions.

The performance of the bagged ensemble of XGBoost models is compared with a mixed model introduced by [Selles et al., 2021](https://jnnp.bmj.com/content/92/6/574). 

The most important functions are:

* <em>XGB</em> - main script, contains ETL, data preproc, exploration and modelling. Start here.
* <em>predict.XGB</em> - bagging function, fits bootstrap XGB models and calculates prediction intervals.
* <em>predict.multi.MM</em> - function that does individual point predictions with the fitted mixed model
* <em>my.rep.folds</em> - creates CV folds stratified on patient level

Inputs, outputs and purpose off all functions are described in their docstrings. 

### Visuals
<em>Work in progress</em>

### To do
* Clean-up repo: delete obsolete files and add/update docstrings where necessary  
* Update ReadMe with visuals, proper function descriptions, reference to shiny
* Add licensing information an acknowledgements
* Make repo public
* Add link to scientific publication


### Authors and acknowledgment
By G.J. van der Gun <br>
Partially based on [Selles et al., 2021](https://jnnp.bmj.com/content/92/6/574)

### License
Public domain

### Project status
Under development
