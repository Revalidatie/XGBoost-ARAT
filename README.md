# XGBoost-ARAT Research Repository

We developed an XGBoost model that predicts patient-specific upper-extremity capacity at 6 months as measured with the ARAT. Serial measurements of up to 5 months of ARAT score, finger extension, shoulder abduction, and their time of measurement were included as predictors. Best fit was found using grid search with stratified 5-fold, 5-repeat cross-validation. We estimated prediction intervals by resampling with replacement 300 bootstrap datasets from the train set, training our model on each bootstrap sample, and calculating the prediction errors on the left-out samples. The variance in the standard errors over the bootstrapped samples provides an estimate of the prediction uncertainty. As a final prediction, we took the median of the bootstrapped predictions.

The performance of the bagged ensemble of XGBoost models is compared with a mixed model introduced by [Selles et al., 2021](https://jnnp.bmj.com/content/92/6/574).

The most important functions are:

-   **main** - main script, contains ETL, data preproc, exploration and modelling. Start here.
-   **predict.XGB** - bagging function, fits bootstrap XGB models and calculates prediction intervals.
-   **predict.multi.MM** - function that does individual point predictions with the fitted mixed model.
-   **my.rep.folds** - creates CV folds stratified on patient level (required for cross-validation).
-   **pred.viz.\*** - different visualisation functions for the prediction output of the Mixed Effects and XGB model.

Inputs, outputs and purpose off all functions are described in their docstrings.

### Visuals

![XGB and Mixed Model performance comparison](/Output/figure4.png) ![XGB prediction visualisation](/Output/figure6.png)

### Shiny application

We created an [online tool](https://emcbiostatistics.shinyapps.io/XGBoostARATpredictions/) for real-time upper limb recovery predictions. Clinicians and researchers can upload a file with ARAT, shoulder abduction, and finger extension measurements for a stroke patient to receive automatic predictions of upper limb recovery at 6 months, along with 80% prediction intervals.

### To do

-   Add licensing information
-   Make repo public
-   Add link to scientific publication

### Authors and acknowledgment

By G.J. van der Gun, ICAI Stroke Lab.

This work was supported by the Innovation Center of Artificial Intelligence and partially based on [Selles et al., 2021](https://jnnp.bmj.com/content/92/6/574)

### License

Public domain

### Project status

Under development
