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

-   Add link to scientific publication

### Authors and acknowledgment

By G.J. van der Gun, ICAI Stroke Lab.

This work was supported by the Innovation Center of Artificial Intelligence and partially based on [Selles et al., 2021](https://jnnp.bmj.com/content/92/6/574)

### License

Copyright (c) [2024] [ICAI Stroke Lab]

=======================================

“Commons Clause” License Condition v1.0

The Software is provided to you by the Licensor under the License, as defined below, subject to the following condition.

Without limiting other conditions in the License, the grant of rights under the License will not include, and the License does not grant to you, the right to Sell the Software.

**For purposes of the foregoing, “Sell” means practicing any or all of the rights granted to you under the License to provide to third parties, for a fee or other consideration (including without limitation fees for hosting or consulting/ support services related to the Software), a product or service whose value derives, entirely or substantially, from the functionality of the Software. Any license notice or attribution required by the License must also include this Commons Clause License Condition notice.**

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

### Project status

⏳Submitted - Under review
