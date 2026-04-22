# High-Dimensional Model-Assisted Estimation

## Overview
This repository contains the simulation code and report for a project investigating the efficiency of model-assisted estimators in survey sampling under high-dimensional settings. Specifically, it explores scenarios where the number of auxiliary variables ($p$) is comparable to the sample size ($n$), varying the dimension ratio $p/n$ from $0$ to $2$.

## Project Details
The study compares the baseline design-based Horvitz-Thompson estimator against several model-assisted estimators that use different statistical methods to predict the study variable. 

**Sampling Designs:**
* Simple Random Sampling Without Replacement (SRSWOR)
* Bernoulli Sampling

**Estimation Methods Evaluated:**
* Horvitz-Thompson (Baseline)
* Ordinary Least Squares (OLS)
* LASSO
* Elastic Net
* Random Forest (RF)
* Gradient Boosting Machine (GBM)

## Simulation Setup
* **Population Size:** $N = 500$
* **Sample Size:** $n = 150$ (or an expected size of 150 for Bernoulli sampling).
* **Covariates:** The data-generating process mixes true relevant covariates (Standard Gaussian) and heavy-tailed noise covariates (Student's $t$ with 3 degrees of freedom) to challenge the estimators.
* **Evaluation:** Models are evaluated based on their Mean Squared Error (MSE) across 200 independent Monte Carlo repetitions.
