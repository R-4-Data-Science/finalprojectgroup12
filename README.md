# finalprojectgroup12

This R package implements a multi-path forward selection framework for model building, 
stability estimation, and selection of plausible models based on AIC. The package 
contains functions for building model paths, estimating variable stability via resampling, 
selecting plausible models, and computing confusion metrics.

## Installation

You can install the development version directly from GitHub:

### Using devtools:
```r
devtools::install_github("R-4-Data-Science/finalprojectgroup12")
```

### Using remotes:
```r
remotes::install_github("R-4-Data-Science/finalprojectgroup12")
```

## Functions

 1. build_paths(): build multi-path forward selection
 2. stability(): estimate variable stability via resampling
 3. plausible_models(): select plausible models based on AIC and stability
 4. confusion_metrics(): compute confusion metrics for model evaluation

## Example

### Linear Regression (Gaussian)
```r
set.seed(123)
n <- 100
p <- 5
X <- data.frame(matrix(rnorm(n*p), n, p))
colnames(X) <- paste0("X", 1:p)
y_gaussian <- 2 + 0.5*X$X1 + 0.3*X$X2 + rnorm(n)

# Build model paths
forest_gauss <- build_paths(X, y_gaussian, family = "gaussian", K = 5)

# Estimate variable stability
stable_gauss <- stability(X, y_gaussian, B = 30, family = "gaussian")
pi_gauss <- stable_gauss$pi

# Select plausible models
plausible_gauss <- plausible_models(forest_gauss, pi_gauss, Delta = 2, tau = 0.6)
plausible_gauss

# Visualize variable stability
barplot(pi_gauss, main = "Variable Stability (Linear)", col = "steelblue", las = 2)
```

### Logistic Regression (Binomial)
```r
set.seed(123)
n <- 100
p <- 5
X <- data.frame(matrix(rnorm(n*p), n, p))
colnames(X) <- paste0("X", 1:p)
y_binomial <- rbinom(n, 1, 0.5)

# Build model paths
forest_bin <- build_paths(X, y_binomial, family = "binomial", K = 5)

# Estimate variable stability
stable_bin <- stability(X, y_binomial, B = 30, family = "binomial")
pi_bin <- stable_bin$pi

# Select plausible models
plausible_bin <- plausible_models(forest_bin, pi_bin, Delta = 2, tau = 0.6)
plausible_bin

# Visualize variable stability
barplot(pi_bin, main = "Variable Stability (Logistic)", col = "salmon", las = 2)
```

## Shiny Demo

The package includes an interactive Shiny application to explore the multi-path selection workflow:

```r
# Run the Shiny app from the installed package
shiny::runApp(system.file("shiny", package = "finalprojectgroup12"))
```
