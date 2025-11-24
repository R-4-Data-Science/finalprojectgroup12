# finalprojectgroup12

This R package implements a multi-path forward selection framework for model building, 
stability estimation, and selection of plausible models based on AIC. The package 
contains functions for building model paths, estimating variable stability via resampling, 
selecting plausible models, and computing confusion metrics.

## Installation

You can install the package from your local directory using `devtools`:

```{r}
# Install devtools if not installed
install.packages("devtools")

# Install the package from local folder
devtools::install_local("path/to/finalprojectgroup12")
```
## Functions

 1. build_paths(): build multi-path forward selection
 2. stability(): estimate variable stability via resampling
 3. plausible_models(): select plausible models based on AIC and stability
 4. confusion_metrics(): compute confusion metrics for model evaluation

## Example

```{r}
set.seed(123)
n <- 100
p <- 5
X <- data.frame(matrix(rnorm(n*p), n, p))
colnames(X) <- paste0("X", 1:p)
y <- rbinom(n, 1, 0.5)

# Build model paths
forest <- build_paths(X, y, family = "binomial", K = 5)

# Estimate variable stability
stable <- stability(X, y, B = 30, build_args = list(K = 5, family = "binomial"))
pi <- stable$pi

# Select plausible models
plausible <- plausible_models(forest, pi, Delta = 2, tau = 0.4)
plausible

# Visualize variable stability
barplot(pi, main="Variable Stability", col="steelblue", las=2)
```
