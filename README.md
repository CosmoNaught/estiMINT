# estiMINT

**estiMINT** (pronounced *estimate*) provides fast, transparent machine-learning models for translating routine malaria surveillance data and intervention coverage into:
- **Initial entomological inoculation rate** (EIR₀) estimates
- **Annual case incidence** predictions (cases per 1000 population)

It grew out of the **MINTverse** modelling pipelines and is designed to be drop-in friendly for anyone working with *malariasimulation* style simulation outputs.

---

## Table of Contents
- [Why estiMINT?](#why-estimint)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Model Architecture](#model-architecture)
- [Performance Metrics](#performance-metrics)
- [Advanced Usage](#advanced-usage)
- [Model Management](#model-management)
- [Training Custom Models](#training-custom-models)
- [API Reference](#api-reference)
- [Contributing](#contributing)

---

## Why estiMINT?

While simple empirical curves work well at the national scale, local planning often needs site-level estimates that incorporate:

- **Non-linear combinations** of covariates (seasonality, net/IRS usage, biting behaviour)
- **Temporal projections** – predict case incidence for future years under different intervention scenarios
- **Low latency** – you don't want to wait days for a full IBMs run just to initialise a simulation grid
- **Re-trainability** – when new simulation runs land, you should be able to fine-tune models with one command
- **Handling of zero-inflated data** – real-world malaria data often contains many zeros, especially in low-transmission settings

**estiMINT** wraps gradient-boosted trees (*xgboost*) and random forests (*ranger*) behind a consistent API, ships reasonably good pretrained ensembles for both EIR and case estimation, and lets power-users swap in their own learners.

---

## Installation

```r
# Install from GitHub
install.packages("devtools")
devtools::install_github("CosmoNaught/estiMINT")

# Dependencies (installed automatically):
# - xgboost (>= 1.6.0)
# - ranger (>= 0.14.0)
# - duckdb (>= 0.8.0)
# - data.table (>= 1.14.0)
# - future (>= 1.30.0)
# - progressr (>= 0.13.0)
# - piggyback (>= 0.1.5) - for model management

# Requires R >= 4.2
```

---

## Quick Start

### Estimating Initial EIR

```r
library(estiMINT)

# Create input data
new_data <- data.frame(
  prevalence = c(0.1, 0.2),
  dn0_use    = c(0.3, 0.4),  # dormant (non-feeding) mosquito fraction
  Q0         = c(0.7, 0.8),  # human blood index
  phi_bednets= c(0.5, 0.6),
  seasonal   = c(0, 1),
  routine    = c(0, 1),
  itn_use    = c(0.4, 0.5),
  irs_use    = c(0.2, 0.3)
)

# Load pretrained models (automatically downloads on first use)
pretrained <- load_pretrained_eir_models()

# IMPORTANT: Extract models from wrappers if necessary
xgb_model <- if (is.list(pretrained$xgboost) && "model" %in% names(pretrained$xgboost)) {
  pretrained$xgboost$model
} else {
  pretrained$xgboost
}

rf_model <- if (is.list(pretrained$rf_model) && "model" %in% names(pretrained$rf_model)) {
  pretrained$rf_model$model
} else {
  pretrained$rf_model
}

# Get predictions
xgb_eir <- predict_initial_eir(xgb_model, new_data, pretrained$feature_cols)
rf_eir  <- predict_initial_eir(rf_model, new_data, pretrained$feature_cols)

# Calculate ensemble (average of both models)
ensemble_eir <- (xgb_eir + rf_eir) / 2

print(round(ensemble_eir, 3))
```

### Estimating Annual Cases per 1000

```r
library(estiMINT)
library(dplyr)
library(tidyr)

# Create scenarios with current and future intervention coverage
scenarios <- data.frame(
  eir        = c(5.2, 35.8, 180.5),  # Can use output from predict_initial_eir()
  dn0_use    = c(0.15, 0.35, 0.55),
  dn0_future = c(0.20, 0.45, 0.65),  # future mosquito behaviour
  Q0         = c(0.65, 0.75, 0.85),
  phi_bednets= c(0.45, 0.65, 0.75),
  seasonal   = c(0, 1, 1),
  routine    = c(0, 0, 1),
  itn_use    = c(0.25, 0.55, 0.85),
  irs_use    = c(0.10, 0.35, 0.70),
  itn_future = c(0.30, 0.60, 0.90),  # future ITN coverage
  irs_future = c(0.15, 0.40, 0.75),  # future IRS coverage
  lsm        = c(0.05, 0.45, 0.85),  # larval source management
  year       = 3                      # prediction year (0-5, maps to simulation years 6-12)
)

# Load pretrained models and predict
pretrained_cases <- load_pretrained_case_models()

# IMPORTANT: Extract models from wrappers if necessary
xgb_model <- if (is.list(pretrained_cases$xgboost_cases) && "model" %in% names(pretrained_cases$xgboost_cases)) {
  pretrained_cases$xgboost_cases$model
} else {
  pretrained_cases$xgboost_cases
}

rf_model <- if (is.list(pretrained_cases$rf_cases) && "model" %in% names(pretrained_cases$rf_cases)) {
  pretrained_cases$rf_cases$model
} else {
  pretrained_cases$rf_cases
}

# Get predictions
cases_xgb <- predict_annual_cases(xgb_model, scenarios, pretrained_cases$feature_cols)
cases_rf  <- predict_annual_cases(rf_model, scenarios, pretrained_cases$feature_cols)

# Calculate ensemble
cases_ensemble <- (cases_xgb + cases_rf) / 2

print(data.frame(eir = scenarios$eir, cases_per_1000 = round(cases_ensemble, 2)))
```

---

## Model Architecture

### Four Model Types

estiMINT provides **four distinct models** with different methodological approaches:

#### 1. **EIR XGBoost Model**
- **Transformation**: Log₁₀(EIR + 1)
- **Objective**: Squared error regression
- **Best for**: Wide range of EIR values with good handling of extreme values
- **Performance**: RMSE = 35.08, R² = 0.903, Correlation = 0.951

#### 2. **EIR Random Forest Model**
- **Transformation**: Log₁₀(EIR + 1)
- **Trees**: 1000-2000 with optimized mtry and node size
- **Best for**: Robust predictions with natural uncertainty quantification
- **Performance**: RMSE = 36.85, R² = 0.893, Correlation = 0.947

#### 3. **Case XGBoost Model** (Advanced)
- **Distribution**: Tweedie (handles zero-inflation naturally)
- **Objective**: `reg:tweedie` with variance power ~1.15
- **Dynamic Weighting**: Adaptive weights based on case rarity and importance
  - Base weights: 1x for median cases, up to 128x for extreme cases
  - Year-adjusted weights for temporal patterns
  - Extra boost for rare high-case scenarios
- **Best for**: Zero-inflated data with many low/zero values
- **Performance**: RMSE = 0.265, R² = 0.926, Correlation = 0.963

#### 4. **Case Random Forest Model** (Advanced)
- **Transformation**: Square root (better than log for near-zero values)
- **Dynamic Weighting**: Same adaptive system as XGBoost
- **Trees**: 1000-2000 with Latin Hypercube sampling for hyperparameters
- **Best for**: Robust predictions with natural handling of outliers
- **Performance**: RMSE = 0.277, R² = 0.919, Correlation = 0.959

### Key Methodological Differences

| Aspect | EIR Models | Case Models |
|--------|------------|-------------|
| **Data Distribution** | Continuous, positive | Zero-inflated, highly skewed |
| **Transformation** | Simple log₁₀ | Tweedie/Square root |
| **Weighting** | Uniform | Dynamic, case-based |
| **Hyperparameter Tuning** | Grid search | Latin Hypercube Sampling |
| **Evaluation** | Standard metrics | Stratified metrics by quantile |
| **Zero Handling** | Add 1 before log | Native (Tweedie) or threshold |

---

## Performance Metrics

### Understanding the Metrics

Our models report comprehensive performance metrics:

#### Standard Metrics
- **RMSE** (Root Mean Square Error): Average prediction error magnitude
- **MAE** (Mean Absolute Error): Average absolute difference
- **R²** (Coefficient of Determination): Proportion of variance explained (0-1, higher is better)
- **MAPE** (Mean Absolute Percentage Error): Percentage error (useful for relative comparisons)
- **Correlation**: Linear relationship strength between predictions and truth

#### Stratified Metrics (Case Models Only)

The case models include **stratified evaluation** to ensure good performance across the entire range:

```
Quantile Range | Cases/1000 | RMSE   | MAE    | N samples
---------------|------------|--------|--------|----------
0-50%          | [0, 0.24]  | 0.326  | 0.033  | 25,815
50-75%         | [0.24,1.04]| 0.118  | 0.079  | 12,907
75-90%         | [1.04,1.95]| 0.155  | 0.098  | 7,745
90-95%         | [1.95,2.24]| 0.121  | 0.066  | 2,581
95-100%        | [2.24,16.7]| 0.436  | 0.251  | 2,582
```

**High-Case Metrics** (top 5%):
- **RMSE_High**: Error for extreme cases
- **R²_High**: Variance explained for high transmission
- **Bias_High**: Systematic over/under-estimation (negative = underestimation)

### Current Performance

#### EIR Models (Test Set)
```
Model         | RMSE  | MAE   | R²    | Correlation
--------------|-------|-------|-------|------------
XGBoost       | 35.08 | 18.10 | 0.903 | 0.951
Random Forest | 36.85 | 17.81 | 0.893 | 0.947
```

#### Case Models (Test Set with Stratification)
```
Model         | RMSE  | MAE   | R²    | Correlation | High-Case Bias
--------------|-------|-------|-------|-------------|---------------
XGBoost       | 0.265 | 0.067 | 0.926 | 0.963       | -0.129
Random Forest | 0.277 | 0.079 | 0.919 | 0.959       | -0.193
```

---

## Advanced Usage

### Three Ways to Load Models

estiMINT supports three model loading strategies:

#### 1. From Pretrained Models (Recommended for Most Users)
```r
# Automatically downloads models on first use
pretrained_cases <- load_pretrained_case_models()

# IMPORTANT: Extract models from wrappers
xgb_model <- if (is.list(pretrained_cases$xgboost_cases) && "model" %in% names(pretrained_cases$xgboost_cases)) {
  pretrained_cases$xgboost_cases$model
} else {
  pretrained_cases$xgboost_cases
}

rf_model <- if (is.list(pretrained_cases$rf_cases) && "model" %in% names(pretrained_cases$rf_cases)) {
  pretrained_cases$rf_cases$model
} else {
  pretrained_cases$rf_cases
}

feature_cols <- pretrained_cases$feature_cols

# Make predictions
predictions <- predict_annual_cases(xgb_model, new_data, feature_cols)
```

#### 2. From Saved Local Files (After Custom Training)
```r
# After training your own models
model_dir <- "case_model_files"

# Load wrapped models (contain model + metadata)
xgb_wrapper <- readRDS(file.path(model_dir, "xgb_cases_model.rds"))
rf_wrapper <- readRDS(file.path(model_dir, "rf_cases_model.rds"))
feature_cols <- readRDS(file.path(model_dir, "case_feature_columns.rds"))

# Extract model objects from wrappers
xgb_model <- if (is.list(xgb_wrapper) && "model" %in% names(xgb_wrapper)) {
  xgb_wrapper$model
} else {
  xgb_wrapper
}

rf_model <- if (is.list(rf_wrapper) && "model" %in% names(rf_wrapper)) {
  rf_wrapper$model
} else {
  rf_wrapper
}

# Make predictions
predictions <- predict_annual_cases(xgb_model, new_data, feature_cols)
```

#### 3. From Environment (During Active Development)
```r
# Immediately after training
results <- build_case_models(db_path = "simulation.duckdb", ...)

# Use models directly from results (already unwrapped)
predictions <- predict_annual_cases(
  results$models$xgboost_cases,
  new_data,
  results$feature_cols
)
```

### Working with Multi-Year Projections

```r
library(tidyr)
library(dplyr)

# Create scenarios for multiple years
scenarios <- data.frame(
  scenario = 1:2,
  eir = c(5.2, 180.5),
  dn0_use = c(0.5, 0.5),
  dn0_future = c(0.80, 0.40),
  Q0 = c(0.5, 0.5),
  phi_bednets = c(0.5, 0.5),
  seasonal = c(0, 0),
  routine = c(0, 0),
  itn_use = c(0.5, 0.5),
  irs_use = c(0.5, 0.2),
  itn_future = c(0.80, 0.10),
  irs_future = c(0.8, 0.1),
  lsm = c(0.8, 0)
)

years <- 2:5  # Maps to simulation years
new_data_cases <- tidyr::crossing(scenarios, year = years) %>%
  dplyr::select(-scenario)

# Load and extract models
pretrained_cases <- load_pretrained_case_models()

xgb_model <- if (is.list(pretrained_cases$xgboost_cases) && "model" %in% names(pretrained_cases$xgboost_cases)) {
  pretrained_cases$xgboost_cases$model
} else {
  pretrained_cases$xgboost_cases
}

rf_model <- if (is.list(pretrained_cases$rf_cases) && "model" %in% names(pretrained_cases$rf_cases)) {
  pretrained_cases$rf_cases$model
} else {
  pretrained_cases$rf_cases
}

# Get predictions for all year-scenario combinations
xgb_predictions <- predict_annual_cases(xgb_model, new_data_cases, pretrained_cases$feature_cols)
rf_predictions <- predict_annual_cases(rf_model, new_data_cases, pretrained_cases$feature_cols)

# Calculate ensemble and add to data
new_data_cases$xgb_cases_per_1000 <- xgb_predictions
new_data_cases$rf_cases_per_1000 <- rf_predictions
new_data_cases$ensemble_cases_per_1000 <- (xgb_predictions + rf_predictions) / 2

# Summarize by year
results <- new_data_cases %>%
  mutate(
    scenario = rep(1:2, each = length(years)),
    year_label = case_when(
      year == 2 ~ "Year 2-3 (timesteps 2920-3285)",
      year == 3 ~ "Year 3-4 (timesteps 3285-3650)",
      year == 4 ~ "Year 4-5 (timesteps 3650-4015)",
      year == 5 ~ "Year 5-6 (timesteps 4015-4380)",
      TRUE ~ as.character(year)
    )
  ) %>%
  select(scenario, year, year_label, eir,
         xgb_cases_per_1000,
         rf_cases_per_1000,
         ensemble_cases_per_1000) %>%
  arrange(scenario, year)

print(results)
```

### Working with Prediction Intervals

```r
# Get predictions with uncertainty intervals
# Note: You may need to extract the model first as shown above
predictions_with_intervals <- predict_annual_cases(
  model_obj = xgb_model,  # Use extracted model, not wrapper
  new_data = scenarios,
  feature_cols = pretrained_cases$feature_cols,
  return_intervals = TRUE,
  interval_level = 0.95
)

# Returns data frame with:
# - prediction: point estimate
# - lower: 95% CI lower bound
# - upper: 95% CI upper bound
```

---

## Model Management

### Versioning and Updates

estiMINT uses a sophisticated model management system with GitHub releases:

#### Model Tags
Each model release has a unique tag: `models-YYYYMMDD-HHMMSS-hash`
- Timestamp ensures chronological ordering
- Hash (12 chars) ensures content integrity
- Tags are immutable once published

#### Checksum Verification
```
# Models ship with SHA256 checksums
models-checksums.csv:
  path                           | md5                              | size_B
  -------------------------------|----------------------------------|-------
  eir_model/xgboost_model.rds   | a3f8e2c1b9d4567890abcdef12345678 | 245632
  case_model/rf_cases_model.rds | b4e9f3d2c8a5678901bcdefg23456789 | 512048
```

#### Cache Management
```r
# Models are cached in user directory
# Default: ~/.cache/estiMINT/ (Linux/Mac) or %LOCALAPPDATA%/estiMINT/Cache (Windows)

# Clear cache for current model version
.purge_models_cache(all = FALSE)

# Clear entire model cache
.purge_models_cache(all = TRUE)

# Check cache location
.model_cache_dir()
```

### Publishing New Models (Maintainers Only)

```r
# After training new models in inst/extdata/
.publish_models(tag = NULL)  # Auto-generates unique tag

# This will:
# 1. Create a versioned ZIP of model files
# 2. Generate checksums for verification
# 3. Upload to GitHub release
# 4. Update inst/models-tag.txt and inst/models-checksums.csv
# 5. Prompt to commit and push changes
```

---

## Training Custom Models

### Complete Training Pipeline

```r
library(estiMINT)

# Train EIR models with full pipeline
eir_results <- build_eir_models(
  db_path = "simulation_database.duckdb",
  
  # Data management
  data_dir = "eir_data",           # Where to export training data
  export_data = TRUE,              # Save data for reproducibility
  
  # Model output
  model_dir = "eir_model_files",   # Model artifacts
  plot_dir = "eir_training_plots", # Diagnostic plots
  plotting = TRUE,                 # Generate plots
  
  # Data sampling
  param_limit = 8192,              # Max parameter sets to use
  sim_limit = 8,                   # Simulations per parameter
  
  # Training options
  tune_hyperparams = TRUE          # Use Latin Hypercube for tuning
)

# Train case models with advanced features
case_results <- build_case_models(
  db_path = "simulation_database.duckdb",
  
  # Data management
  data_dir = "case_data",
  export_data = TRUE,
  
  # Model output
  model_dir = "case_model_files",
  plot_dir = "case_training_plots",
  plotting = TRUE,
  
  # Data sampling
  param_limit = 8192,
  sim_limit = 8,
  y_keep = 0:5,                    # Years to include (0-5 = timesteps 2190-4380)
  
  # Advanced options
  tune_hyperparams = TRUE,
  use_case_weights = TRUE,         # Dynamic weighting for imbalanced data
  weight_power = 0.75,             # Power for weight calculation
  stratified_eval = TRUE           # Compute stratified metrics
)

# Models are automatically saved as:
# - {model_dir}/xgboost_model.rds or xgb_cases_model.rds
# - {model_dir}/rf_model.rds or rf_cases_model.rds
# - {model_dir}/feature_columns.rds or case_feature_columns.rds
# - {model_dir}/model_metrics.csv or case_model_metrics.csv
```

### Training Data Requirements

Your DuckDB database should contain a `simulation_results` table with:

#### Required Columns
```sql
-- Core identifiers
parameter_index    INTEGER  -- Parameter set ID
simulation_index   INTEGER  -- Simulation replicate ID
timesteps          INTEGER  -- Time point (365-day units)

-- EIR-related
eir               DOUBLE   -- Entomological inoculation rate

-- Prevalence data (for EIR models)
n_detect_lm_0_1825 INTEGER -- Detected infections (age 0-5)
n_age_0_1825       INTEGER -- Population (age 0-5)

-- Case data (for case models)
n_inc_clinical_0_36500 INTEGER -- Clinical cases (all ages)
n_age_0_36500          INTEGER -- Population (all ages)

-- Intervention parameters
dn0_use, dn0_future    DOUBLE -- Mosquito behaviour
Q0                     DOUBLE -- Human blood index
phi_bednets            DOUBLE -- Bed biting proportion
seasonal               BINARY -- Seasonality flag
routine                BINARY -- Routine immunisation
itn_use, itn_future    DOUBLE -- ITN coverage
irs_use, irs_future    DOUBLE -- IRS coverage
lsm                    DOUBLE -- Larval source management
```

### Hyperparameter Tuning Details

#### Latin Hypercube Sampling (Cases)
```r
# XGBoost explores:
- max_depth: [6, 12]
- learning_rate: [0.01, 0.04]
- min_child_weight: [1, 5]
- tweedie_variance_power: [1.05, 1.25]
- subsample: [0.75, 0.95]
- colsample_bytree: [0.75, 0.95]

# Random Forest explores:
- num_trees: [1000, 2000]
- mtry_fraction: [0.4, 0.7]
- min_node_size: [1, 8]
- max_depth: [16, 25]
```

### Diagnostic Plots Generated

Training automatically generates comprehensive diagnostic plots:

1. **predictions_scatter_combined.png** - Observed vs predicted
2. **feature_importance_combined.png** - Variable importance
3. **predictions_scatter_by_year.png** - Performance by year (cases only)
4. **stratified_performance.png** - Performance by case quantile (cases only)
5. **predictions_scatter_by_{covariate}.png** - Performance by covariate bins
6. **case_error_distribution.png** - Error analysis (cases only)

---

## API Reference

### Core Functions

| Function | Purpose | Returns |
|----------|---------|---------|
| `load_pretrained_eir_models()` | Load package EIR models | List with models + features |
| `load_pretrained_case_models()` | Load package case models | List with models + features |
| `predict_initial_eir()` | Predict EIR from conditions | Numeric vector |
| `predict_annual_cases()` | Predict cases/1000 | Numeric vector or data.frame |
| `build_eir_models()` | Train custom EIR models | List with models + metrics |
| `build_case_models()` | Train custom case models | List with models + metrics |

**Note**: When using `load_pretrained_*` functions, the returned models may be wrapped. Always extract the actual model using the pattern shown in the examples.

### Utility Functions

| Function | Purpose |
|----------|---------|
| `get_threads()` | Get optimal thread count for parallel processing |
| `create_default_bin_edges()` | Generate standard bins for covariate analysis |
| `compare_case_predictions()` | Compare multiple models on same data |
| `evaluate_model()` | Compute comprehensive metrics including stratified |
| `plot_stratified_performance()` | Visualize performance across case ranges |

### Data Processing Functions

| Function | Purpose |
|----------|---------|
| `load_sim_data()` | Load EIR training data from DuckDB |
| `load_case_data()` | Load case training data with year mapping |
| `clean_features()` | Prepare feature matrix for prediction |
| `validate_data()` | Check data integrity before training |

---

## Year Mapping

The case models use a specific timestep-to-year mapping:

| Year Index | Timestep Range | Calendar Interpretation |
|------------|----------------|------------------------|
| 0 | 2190-2555 | Baseline year |
| 1 | 2555-2920 | Year 1 post-intervention |
| 2 | 2920-3285 | Year 2 post-intervention |
| 3 | 3285-3650 | Year 3 post-intervention |
| 4 | 3650-4015 | Year 4 post-intervention |
| 5 | 4015-4380 | Year 5 post-intervention |

---

## Troubleshooting

### Common Issues

#### Models Not Loading Correctly
```r
# If you get errors about model structure, ensure you extract from wrapper:
pretrained <- load_pretrained_eir_models()

# Check structure
str(pretrained$xgboost, max.level = 1)

# Extract model if wrapped
xgb_model <- if (is.list(pretrained$xgboost) && "model" %in% names(pretrained$xgboost)) {
  pretrained$xgboost$model
} else {
  pretrained$xgboost
}
```

#### Models Not Downloading
```r
# Check internet connection and GitHub access
# Clear cache and retry
.purge_models_cache(all = TRUE)
pretrained <- load_pretrained_eir_models()
```

#### Out of Memory During Training
```r
# Reduce parallel workers
options(mc.cores = 2)

# Reduce data size
build_case_models(
  param_limit = 1000,  # Fewer parameter sets
  sim_limit = 4        # Fewer simulations per parameter
)
```

#### Prediction Errors
```r
# Ensure all required columns are present
required_cols <- pretrained$feature_cols
missing <- setdiff(required_cols, names(your_data))
if (length(missing) > 0) {
  stop("Missing columns: ", paste(missing, collapse = ", "))
}

# Ensure model is extracted from wrapper if needed
if (is.list(model_object) && "model" %in% names(model_object)) {
  model_object <- model_object$model
}
```

---

## Contributing

We welcome contributions! Please:

1. **Open an issue first** for major changes
2. **Follow the existing code style**
3. **Add tests** for new functionality
4. **Update documentation** including this README
5. **Ensure all tests pass** before submitting PR

### Development Setup
```r
# Clone repository
git clone https://github.com/CosmoNaught/estiMINT.git

# Install development dependencies
devtools::install_deps(dependencies = TRUE)

# Run tests
devtools::test()

# Build documentation
devtools::document()
```

---

---

## License

estiMINT is released under the MIT License – see [`LICENSE`](LICENSE) for details.

---

## Acknowledgments

estiMINT builds upon the broader MINTverse ecosystem and benefits from:
- The `malariasimulation` package for generating training data
- The XGBoost and ranger communities for excellent ML implementations
- DuckDB for efficient data processing

---

*For questions, bug reports, or feature requests, please [open an issue](https://github.com/CosmoNaught/estiMINT/issues).*