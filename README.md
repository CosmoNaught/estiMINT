# estiMINT &#x20;

**estiMINT** (pronounced *estimate*) provides fast, transparent machine‑learning models for translating routine malaria surveillance data and intervention coverage into:
- **Initial entomological inoculation rate** (EIR0) estimates
- **Annual case incidence** predictions (cases per 1000 population)

It grew out of the **MINTverse** modelling pipelines and is designed to be drop‑in friendly for anyone working with *malariasimulation* style simulation outputs.

---

## Why estiMINT?

While simple empirical curves work well at the national scale, local planning often needs site‑level estimates that incorporate:

- **Non‑linear combinations** of covariates (seasonality, net/IRS usage, biting behaviour …)
- **Temporal projections** – predict case incidence for future years under different intervention scenarios
- **Low latency** – you don't want to wait days for a full IBMs run just to initialise a simulation grid
- **Re‑trainability** – when new simulation runs land, you should be able to fine‑tune models with one command

**estiMINT** wraps gradient‑boosted trees (*xgboost*) and random forests (*ranger*) behind a consistent API, ships reasonably good pretrained ensembles for both EIR and case estimation, and lets power‑users swap in their own learners.

---

## Installation

```r
# development version from your local repo (or replace with GitHub once public)
install.packages("devtools")
devtools::install("/home/cosmo/net/malaria/Cosmo/packages/estiMINT")

# OR straight from GitHub
# devtools::install_github("your‑org/estiMINT")
```

The package depends on **xgboost**, **ranger**, **duckdb**, and **data.table** (installed automatically) and requires R ≥ 4.2.

---

## Quick start

### Estimating Initial EIR

```r
library(estiMINT)

# Get a prediction immediately with the shipped models
new_data <- data.frame(
  prevalence = c(0.1, 0.2),
  dn0_use    = c(0.3, 0.4),  # dormant (non‑feeding) mosquito fraction
  Q0         = c(0.7, 0.8),  # human blood index
  phi_bednets= c(0.5, 0.6),
  seasonal   = c(0, 1),
  routine    = c(0, 1),
  itn_use    = c(0.4, 0.5),
  irs_use    = c(0.2, 0.3)
)

pretrained <- load_pretrained_eir_models()
results    <- predict_initial_eir(pretrained$ensemble, new_data, pretrained$feature_cols)
print(round(results, 3))
```

### Estimating Annual Cases per 1000

```r
# Create scenarios with current and future intervention coverage
scenarios <- data.frame(
  eir        = c(5.2, 35.8, 180.5),
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
  year       = 4                      # prediction year 3-4
)

# Load pretrained models and predict
pretrained_cases <- load_pretrained_case_models()
cases_xgb <- predict_annual_cases(pretrained_cases$xgboost_cases, scenarios, pretrained_cases$feature_cols)
cases_rf  <- predict_annual_cases(pretrained_cases$rf_cases, scenarios, pretrained_cases$feature_cols)
cases_ensemble <- (cases_xgb + cases_rf) / 2

print(data.frame(scenarios$eir, cases_ensemble))
```

---

## Training Custom Models

```r
# Train EIR models
eir_models <- build_eir_models(
  db_path          = "/my_database.duckdb",
  model_dir        = "eir_model_files",
  plot_dir         = "eir_training_plots",
  plotting         = TRUE,
  param_limit      = 4096,
  sim_limit        = 8,
  tune_hyperparams = TRUE
)

# Train case estimation models
case_models <- build_case_models(
  db_path          = "/my_database.duckdb",
  model_dir        = "case_model_files",
  plot_dir         = "case_training_plots",
  plotting         = TRUE,
  param_limit      = 1024,
  sim_limit        = 8,
  y_keep           = 0:6,         # which years to include in training
  tune_hyperparams = TRUE
)

# Use your freshly trained models
my_eir  <- predict_initial_eir(eir_models$xgboost, new_data, eir_models$feature_cols)
my_cases <- predict_annual_cases(case_models$models$xgboost_cases, scenarios, case_models$feature_cols)
```

---

## Package tour

| Function                         | What it does                                                                                                               | Output                                        | Typical audience                             |
| -------------------------------- | -------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------- | -------------------------------------------- |
| `build_eir_models()`             | Trains models to predict initial EIR from baseline conditions                                                              | List with RF/XGB models + feature columns     | Modellers with fresh simulation runs         |
| `build_case_models()`            | Trains models to predict annual cases per 1000 for years 3-6 under intervention scenarios                                 | List with RF/XGB models + metrics + features  | Planning teams needing temporal projections  |
| `load_pretrained_eir_models()`   | Loads package‑bundled EIR models (≈2 MB)                                                                                  | List with RF/XGB models ready for prediction  | Analysts needing quick EIR estimates         |
| `load_pretrained_case_models()`  | Loads package‑bundled case models (≈3 MB)                                                                                 | List with RF/XGB models ready for prediction  | Analysts needing quick case projections      |
| `predict_initial_eir()`          | Predicts initial EIR from current conditions                                                                              | Numeric vector of EIR values                  | Everyone                                     |
| `predict_annual_cases()`         | Predicts cases per 1000 for specified years under given intervention scenarios                                            | Numeric vector of case rates                  | Everyone                                     |

All data‑frames *must* include the required columns (returned by training / load functions). Extra columns are ignored; missing columns throw an informative error.

---

## Interfaces at a glance

```
      EIR Estimation                              Case Estimation
      ──────────────                              ───────────────
            │                                             │
    ┌───────▼────────┐                          ┌────────▼────────┐
    │ Current state  │                          │ Current + future│
    │   covariates   │                          │  interventions  │
    └───────┬────────┘                          └────────┬────────┘
            │                                             │
    predict_initial_eir()                      predict_annual_cases()
            │                                             │
            ▼                                             ▼
        EIR (t=0)                               Cases/1000 (t=3-6)


                    Model Architecture
                    ─────────────────
         ┌──────────────┐        ┌──────────────┐
         │ build models │        │load pretrained│
         │  (optional)  │        │   (instant)   │
         └──────┬───────┘        └───────┬──────┘
                │                         │
    .rds files  ▼                         ▼  in‑memory
         ┌─────────────┐          ┌──────────────┐
         │ xgboost     │          │ packaged RF  │
         │ ranger RF   │          │ packaged XGB │
         └──────┬──────┘          └───────┬──────┘
                │                          │
                └──────── predict() ───────┘
                              │
                              ▼
                        numeric vector
```

---

## Expected features / column glossary

### For EIR estimation

| Column        | Description (units)                     |
| ------------- | --------------------------------------- |
| `prevalence`  | PfPR2–10 or PfPR0–99 (proportion, 0–1)  |
| `dn0_use`     | Dormant (non‑feeding) mosquito fraction |
| `Q0`          | Baseline human blood index              |
| `phi_bednets` | Proportion of bites taken in bed        |
| `seasonal`    | 1 = simulation includes seasonality     |
| `routine`     | 1 = routine EPI immunisation included   |
| `itn_use`     | ITN usage rate (0–1)                    |
| `irs_use`     | IRS usage rate (0–1)                    |

### For case estimation (includes all EIR columns plus)

| Column        | Description (units)                                |
| ------------- | -------------------------------------------------- |
| `eir`         | Initial EIR (can be from `predict_initial_eir()`) |
| `dn0_future`  | Future dormant mosquito fraction                   |
| `itn_future`  | Future ITN usage rate (0–1)                       |
| `irs_future`  | Future IRS usage rate (0–1)                       |
| `lsm`         | Larval source management coverage (0–1)            |
| `year`        | Prediction year (3, 4, 5, or 6)                   |

---

## Example: Complete workflow

```r
library(estiMINT)
library(dplyr)

# Step 1: Estimate baseline EIR from current prevalence
baseline <- data.frame(
  prevalence = 0.35,
  dn0_use    = 0.25,
  Q0         = 0.70,
  phi_bednets= 0.60,
  seasonal   = 1,
  routine    = 0,
  itn_use    = 0.40,
  irs_use    = 0.15
)

eir_models <- load_pretrained_eir_models()
baseline_eir <- predict_initial_eir(eir_models$ensemble, baseline, eir_models$feature_cols)

# Step 2: Project cases under different intervention scenarios
intervention_scenarios <- data.frame(
  scenario_name = c("Scenario1", "Scenario2", "Scenario3"),
  eir          = baseline_eir,
  dn0_use      = baseline$dn0_use,
  dn0_future   = c(0.25, 0.30, 0.35),  # mosquito adaptation
  Q0           = baseline$Q0,
  phi_bednets  = baseline$phi_bednets,
  seasonal     = baseline$seasonal,
  routine      = baseline$routine,
  itn_use      = baseline$itn_use,
  irs_use      = baseline$irs_use,
  itn_future   = c(0.40, 0.80, 0.75),  # intervention scenarios
  irs_future   = c(0.15, 0.20, 0.60),
  lsm          = c(0.00, 0.00, 0.50)
)

# Generate predictions for years 3-6 (3-4, 4-5, 5-6)
case_models <- load_pretrained_case_models()
results <- list()

for (yr in 3:5) {
  scenarios_yr <- intervention_scenarios
  scenarios_yr$year <- yr
  
  predictions <- predict_annual_cases(
    case_models$ensemble,
    scenarios_yr,
    case_models$feature_cols
  )
  
  results[[paste0("year_", yr)]] <- data.frame(
    scenario = intervention_scenarios$scenario_name,
    cases_per_1000 = predictions
  )
}

# Display results
print(results)
```

---

## File structure

```
estiMINT/
├── R/                       # core functions
├── inst/
│   └── pretrained_models/   # xgboost & rf models for EIR and cases
│       ├── xgboost_model.rds
│       ├── rf_model.rds
│       ├── xgb_cases_model.rds
│       ├── rf_cases_model.rds
│       ├── feature_cols.rds
│       └── case_feature_columns.rds
├── man/                     # R‑oxygen docs
└── vignettes/               # worked examples COMING SOON!
```

---

## Model performance

The pretrained models achieve the following performance on held‑out test data:

- **EIR models**: RMSE < 15 infectious bites/person/year (R² > 0.95)
- **Case models**: RMSE < 25 cases/1000/year (R² > 0.95)

Performance varies by transmission intensity; see vignettes for detailed validation plots.

---

## Contributing

Pull requests and issue reports are very welcome!\
Please open an issue first if you plan large architectural changes.

### Licence

`estiMINT` is released under the MIT License – see [`LICENSE`](LICENSE) for details.

---