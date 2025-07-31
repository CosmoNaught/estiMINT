# estiMINT &#x20;

**estiMINT** (pronounced *estimate*) provides fast, transparent machine‑learning models for translating routine malaria surveillance data and intervention coverage into estimates of the *initial entomological inoculation rate* (EIR0).\
It grew out of the **MINTverse** modelling pipelines and is designed to be drop‑in friendly for anyone working with *malariasimulation* style simulation outputs.

---

## Why another EIR estimator?

While simple empirical curves work well at the national scale, local planning often needs site‑level EIR estimates that incorporate:

- **Non‑linear combinations** of covariates (seasonality, net/IRS usage, biting behaviour …)
- **Low latency** – you don’t want to wait days for a full IBMs run just to initialise a simulation grid
- **Re‑trainability** – when new simulation runs land, you should be able to fine‑tune models with one command

**estiMINT** wraps gradient‑boosted trees (*xgboost*) and random forests (*ranger*) behind a consistent API, ships a reasonably good pretrained ensemble, and lets power‑users swap in their own learners.

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

```r
library(estiMINT)

# 1. Get a prediction immediately with the shipped models -------------------
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

# 2. Train your own models ---------------------------------------------------
my_models <- build_eir_models(
  db_path          = "/my_database.duckdb", # large simulation archive
  model_dir        = "model_files",        # where to cache .rds files
  plot_dir         = "training_plots",    # residual + importance plots
  plotting         = TRUE,                 # toggle ggplot outputs
  param_limit      = 4096,                 # optional: thin the parameter grid
  sim_limit        =  8,                   # optional: max #Sims per param set dependent upon simulation bank
  tune_hyperparams = TRUE                  # 10‑fold CV grid search
)

# 3. Re‑use your freshly trained models --------------------------------------
rf_preds  <- predict_initial_eir(my_models$rf_model,  new_data, my_models$feature_cols)
xgb_preds <- predict_initial_eir(my_models$xgboost,   new_data, my_models$feature_cols)
ens_preds <- (rf_preds + xgb_preds) / 2
```

---

## Package tour

| Function                                             | What it does                                                                                                                                                                                                                                | Typical audience                             |
| ---------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------- |
| `build_eir_models()`                                 | Automates the full training loop: loads *segMINT*‑style DUCKDB database, performs optional stratified thinning, hyper‑parameter search (xgboost + ranger), writes models & feature list to disk, returns a named list ready for prediction. | Modellers who just got fresh simulation runs |
| `load_pretrained_eir_models()`                       | Instantly loads the package‑bundled models (≈2 MB) – ideal when you do *not* want to train.                                                                                                                                                 | Analysts needing quick first‑cut estimates   |
| `predict_initial_eir(model, new_data, feature_cols)` | Standardised prediction accessor that works with *xgboost*, *ranger*, or any caret‑compatible learner that returns a numeric vector.                                                                                                        | Everyone                                     |
| Quick look at RMSE evolution, variable importance, partial dependence.                                                                                                                                                                      | Modellers validating a new run               |

All data‑frames *must* include the columns listed in \`\` (returned by training / load functions). Extra columns are ignored; missing columns throw an informative error.

---

## Interfaces at a glance

```
                ┌───────────────┐         ┌──────────────────────┐
                │  build models │         │  load pretrained     │
                │   (optional)  │         │    (instant)         │
                └──────┬────────┘         └─────────┬────────────┘
                       │                              │
         .rds on disk  ▼                              ▼  in‑memory list
                ┌──────────────┐              ┌───────────────┐
                │  xgboost     │              │ packaged RF   │
                │  ranger RF   │              │ packaged XGB  │
                └──────┬───────┘              └────────┬──────┘
                       │                                │
                       ├──── predict_initial_eir() ─────┤
                       │                                │
                       ▼                                ▼
                 numeric vector                  numeric vector
                        \_______________________________/
                                        │
                                        ▼
                                Ensemble (mean)
```

---

## Expected features / column glossary

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


---

## File structure

```
estiMINT/
├── R/                       # core functions
├── inst/
│   └── pretrained_models/   # xgboost_model.rds, rf_model.rds, feature_cols.rds
├── man/                     # R‑oxygen docs
└── vignettes/               # worked examples COMING SOON!
```

---

## Contributing

Pull requests and issue reports are very welcome!\
Please open an issue first if you plan large architectural changes.



### Licence

`estiMINT` is released under the MIT License – see [`LICENSE`](LICENSE) for details.

---