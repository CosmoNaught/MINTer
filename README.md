# MINTer &#x20;

**M**alaria **INT**ervention **e**mulator in **R**\
Fast, lightweight emulation of *malariasimulation* written entirely for the R ecosystem.

---

## ✨ Key Features

| Feature                     | Description                                                                                                                                               |
| --------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **Direct emulation**        | Predict prevalence or clinical cases in seconds using pre‑trained GRU / LSTM networks—no simulator required.                                              |
| **Full pipeline mode**      | Launch full *malariasimulation* runs, store outputs in a DuckDB database with **segMINT**, then emulate for counter‑factuals or performance benchmarking. |
| **Python‑free R workflow**  | Python (Torch) is configured automatically on first use—no manual setup.                                                                                  |
| **New! **``** integration** | Convert point‑prevalence surveys directly into entomological inoculation rate (EIR) inputs, then pipe them straight into the emulator.                    |

---

## 📦 Installation

```r
# 1. Install devtools if needed
if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools")

# 2. Install MINTer
devtools::install_github("CosmoNaught/MINTer")

# 3. (Optional) Tools for the full pipeline
#    segMINT  – database helper
#    estiMINT – prevalence → EIR models

devtools::install_github("CosmoNaught/segMINT")
devtools::install_github("CosmoNaught/estiMINT")
```

### System Requirements

- **R ≥ 4.2**
- Python 3.8 – 3.12 with **torch** (installed automatically via `{reticulate}`)
- For simulation mode: a C‑compiler (e.g., `clang`, `gcc`) and OpenMP‑capable CPU

> **Tip**\
> The first call to `library(MINTer)` can take \~1 min while the torch environment is built.

---

## 🚀 Quick Start

### 1. Direct Emulation (fastest)

```r
library(MINTer)

# Define 3 intervention scenarios
scenarios <- create_scenarios(
  eir          = c(5.2, 35.8, 180.5),
  dn0_use      = c(0.15, 0.35, 0.55),
  dn0_future   = c(0.20, 0.45, 0.65),
  Q0           = c(0.65, 0.75, 0.85),
  phi_bednets  = c(0.45, 0.65, 0.75),
  seasonal     = c(0, 1, 1),
  routine      = c(0, 0, 1),
  itn_use      = c(0.25, 0.55, 0.85),
  irs_use      = c(0.10, 0.35, 0.70),
  itn_future   = c(0.30, 0.60, 0.90),
  irs_future   = c(0.15, 0.40, 0.75),
  lsm          = c(0.05, 0.45, 0.85)
)

# Run the neural‑network emulator
results <- run_malaria_emulator(
  scenarios   = scenarios,
  predictor   = "prevalence",   # or "cases"
  model_types = c("LSTM", "GRU")
)

# Plot and save
create_scenario_plots(results, output_dir = "output/plots")
```

### 2. Full Simulation ➜ Database ➜ Emulation

```r
library(MINTer)
library(segMINT)

# 2A. Design LHS of simulation inputs
design <- create_malariasim_scenarios(...)
write.csv(design, "Data/malariasim_scenarios.csv", row.names = FALSE)

# 2B. Run simulations (parallelised)
run_malariasim(max_threads = 12,
               lhs_scenario = "Data/malariasim_scenarios.csv",
               output_dir   = "Data/Simout")

# 2C. Ingest outputs into DuckDB
dir.create("Data/Database", recursive = TRUE, showWarnings = FALSE)
segMINT::create_database(
  dir        = "Data/Database",
  file_name  = "malariasim.duckdb",
  table_name = "sim_results",
  data_dir   = "Data/Simout"
)

# 2D. Emulate counter‑factuals on scenario #3
cases <- run_malaria_emulator(
  db_path       = "Data/Database/malariasim.duckdb",
  param_index   = 3,                     # pull inputs from sim #3
  predictor     = "cases",
  counterfactual = list(eir = c(0.5, 60, 300))
)
```

---

## 🔌 New: Prevalence‑Driven Workflows with **estiMINT**

Field surveys usually measure *parasite prevalence*, not EIR. Thus users will not likely have access to EIR readings to interface with MINTe natively.  **estiMINT** bridges that gap by converting prevalence + entomological context straight into starting‑EIR values that MINTer understands.

### End‑to‑End Example

```r
library(MINTer)
library(estiMINT)

###############################################################################
# 1.  Input your bed‑net mix & context                                   🔧
###############################################################################
res_use        <- c(0.30, 0.45, 0.20)   # current pyrethroid resistance
res_future     <- c(0.60, 0.75, 0.50)   # resistance after next campaign

# proportion of each long‑lasting net type in circulation (must sum to ≤1)
py_only   <- c(0.40, 0.30, 0.50)
py_pbo    <- c(0.10, 0.15, 0.05)
py_pyrrole<- c(0.00, 0.05, 0.00)
py_ppf    <- c(0.10, 0.10, 0.15)

prev_vec  <- c(0.20, 0.40, 0.60)   # measured PfPR2‑10
Q0_vec    <- c(0.65, 0.75, 0.85)   # anthropophagy
phi_vec   <- c(0.45, 0.65, 0.75)   # proportion bites in bed
season_vec<- c(0, 1, 1)            # perennial vs seasonal
routine_vec <- c(0, 0, 1)          # routine ITN distribution?
irs_vec   <- c(0.10, 0.35, 0.70)   # IRS now
irs_future_vec <- c(0.20, 0.40, 0.50) # IRS after campaign
lsm_vec   <- c(0.05, 0.45, 0.85)   # LSM coverage

###############################################################################
# 2.  One‑off: load the pretrained prevalence→EIR models                 
###############################################################################
pretrained <- estiMINT::load_pretrained_eir_models()

###############################################################################
# 3.  Helper to run *one* composite scenario                              
###############################################################################
run_scenario <- function(i) {

  # (a) Net performance today & after campaign
  net_now  <- calculate_overall_dn0(res_use[i],  py_only[i], py_pbo[i],
                                    py_pyrrole[i], py_ppf[i])
  net_next <- calculate_overall_dn0(res_future[i], py_only[i], py_pbo[i],
                                    py_pyrrole[i], py_ppf[i])

  # (b) Estimate starting EIR for each prevalence point
  runtime <- data.frame(prevalence  = prev_vec,
                        dn0_use     = net_now$dn0,
                        Q0          = Q0_vec,
                        phi_bednets = phi_vec,
                        seasonal    = season_vec,
                        routine     = routine_vec,
                        itn_use     = net_now$itn_use,
                        irs_use     = irs_vec)

  eir <- rowMeans(cbind(
    estiMINT::predict_initial_eir(pretrained$xgboost, runtime, pretrained$feature_cols),
    estiMINT::predict_initial_eir(pretrained$rf_model, runtime, pretrained$feature_cols)))

  # (c) Build emulator scenarios
  scen <- create_scenarios(
    eir          = eir,
    dn0_use      = net_now$dn0,
    dn0_future   = net_next$dn0,
    Q0           = Q0_vec,
    phi_bednets  = phi_vec,
    seasonal     = season_vec,
    routine      = routine_vec,
    itn_use      = net_now$itn_use,
    irs_use      = irs_vec,
    itn_future   = net_next$itn_use,
    irs_future   = irs_future_vec,
    lsm          = lsm_vec)

  out <- run_malaria_emulator(scenarios = scen,
                              predictor = "prevalence",
                              model_types = c("LSTM", "GRU"))
  out$scenario <- paste0("scenario", i)
  out
}

###############################################################################
# 4.  Execute three composite scenarios                                   
###############################################################################
results <- do.call(rbind, lapply(seq_along(res_use), run_scenario))
write.csv(results, "results_prevalence.csv", row.names = FALSE)
create_scenario_plots(results, output_dir = "output/plots")
```

**What just happened?**

1. Field prevalence + entomological context were up‑converted into *initial EIR* using ensemble ML models (XGBoost + Random Forest).
2. Those EIRs became inputs for the neural‑network emulator.
3. Three what‑if net‑mix scenarios returned prevalence projections in under a second.

---

## ⚙️ Model Parameters – Cheat‑Sheet

| Group                       | Parameter                | Description                                              | Typical Range |
| --------------------------- | ------------------------ | -------------------------------------------------------- | ------------- |
| **Transmission intensity**  | `eir`                    | Entomological inoculation rate (infectious bites pp/yr)  | 0.1 – 500+    |
| **ITN coverage & efficacy** | `itn_use` / `itn_future` | Proportion of population sleeping under an ITN           | 0 – 1         |
|                             | `dn0_use` / `dn0_future` | Pre‑intervention reduction in biting due to ITNs         | 0 – 1         |
|                             | `phi_bednets`            | Fraction of mosquito bites taken while humans are in bed | 0 – 1         |
|                             | `routine`                | Routine ITN distribution each year (0/1)                 | {0,1}         |
| **IRS & LSM**               | `irs_use` / `irs_future` | Household coverage of IRS                                | 0 – 1         |
|                             | `lsm`                    | Larval source management coverage                        | 0 – 1         |
| **Vector behaviour**        | `Q0`                     | Human blood index                                        | 0.5 – 0.9     |
|                             | `seasonal`               | 0 = perennial, 1 = seasonal transmission                 | {0,1}         |

---

## 📊 Interpreting Outputs

- **Time** – x‑axis is years (simulated up to 6 y by default).
- **Vertical dashed line (year 3)** – change‑point between *current* and *future* coverage inputs.
- **Solid lines** – GRU (blue) & LSTM (orange) predictions.
- **Dashed lines** (simulation mode only) – ground‑truth from *malariasimulation*.

### Prevalence (`predictor = "prevalence"`)

- Output: parasite prevalence in children <5 y.
- Y‑axis: proportion infected (0 – 1).

### Clinical cases (`predictor = "cases"`)

- Output: incident clinical cases per 1000 population per 30 days.
- Y‑axis: incidence rate.

---

## 👍 Best Practices

1. **Prototype with direct emulation** before running heavy simulations.
2. **Validate**: include at least one scenario in simulation mode to benchmark emulator accuracy in your setting.
3. **Keep parameters in-range**—especially coverage (0 – 1) and `eir` (≥0).
4. **Scale cores**: `max_threads = parallel::detectCores() - 1` is usually safe.

---

## 🛠️ Troubleshooting

```r
reticulate::py_config()
# Re‑initialise if needed
initialize_python(verbose = TRUE)
```

---
