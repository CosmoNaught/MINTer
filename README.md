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

- **R ≥ 4.2**
- Python 3.8 – 3.12 with **torch** (installed automatically via `{reticulate}`)
- For simulation mode: a C‑compiler (e.g., `clang`, `gcc`) and OpenMP‑capable CPU

> **Tip**\
> The first call to `library(MINTer)` can take \~1 min while the torch environment is built.

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

Field surveys usually measure *parasite prevalence*, not EIR. Thus users will not likely have access to EIR readings to interface with MINTer natively. **estiMINT** bridges that gap by converting prevalence + entomological context straight into starting‑EIR values that MINTer understands.

### End‑to‑End Example

```r
library(MINTer)

###############################################################################
# 1.  Input your bed‑net mix & context                                   🔧
###############################################################################
# Run comprehensive scenarios with prevalence-driven workflow
results <- run_mint_scenarios(
  # Current and future resistance levels
  res_use        = c(0.30, 0.45, 0.20),   # current pyrethroid resistance
  res_future     = c(0.60, 0.75, 0.50),   # resistance after next campaign
  
  # Proportion of each long‑lasting net type (must sum to ≤1)
  py_only        = c(0.40, 0.30, 0.50),
  py_pbo         = c(0.10, 0.15, 0.05),
  py_pyrrole     = c(0.00, 0.05, 0.00),
  py_ppf         = c(0.10, 0.10, 0.15),
  
  # Malaria environment parameters
  prev_vec       = c(0.20, 0.40, 0.60),   # measured PfPR2‑10
  Q0_vec         = c(0.65, 0.75, 0.85),   # anthropophagy
  phi_vec        = c(0.45, 0.65, 0.75),   # proportion bites in bed
  season_vec     = c(0, 1, 1),            # perennial vs seasonal
  routine_vec    = c(0, 0, 1),            # routine ITN distribution?
  irs_vec        = c(0.10, 0.35, 0.70),   # IRS coverage now
  irs_future_vec = c(0.20, 0.40, 0.50),   # IRS after campaign
  lsm_vec        = c(0.05, 0.45, 0.85)    # LSM coverage
)

###############################################################################
# 2.  Save outputs & create visualizations                                📊
###############################################################################
# Access results for both predictors
write.csv(results$prevalence, "results_prevalence.csv", row.names = FALSE)
write.csv(results$cases, "results_cases.csv", row.names = FALSE)

# Generate plots
create_scenario_plots(results$prevalence, output_dir = "output/plots")
create_scenario_plots(results$cases, output_dir = "output/plots")
```

**What just happened?**

1. Field prevalence + entomological context were up‑converted into *initial EIR* using ML models (XGBoost by default).
2. Those EIRs became inputs for the neural‑network emulator (LSTM by default) for prevalence predictions, or XGBoost/RF ensemble models for case predictions.
3. Three what‑if net‑mix scenarios returned both prevalence and case projections in under a second.

### Advanced Model Options 🧪

While the defaults use well-tested models, alternative models are available:

```r
# Use ensemble of models for EIR estimation (averages predictions)
results <- run_mint_scenarios(
  # ... your parameters ...
  eir_models = c("xgboost", "rf")  # Random Forest is experimental
)

# Compare multiple prevalence prediction models (returns separate time series)
results <- run_mint_scenarios(
  # ... your parameters ...
  prevalence_models = c("LSTM", "GRU")  # GRU is experimental
)

# Run only prevalence predictions with specific models
results <- run_mint_scenarios(
  # ... your parameters ...
  predictor = "prevalence",
  eir_models = c("xgboost", "rf"),
  prevalence_models = "GRU"
)
```

**Model behavior:**
- **EIR models**: Multiple models create an ensemble (averaged predictions)
- **Prevalence models**: Multiple models create individual time series for comparison (LSTM/GRU neural networks)
- **Case models**: Always uses XGBoost/RF ensemble for annual case predictions (years 3-5)
- **Experimental models** (`rf` for EIR, `GRU` for prevalence) may have different performance characteristics

---

## ⚙️ Model Parameters – Cheat‑Sheet

| Group                       | Parameter                | Description                                              | Typical Range |
| --------------------------- | ------------------------ | -------------------------------------------------------- | ------------- |
| **Transmission intensity**  | `eir`                    | Entomological inoculation rate (infectious bites pp/yr)  | 0.1 – 500+    |
| **ITN coverage & efficacy** | `itn_use` / `itn_future` | Proportion of population sleeping under an ITN           | 0 – 1         |
|                             | `dn0_use` / `dn0_future` | Pre‑intervention reduction in biting due to ITNs         | 0 – 1         |
|                             | `phi_bednets`            | Fraction of mosquito bites taken while humans are in bed | 0 – 1         |
|                             | `routine`                | Routine ITN distribution each year (0/1)                 | {0,1}         |
| **IRS & LSM**               | `irs_use` / `irs_future` | Household coverage of IRS                                | 0 – 1         |
|                             | `lsm`                    | Larval source management coverage                        | 0 – 1         |
| **Vector behaviour**        | `Q0`                     | Human blood index                                        | 0.5 – 0.9     |
|                             | `seasonal`               | 0 = perennial, 1 = seasonal transmission                 | {0,1}         |

---

## 📊 Interpreting Outputs

- **Time** – x‑axis is years (simulated up to 6 y by default).
- **Vertical dashed line (year 3)** – change‑point between *current* and *future* coverage inputs.
- **Solid lines** – Model predictions (LSTM/GRU for prevalence, XGBoost/RF ensemble for cases).
- **Dashed lines** (simulation mode only) – ground‑truth from *malariasimulation*.

### Prevalence (`predictor = "prevalence"`)

- Output: parasite prevalence in children <5 y.
- Y‑axis: proportion infected (0 – 1).
- Method: Neural network predictions (LSTM/GRU).

### Clinical cases (`predictor = "cases"`)

- Output: annual incident clinical cases per 1000 population (years 3-5).
- Y‑axis: incidence rate per 1000 population.
- Method: XGBoost/Random Forest ensemble predictions.

---

## 👍 Best Practices

1. **Start with defaults** – `eir_models = "xgboost"`, `prevalence_models = "LSTM"`, and the built-in XGBoost/RF ensemble for cases are well‑validated.
2. **Prototype with direct emulation** before running heavy simulations.
3. **Validate**: include at least one scenario in simulation mode to benchmark emulator accuracy in your setting.
4. **Keep parameters in‑range**—especially coverage (0 – 1) and `eir` (≥0).
5. **Experiment carefully** – when using `rf` for EIR or `GRU` for prevalence models, compare results with defaults first.

---

## 🛠️ Troubleshooting

```r
reticulate::py_config()
# Re‑initialise if needed
initialize_python(verbose = TRUE)
```

---