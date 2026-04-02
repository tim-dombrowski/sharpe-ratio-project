# Shiny Dashboard – Sharpe Ratio Analysis

## Overview

This folder contains an interactive **Shiny** dashboard that reproduces the
core financial analysis from the companion R Notebook
(`R Notebook/sharpe.Rmd`) in an interactive, browser-based form.

The dashboard allows graduate finance students and instructors to:

* Select any subset of the default assets (or add custom Yahoo Finance
  tickers).
* Adjust the historical sample length.
* Toggle between **nominal** and **real (inflation-adjusted)** returns.
* Override the risk-free rate with a custom value.
* Instantly see how changes to these inputs propagate through all metrics.

> **The original `R Notebook/` folder is completely unchanged.**
> All new code lives exclusively inside `Shiny Dashboard/`.

---

## Relationship to the R Notebook

| Notebook step | Dashboard equivalent |
|---|---|
| Parameters chunk (tickers, n_months, market/crypto index) | Sidebar controls |
| FRED import (DGS10, CPIAUCSL) | `data_prep.R → load_all_data()` |
| Yahoo Finance import (`getSymbols`) | `data_prep.R → load_all_data()` |
| `compute_returns()` | `functions.R → compute_returns()` |
| `INF$INF = …` (annualised inflation) | `data_prep.R` |
| `FINAL` (last n_months complete obs) | `data_prep.R` |
| `REAL` (Fisher real-return formula) | `functions.R → compute_real_returns()` |
| `XS` (real excess returns) | `functions.R → compute_excess_returns()` |
| `Sharpes = xsEr / xssigma` | `functions.R → compute_sharpe_ratios()` |
| `make_corr_heatmap()` | `functions.R → make_corr_heatmap()` |
| CAPM `lm()` regressions | `functions.R → fit_capm()` |
| Summary table | `functions.R → build_summary_table()` |

All financial definitions and formulas are taken directly from the notebook.
No new analytical methods are introduced.

---

## Files Created in This Folder

| File | Purpose |
|---|---|
| `app.R` | Main Shiny application (UI + server logic) |
| `functions.R` | Pure analytical helpers (returns, CAPM, Sharpe, heatmap) |
| `data_prep.R` | FRED and Yahoo Finance data loading |
| `README.md` | This file |

---

## How to Run Locally

### Prerequisites

Install the required R packages (only needed once):

```r
install.packages(c(
  "shiny",
  "fredr",
  "quantmod",
  "xts",
  "zoo",
  "tidyr",
  "ggplot2",
  "DT"
))
```

### FRED API Key

The dashboard needs a free FRED API key to download the risk-free rate
(10-year Treasury, DGS10) and CPI data.

1. Create a free account at <https://fred.stlouisfed.org/>.
2. Request an API key from
   <https://fred.stlouisfed.org/docs/api/api_key.html>.
3. Either:
   * **Paste the key** into the "FRED API Key" box in the sidebar, or
   * **Save it** in a plain-text file named `fredapikey` in the project root
     (the same location the notebook uses). The app will load it automatically.

### Launching the App

**Option A – from RStudio:**

Open `Shiny Dashboard/app.R` in RStudio and click the **Run App** button.

**Option B – from the R console:**

```r
shiny::runApp("Shiny Dashboard")
```

Run this command from the project root directory
(`sharpe-ratio-project/`).

**Option C – with the renv project library (recommended for reproducibility):**

```r
renv::restore()                       # install locked package versions
shiny::runApp("Shiny Dashboard")
```

---

## Dashboard Tabs

| Tab | What it shows |
|---|---|
| **Price Chart** | Normalised closing prices, rebased to 100 at start of sample |
| **Returns** | Faceted bar charts of monthly annualised returns |
| **Summary Table** | Avg. excess return, volatility, Sharpe ratio, beta, alpha, R² |
| **Risk–Return** | Scatter of expected excess return vs. volatility, coloured by Sharpe |
| **Correlations** | Pairwise correlation heatmap |
| **Sharpe Ratios** | Horizontal bar chart + numeric table of Sharpe ratios |
| **CAPM** | Faceted regression plots vs. market and crypto benchmarks |

---

## Key Assumptions (mirrored from the notebook)

* Returns are **monthly log returns**, annualised as `log_return × 12 × 100`.
* Inflation is computed from the **CPIAUCSL** FRED series using the same
  log-difference formula.
* The **real return** formula is: `(nominal − INF) / (1 + INF/100)`.
* **Excess returns** = real asset return − real risk-free rate.
* **Sharpe ratio** = mean excess return ÷ SD of excess returns.
* CAPM regressions use **real excess returns** for both the asset and the
  benchmark — consistent with the notebook's theoretically rigorous approach.
* Missing CPI observations are linearly interpolated (`zoo::na.approx`).

---

## Notes on Limitations

* The dashboard does **not** include the portfolio-construction section from
  the notebook (PORT_5050, PORT_9505). Portfolio weights are hard-coded in
  the notebook; an interactive weight builder would significantly complicate
  the UI without adding analytical clarity. This is documented here as a
  known omission.
* Two-factor CAPM models (market + crypto) from the notebook are not
  rendered as separate plots in the dashboard but can be reproduced by
  inspecting the `fit_capm()` function in `functions.R` and extending the
  server logic.
* Data loading requires a live internet connection and may take 10–20 seconds
  on first run.
