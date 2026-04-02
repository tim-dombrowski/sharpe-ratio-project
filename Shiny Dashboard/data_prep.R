# =============================================================================
# data_prep.R
# Data loading and preparation for the Sharpe Ratio Dashboard.
#
# This module mirrors the FRED and Yahoo Finance import steps from the
# R Notebook (R Notebook/sharpe.Rmd) and returns clean, analysis-ready
# data structures.
#
# Key function: load_all_data()
#   - downloads risk-free rate and CPI from FRED
#   - downloads adjusted closing prices from Yahoo Finance
#   - computes annualised returns, inflation rates
#   - trims to the requested sample window
#   - returns a named list: list(FINAL, PRICES, asset_cols)
#
# Assignment convention: = (not <-)
# =============================================================================

library(fredr)
library(quantmod)
library(xts)
library(zoo)

# Source analytical helpers.
# When the app runs via shiny::runApp(), the working directory is the app
# folder, so "functions.R" resolves correctly.  The functions.R file must be
# sourced *before* data_prep.R is sourced in app.R.
# (app.R sources functions.R first, then data_prep.R.)


# -----------------------------------------------------------------------------
# load_all_data()
#
# Downloads and merges all data needed for the dashboard.
# Mirrors the import and cleaning steps in sharpe.Rmd exactly.
#
# Args:
#   tickers    : character vector of Yahoo Finance ticker symbols
#                (e.g. c("SPY", "BTC-USD"))
#   fred_key   : character, FRED API key
#   n_months   : integer, number of most-recent complete months to keep
#   rf_series  : FRED series ID for the risk-free rate (default "DGS10")
#   cpi_series : FRED series ID for CPI (default "CPIAUCSL")
#
# Returns: named list
#   $FINAL      – xts with columns RF, INF, and one per asset (nominal returns)
#   $PRICES     – xts of normalised prices (base-100 at start of FINAL window)
#   $asset_cols – character vector of asset column names (no RF, no INF)
# -----------------------------------------------------------------------------
load_all_data = function(tickers,
                         fred_key,
                         n_months   = 60L,
                         rf_series  = "DGS10",
                         cpi_series = "CPIAUCSL") {

  # --- 1. Authenticate with FRED ----------------------------------------
  fredr_set_key(fred_key)

  # --- 2. Download risk-free rate (monthly) --------------------------------
  RFraw = fredr(series_id = rf_series, frequency = "m")
  ALL   = xts(RFraw, order.by = RFraw$date)
  colnames(ALL)[colnames(ALL) == "value"] = "RF"
  ALL   = subset(ALL, select = -c(date, series_id, realtime_start, realtime_end))

  # --- 3. Download CPI and compute annualised inflation rate ---------------
  INFraw = fredr(series_id = cpi_series, frequency = "m")
  INF    = xts(INFraw, order.by = INFraw$date)
  colnames(INF)[colnames(INF) == "value"] = "CPI"
  INF    = subset(INF, select = -c(date, series_id, realtime_start, realtime_end))

  # Interpolate any missing CPI values (e.g. govt-shutdown months)
  INF$CPI = na.approx(INF$CPI, na.rm = FALSE)

  # Monthly log growth rate, then annualise to percent
  INF$INFmonthly = log(as.numeric(INF$CPI)) - log(as.numeric(lag(INF$CPI)))
  INF$INF        = INF$INFmonthly * 12 * 100

  # Merge annualised inflation into ALL
  ALL = merge(ALL, INF$INF)

  # --- 4. Download Yahoo Finance data (into isolated environment) ----------
  yf_env = new.env(parent = emptyenv())
  getSymbols(tickers, src = "yahoo", periodicity = "monthly", env = yf_env)

  # --- 5. Compute annualised returns for each ticker -----------------------
  returns_list = lapply(tickers, compute_returns, env = yf_env)
  ALL          = do.call(merge, c(list(ALL), returns_list))

  # --- 6. Trim to n_months most-recent complete observations ---------------
  ntrim = sum(!complete.cases(tail(ALL, 12)))   # look at last 12 rows for NAs
  FINAL = ALL[seq_len(nrow(ALL) - ntrim), ]
  FINAL = last(FINAL, n_months)

  # --- 7. Build normalised price series aligned to FINAL window ------------
  PRICES_all = compute_normalized_prices(tickers, yf_env)
  # Keep only rows that fall within the FINAL date range
  PRICES_trimmed = PRICES_all[index(FINAL)]
  # Re-normalise so that base = 100 at the start of the FINAL window
  PRICES = xts(
    apply(coredata(PRICES_trimmed), 2, function(x) x / x[1] * 100),
    order.by = index(PRICES_trimmed)
  )

  # --- 8. Identify asset columns (exclude RF and INF) ----------------------
  asset_cols = setdiff(colnames(FINAL), c("RF", "INF"))

  return(list(FINAL = FINAL, PRICES = PRICES, asset_cols = asset_cols))
}
