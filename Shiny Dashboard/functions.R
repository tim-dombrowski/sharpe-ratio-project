# =============================================================================
# functions.R
# Analytical helper functions for the Sharpe Ratio Dashboard.
#
# These functions mirror the logic in the R Notebook (R Notebook/sharpe.Rmd)
# as closely as possible. Each function is self-contained and does not perform
# any I/O — all data loading happens in data_prep.R.
#
# Assignment convention: = (not <-)
# =============================================================================

library(xts)
library(zoo)
library(tidyr)
library(ggplot2)


# -----------------------------------------------------------------------------
# compute_returns()
#
# Given a ticker string and the environment where getSymbols() stored the data,
# compute monthly log returns, annualise them (×12×100), clean the column name,
# and return an xts object.
#
# Args:
#   ticker  : character, e.g. "BTC-USD" or "SPY"
#   env     : environment that holds the getSymbols() output objects
#
# Returns: single-column xts with annualised returns (%)
# -----------------------------------------------------------------------------
compute_returns = function(ticker, env) {
  # getSymbols stores objects under the original ticker name (e.g. "BTC-USD")
  data_obj = get(ticker, envir = env)

  adj_col = paste0(ticker, ".Adjusted")

  # Monthly log return: ln(P_t / P_{t-1})
  prices   = as.numeric(data_obj[, adj_col])
  log_rets = c(NA, diff(log(prices)))

  # Annualise: multiply by 12 months and convert to percent
  annualised = log_rets * 12 * 100

  # Tidy column name: strip "-USD" suffix, then non-alphanumeric chars
  base_name = sub("-USD$", "", ticker)
  base_name = gsub("[^A-Za-z0-9]", "", base_name)

  result = xts(annualised, order.by = index(data_obj))
  colnames(result) = base_name
  return(result)
}


# -----------------------------------------------------------------------------
# compute_normalized_prices()
#
# Extracts raw adjusted closing prices from a getSymbols() environment,
# normalises each series to 100 at its first observation, and returns a merged
# multi-column xts.  Used for the "Price Chart" tab.
#
# Args:
#   tickers : character vector of Yahoo Finance tickers
#   env     : environment that holds the getSymbols() output objects
#
# Returns: multi-column xts with normalised prices
# -----------------------------------------------------------------------------
compute_normalized_prices = function(tickers, env) {
  price_list = list()
  for (ticker in tickers) {
    # getSymbols stores objects under the original ticker name (e.g. "BTC-USD")
    data_obj = get(ticker, envir = env)
    adj_col  = paste0(ticker, ".Adjusted")

    prices     = as.numeric(data_obj[, adj_col])
    normalised = prices / prices[1] * 100

    base_name = sub("-USD$", "", ticker)
    base_name = gsub("[^A-Za-z0-9]", "", base_name)

    result = xts(normalised, order.by = index(data_obj))
    colnames(result) = base_name
    price_list[[base_name]] = result
  }
  return(do.call(merge, price_list))
}


# -----------------------------------------------------------------------------
# compute_real_returns()
#
# Converts nominal annualised returns in FINAL to real (inflation-adjusted)
# returns using the Fisher equation:
#   real_return = (nominal - INF) / (1 + INF/100)
#
# All columns in FINAL except "INF" are adjusted.
# The "RF" column is adjusted too — it represents the real risk-free rate.
#
# Args:
#   FINAL : xts data frame with nominal returns and an "INF" column (%)
#
# Returns: xts without the INF column (all values are real returns %)
# -----------------------------------------------------------------------------
compute_real_returns = function(FINAL) {
  asset_cols_real = setdiff(colnames(FINAL), "INF")
  REAL = FINAL[, asset_cols_real]
  for (col in asset_cols_real) {
    REAL[, col] = as.numeric(FINAL[, col] - FINAL$INF) / (1 + as.numeric(FINAL$INF / 100))
  }
  return(REAL)
}


# -----------------------------------------------------------------------------
# compute_excess_returns()
#
# Computes real excess returns (risk premia) by subtracting the real risk-free
# rate (RF column) from every other asset column.
#
# Args:
#   REAL : xts with real returns, including an "RF" column
#
# Returns: xts without the RF column (each column = asset excess return %)
# -----------------------------------------------------------------------------
compute_excess_returns = function(REAL) {
  asset_cols_xs = setdiff(colnames(REAL), "RF")
  XS = REAL[, asset_cols_xs]
  for (col in asset_cols_xs) {
    XS[, col] = REAL[, col] - REAL$RF
  }
  return(XS)
}


# -----------------------------------------------------------------------------
# compute_excess_returns_custom_rf()
#
# Same as compute_excess_returns() but uses a user-supplied constant annual
# risk-free rate (in %) instead of the RF column from FRED.
# Subtracts that constant from every real-return column.
#
# Args:
#   REAL           : xts with real returns (may or may not have RF column)
#   rf_annual_pct  : numeric, e.g. 4.5 means 4.5 % p.a.
#
# Returns: xts without the RF column (each column = asset excess return %)
# -----------------------------------------------------------------------------
compute_excess_returns_custom_rf = function(REAL, rf_annual_pct) {
  asset_cols_xs = setdiff(colnames(REAL), "RF")
  XS = REAL[, asset_cols_xs]
  for (col in asset_cols_xs) {
    XS[, col] = REAL[, col] - rf_annual_pct
  }
  return(XS)
}


# -----------------------------------------------------------------------------
# compute_summary_stats()
#
# Wraps colMeans() and apply(..., sd) to produce a clean named list of
# mean returns and volatilities for any xts data frame.
#
# Args:
#   data : xts (or data.frame) with return columns
#
# Returns: list(means, sds)
# -----------------------------------------------------------------------------
compute_summary_stats = function(data) {
  means = colMeans(data, na.rm = TRUE)
  sds   = apply(data, 2, sd, na.rm = TRUE)
  return(list(means = means, sds = sds))
}


# -----------------------------------------------------------------------------
# compute_sharpe_ratios()
#
# Sharpe ratio = mean excess return / SD of excess returns.
# Both numerator and denominator come from the XS data frame.
#
# Args:
#   XS : xts of real excess returns
#
# Returns: list(means, sds, sharpes)
# -----------------------------------------------------------------------------
compute_sharpe_ratios = function(XS) {
  stats   = compute_summary_stats(XS)
  sharpes = stats$means / stats$sds
  return(list(means = stats$means, sds = stats$sds, sharpes = sharpes))
}


# -----------------------------------------------------------------------------
# fit_capm()
#
# Runs an OLS regression: asset_excess ~ factor_excess
# Extracts alpha, beta, R-squared, and p-values.
#
# Args:
#   asset_name  : character, column name of the dependent variable in `data`
#   factor_name : character, column name of the independent variable in `data`
#   data        : data.frame or xts with both columns
#
# Returns: list(model, beta, alpha, r_squared, p_beta, p_alpha)
# -----------------------------------------------------------------------------
fit_capm = function(asset_name, factor_name, data) {
  fml   = as.formula(paste(asset_name, "~", factor_name))
  model = lm(fml, data = as.data.frame(data))

  coefs     = summary(model)$coefficients
  beta      = coef(model)[2]
  alpha     = coef(model)[1]
  r_squared = summary(model)$r.squared

  # p-values (may not exist if only one observation)
  p_alpha = if (nrow(coefs) >= 1) coefs[1, 4] else NA
  p_beta  = if (nrow(coefs) >= 2) coefs[2, 4] else NA

  return(list(
    model     = model,
    beta      = beta,
    alpha     = alpha,
    r_squared = r_squared,
    p_alpha   = p_alpha,
    p_beta    = p_beta
  ))
}


# -----------------------------------------------------------------------------
# make_corr_heatmap()
#
# Builds a ggplot2 correlation heatmap from any data frame / xts.
# Identical logic to the helper in the R Notebook.
#
# Args:
#   data       : xts or data.frame with numeric columns
#   title_text : character, plot title
#
# Returns: a ggplot object
# -----------------------------------------------------------------------------
make_corr_heatmap = function(data, title_text) {
  Rho        = cor(data, use = "pairwise.complete.obs")
  corr_order = colnames(data)

  Rho_df       = as.data.frame(Rho)
  Rho_df$Var1  = rownames(Rho_df)
  Rho_long     = tidyr::pivot_longer(
    Rho_df,
    cols      = -Var1,
    names_to  = "Var2",
    values_to = "Correlation"
  )

  Rho_long$Var1 = factor(Rho_long$Var1, levels = corr_order)
  Rho_long$Var2 = factor(Rho_long$Var2, levels = rev(corr_order))

  ggplot(Rho_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low      = "blue",
      mid      = "white",
      high     = "red",
      midpoint = 0,
      limits   = c(-1, 1)
    ) +
    geom_text(aes(label = round(Correlation, 2)), size = 3) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = title_text, x = "", y = "") +
    coord_fixed()
}


# -----------------------------------------------------------------------------
# build_summary_table()
#
# Assembles the comprehensive summary table from the R Notebook's last chunk.
# Columns: Asset, Avg_Excess_Return, Volatility, Sharpe_Ratio,
#          Beta (vs market_index), Alpha, R_Squared
#
# Args:
#   XS           : xts of real excess returns
#   market_index : character, benchmark column in XS (e.g. "SPY")
#   asset_cols   : character vector of asset names to include
#
# Returns: data.frame summary table
# -----------------------------------------------------------------------------
build_summary_table = function(XS, market_index, asset_cols) {
  sharpe_stats = compute_sharpe_ratios(XS)
  xs_means     = sharpe_stats$means
  xs_sds       = sharpe_stats$sds
  sharpes      = sharpe_stats$sharpes

  # CAPM regressions vs market index
  capm_assets = intersect(asset_cols, colnames(XS))

  betas     = setNames(rep(NA_real_, length(capm_assets)), capm_assets)
  alphas    = setNames(rep(NA_real_, length(capm_assets)), capm_assets)
  r_squared = setNames(rep(NA_real_, length(capm_assets)), capm_assets)

  if (market_index %in% colnames(XS)) {
    for (asset in capm_assets) {
      if (asset != market_index && asset %in% colnames(XS)) {
        res           = fit_capm(asset, market_index, XS)
        betas[asset]  = res$beta
        alphas[asset] = res$alpha
        r_squared[asset] = res$r_squared
      } else if (asset == market_index) {
        # By definition, market regressed on itself gives beta=1, alpha≈0, R²=1
        betas[asset]     = 1
        alphas[asset]    = 0
        r_squared[asset] = 1
      }
    }
  }

  data.frame(
    Asset          = capm_assets,
    Avg_Excess_Ret = round(as.numeric(xs_means[capm_assets]), 2),
    Volatility     = round(as.numeric(xs_sds[capm_assets]),   2),
    Sharpe_Ratio   = round(as.numeric(sharpes[capm_assets]),  2),
    Beta           = round(betas,     2),
    Alpha          = round(alphas,    2),
    R_Squared      = round(r_squared, 3),
    stringsAsFactors = FALSE
  )
}
