# =============================================================================
# app.R  –  Sharpe Ratio Analysis: Interactive Shiny Dashboard
#
# This dashboard is a parallel, interactive version of the analysis in:
#   R Notebook/sharpe.Rmd
#
# It follows the same workflow:
#   1. Import RF and CPI data from FRED
#   2. Import adjusted closing prices from Yahoo Finance
#   3. Compute nominal returns, real returns, and real excess returns
#   4. Calculate Sharpe ratios, volatilities, and correlations
#   5. Run CAPM regressions
#
# The original R Notebook folder is NOT modified.
#
# Run with:  shiny::runApp("Shiny Dashboard")
#            (or open app.R in RStudio and click "Run App")
#
# Required packages: shiny, fredr, quantmod, xts, zoo, tidyr, ggplot2, DT
# Assignment convention: = (not <-)
# =============================================================================

# ---- Package loading --------------------------------------------------------
if (!requireNamespace("shiny",    quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("fredr",    quietly = TRUE)) install.packages("fredr")
if (!requireNamespace("quantmod", quietly = TRUE)) install.packages("quantmod")
if (!requireNamespace("xts",      quietly = TRUE)) install.packages("xts")
if (!requireNamespace("zoo",      quietly = TRUE)) install.packages("zoo")
if (!requireNamespace("tidyr",    quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("ggplot2",  quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("DT",       quietly = TRUE)) install.packages("DT")

library(shiny)
library(fredr)
library(quantmod)
library(xts)
library(zoo)
library(tidyr)
library(ggplot2)
library(DT)

# ---- Source helper files ----------------------------------------------------
source("functions.R")   # analytical functions
source("data_prep.R")   # load_all_data()

# ---- Defaults ---------------------------------------------------------------
DEFAULT_TICKERS      = c("SPY", "NVDA", "TSLA", "MSTR", "BTC-USD", "ETH-USD", "ADA-USD")
DEFAULT_N_MONTHS     = 60L
DEFAULT_MARKET_INDEX = "SPY"
DEFAULT_CRYPTO_INDEX = "BTC"
DEFAULT_RF_SERIES    = "DGS10"
DEFAULT_CPI_SERIES   = "CPIAUCSL"

# Read FRED key from file if it exists (same convention as the notebook)
FRED_KEY_FILE = "../fredapikey"
default_fred_key = if (file.exists(FRED_KEY_FILE)) {
  trimws(readLines(FRED_KEY_FILE, warn = FALSE)[1])
} else {
  ""
}


# =============================================================================
# UI
# =============================================================================
ui = fluidPage(

  # Page title
  tags$head(tags$style(HTML("
    body { font-family: 'Helvetica Neue', Helvetica, sans-serif; }
    .sidebar-panel { background-color: #f8f9fa; padding: 15px; border-radius: 6px; }
    h4 { color: #333; border-bottom: 1px solid #ddd; padding-bottom: 5px; }
    .status-ok   { color: #28a745; font-weight: bold; }
    .status-err  { color: #dc3545; font-weight: bold; }
  "))),

  titlePanel(
    title = "Sharpe Ratio Analysis Dashboard",
    windowTitle = "Sharpe Ratio Dashboard"
  ),

  sidebarLayout(

    # ------------------------------------------------------------------
    # SIDEBAR  –  all user controls
    # ------------------------------------------------------------------
    sidebarPanel(
      width = 3,

      # ---- FRED API key -------------------------------------------------
      h4("FRED API"),
      passwordInput(
        "fred_key", "FRED API Key",
        value       = default_fred_key,
        placeholder = "Paste your FRED API key here"
      ),
      tags$small(tags$a(
        href   = "https://fred.stlouisfed.org/docs/api/api_key.html",
        target = "_blank",
        "Get a free FRED API key"
      )),
      br(), br(),

      # ---- Asset selection ----------------------------------------------
      h4("Assets"),
      checkboxGroupInput(
        "selected_tickers",
        label   = "Select tickers to include",
        choices = DEFAULT_TICKERS,
        selected = DEFAULT_TICKERS
      ),
      textInput(
        "custom_tickers",
        label       = "Add custom tickers (comma-separated)",
        placeholder = "e.g. AAPL, MSFT"
      ),

      hr(),

      # ---- Sample period ------------------------------------------------
      h4("Sample"),
      numericInput(
        "n_months",
        label = "Months of history",
        value = DEFAULT_N_MONTHS,
        min   = 12,
        max   = 240,
        step  = 6
      ),

      hr(),

      # ---- Benchmark indices --------------------------------------------
      h4("Benchmarks"),
      selectInput(
        "market_index",
        label   = "Market benchmark (CAPM)",
        choices = c("SPY"),
        selected = "SPY"
      ),
      selectInput(
        "crypto_index",
        label   = "Crypto benchmark (crypto CAPM)",
        choices = c("BTC"),
        selected = "BTC"
      ),

      hr(),

      # ---- Risk-free rate override -------------------------------------
      h4("Risk-Free Rate"),
      numericInput(
        "rf_override",
        label       = "Override RF rate (% p.a., blank = use FRED data)",
        value       = NA_real_,
        min         = 0,
        max         = 30,
        step        = 0.25
      ),
      tags$small("Leave blank to use the downloaded FRED rate (", DEFAULT_RF_SERIES, ")."),

      hr(),

      # ---- Return type toggle ------------------------------------------
      h4("Return Type"),
      radioButtons(
        "return_type",
        label    = NULL,
        choices  = c("Nominal returns" = "nominal",
                     "Real (inflation-adjusted) returns" = "real"),
        selected = "real"
      ),
      tags$small(
        "Sharpe ratios always use real excess returns (matching the notebook)."
      ),

      hr(),

      # ---- Load data button --------------------------------------------
      actionButton(
        "load_btn",
        label = "Load / Refresh Data",
        class = "btn-primary btn-block",
        icon  = icon("download")
      ),
      br(),
      uiOutput("load_status")
    ),


    # ------------------------------------------------------------------
    # MAIN PANEL  –  tabbed outputs
    # ------------------------------------------------------------------
    mainPanel(
      width = 9,

      tabsetPanel(
        id = "main_tabs",

        # ---- Tab 1: Normalised Price Chart --------------------------
        tabPanel(
          title = "Price Chart",
          br(),
          p("Normalised prices (rebased to 100 at the start of the sample). ",
            "Use this to compare cumulative performance across assets."),
          plotOutput("price_chart", height = "480px")
        ),

        # ---- Tab 2: Returns Overview --------------------------------
        tabPanel(
          title = "Returns",
          br(),
          p("Monthly annualised returns for each selected asset. ",
            "The toggle on the left switches between nominal and ",
            "real (inflation-adjusted) returns."),
          plotOutput("returns_chart", height = "520px")
        ),

        # ---- Tab 3: Summary Table -----------------------------------
        tabPanel(
          title = "Summary Table",
          br(),
          p("Key metrics for each asset: average excess return, volatility, ",
            "Sharpe ratio, and CAPM statistics (beta, alpha, R²) relative to ",
            "the selected market benchmark. All figures are annualised (%)."),
          DTOutput("summary_table")
        ),

        # ---- Tab 4: Risk-Return Scatter -----------------------------
        tabPanel(
          title = "Risk–Return",
          br(),
          p("Each point is one asset, plotted by its annualised volatility ",
            "(x-axis) and average real excess return (y-axis). ",
            "Colour encodes the Sharpe ratio."),
          plotOutput("risk_return_plot", height = "480px")
        ),

        # ---- Tab 5: Correlations ------------------------------------
        tabPanel(
          title = "Correlations",
          br(),
          p("Pairwise correlation heatmap for selected assets. ",
            "The return series used (nominal or real) follows the left-panel toggle."),
          plotOutput("corr_heatmap", height = "500px")
        ),

        # ---- Tab 6: Sharpe Ratios -----------------------------------
        tabPanel(
          title = "Sharpe Ratios",
          br(),
          p("Sharpe ratios computed as: (mean real excess return) / ",
            "(SD of real excess returns). Higher values indicate better ",
            "risk-adjusted performance."),
          plotOutput("sharpe_plot", height = "420px"),
          br(),
          DTOutput("sharpe_table")
        ),

        # ---- Tab 7: CAPM Analysis -----------------------------------
        tabPanel(
          title = "CAPM",
          br(),
          h4("Single-Factor CAPM (vs Market Benchmark)"),
          p("OLS regression of each asset's real excess return on the ",
            "market benchmark's real excess return. ",
            "Beta > 1 = amplified market exposure; ",
            "Beta < 1 = dampened exposure."),
          plotOutput("capm_plot", height = "520px"),
          br(),
          h4("Crypto Factor Model (vs Crypto Benchmark)"),
          p("For assets in the crypto universe, we regress on the crypto ",
            "benchmark instead of the equity market index."),
          plotOutput("crypto_capm_plot", height = "320px")
        )
      )
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================
server = function(input, output, session) {

  # --------------------------------------------------------------------------
  # Reactive: raw loaded data
  # Triggered only when the user clicks "Load / Refresh Data"
  # --------------------------------------------------------------------------
  raw_data = eventReactive(input$load_btn, {

    # Validate API key
    req(nchar(trimws(input$fred_key)) > 0)

    # Collect tickers
    base_tickers = input$selected_tickers
    custom_raw   = trimws(input$custom_tickers)
    extra_tickers = if (nchar(custom_raw) > 0) {
      trimws(strsplit(custom_raw, "[,\\s]+")[[1]])
    } else {
      character(0)
    }
    all_tickers = unique(c(base_tickers, extra_tickers))
    validate(need(length(all_tickers) > 0, "Please select at least one ticker."))

    # Update benchmark dropdowns to only offer assets that are actually loaded
    clean_names = sub("-USD$", "", all_tickers)
    clean_names = gsub("[^A-Za-z0-9]", "", clean_names)
    updateSelectInput(session, "market_index",
                      choices  = clean_names,
                      selected = if (DEFAULT_MARKET_INDEX %in% clean_names)
                                   DEFAULT_MARKET_INDEX else clean_names[1])
    updateSelectInput(session, "crypto_index",
                      choices  = clean_names,
                      selected = if (DEFAULT_CRYPTO_INDEX %in% clean_names)
                                   DEFAULT_CRYPTO_INDEX else clean_names[1])

    # Download and prepare data (may take ~10 seconds)
    withProgress(message = "Downloading data…", value = 0.3, {
      result = tryCatch(
        load_all_data(
          tickers    = all_tickers,
          fred_key   = trimws(input$fred_key),
          n_months   = as.integer(input$n_months),
          rf_series  = DEFAULT_RF_SERIES,
          cpi_series = DEFAULT_CPI_SERIES
        ),
        error = function(e) {
          stop(conditionMessage(e))
        }
      )
      setProgress(value = 1)
    })
    result
  })

  # --------------------------------------------------------------------------
  # Status message below the load button
  # --------------------------------------------------------------------------
  output$load_status = renderUI({
    if (is.null(raw_data())) return(NULL)
    d = raw_data()
    msg = sprintf(
      "Loaded %d months  |  %d assets",
      nrow(d$FINAL),
      length(d$asset_cols)
    )
    tags$p(class = "status-ok", icon("check"), msg)
  })

  # Handle load errors gracefully
  observeEvent(input$load_btn, {
    tryCatch(raw_data(), error = function(e) {
      output$load_status = renderUI({
        tags$p(class = "status-err", icon("times"),
               paste("Error:", conditionMessage(e)))
      })
    })
  })


  # --------------------------------------------------------------------------
  # Reactive: derive analysis-ready data frames from raw_data()
  # --------------------------------------------------------------------------

  # FINAL with only selected asset columns + RF + INF
  final_data = reactive({
    req(raw_data())
    d = raw_data()
    keep = c("RF", "INF", intersect(d$asset_cols, colnames(d$FINAL)))
    d$FINAL[, keep]
  })

  # REAL  (inflation-adjusted returns)
  real_data = reactive({
    req(final_data())
    compute_real_returns(final_data())
  })

  # XS  (real excess returns; uses FRED RF or user override)
  xs_data = reactive({
    req(real_data())
    REAL = real_data()
    rf_val = input$rf_override
    if (!is.na(rf_val) && is.numeric(rf_val)) {
      compute_excess_returns_custom_rf(REAL, rf_val)
    } else {
      compute_excess_returns(REAL)
    }
  })

  # Display data: nominal or real, based on toggle (excludes RF and INF)
  display_data = reactive({
    req(final_data(), real_data())
    asset_cols = raw_data()$asset_cols

    if (input$return_type == "real") {
      REAL = real_data()
      REAL[, intersect(asset_cols, colnames(REAL))]
    } else {
      FINAL = final_data()
      FINAL[, intersect(asset_cols, colnames(FINAL))]
    }
  })

  # Normalised price data trimmed to FINAL window
  price_data = reactive({
    req(raw_data())
    d = raw_data()
    asset_cols = d$asset_cols
    # Filter to selected assets only (may have extra from custom input)
    keep = intersect(asset_cols, colnames(d$PRICES))
    if (length(keep) == 0) return(NULL)
    d$PRICES[, keep]
  })


  # ==========================================================================
  # OUTPUT: Normalised Price Chart
  # ==========================================================================
  output$price_chart = renderPlot({
    req(price_data())
    P = price_data()
    validate(need(ncol(P) > 0, "No price data available. Click 'Load / Refresh Data'."))

    df = data.frame(Date = index(P), coredata(P))
    df_long = pivot_longer(df, cols = -Date, names_to = "Asset", values_to = "Price")

    ggplot(df_long, aes(x = Date, y = Price, color = Asset)) +
      geom_line(linewidth = 0.9, alpha = 0.85) +
      geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(
        title = "Normalised Price Performance (Base = 100 at Start of Sample)",
        x     = "",
        y     = "Normalised Price",
        color = "Asset"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  })


  # ==========================================================================
  # OUTPUT: Returns bar chart (faceted, matches notebook style)
  # ==========================================================================
  output$returns_chart = renderPlot({
    req(display_data())
    DD = display_data()
    validate(need(ncol(DD) > 0,
                  "No return data available. Click 'Load / Refresh Data'."))

    df = data.frame(Date = index(DD), coredata(DD))
    df_long = pivot_longer(df, cols = -Date, names_to = "Asset", values_to = "Return")
    df_long$Asset = factor(df_long$Asset, levels = colnames(DD))

    ret_label = if (input$return_type == "real") "Real" else "Nominal"

    ggplot(df_long, aes(x = Date, y = Return)) +
      geom_col(fill = "black") +
      facet_wrap(~ Asset, scales = "free_y", ncol = 3) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(
        title = paste("Monthly Annualised", ret_label, "Returns (%)"),
        x     = "",
        y     = "Annualised Return (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })


  # ==========================================================================
  # OUTPUT: Summary statistics table (DT)
  # ==========================================================================
  output$summary_table = renderDT({
    req(xs_data())
    XS           = xs_data()
    market_index = input$market_index
    asset_cols   = raw_data()$asset_cols

    validate(
      need(ncol(XS) > 0, "No data. Click 'Load / Refresh Data'."),
      need(market_index %in% colnames(XS),
           paste("Market index", market_index, "not found in loaded data."))
    )

    tbl = build_summary_table(XS, market_index, asset_cols)

    datatable(
      tbl,
      rownames  = FALSE,
      options   = list(pageLength = 15, dom = "t"),
      caption   = htmltools::tags$caption(
        style = "caption-side: top; font-weight: bold;",
        "All figures annualised (%). Beta and CAPM stats vs selected market benchmark."
      )
    ) |>
      formatStyle(
        "Sharpe_Ratio",
        backgroundColor = styleInterval(
          c(0, 0.5, 1),
          c("#f8d7da", "#fff3cd", "#d4edda", "#c3e6cb")
        )
      )
  })


  # ==========================================================================
  # OUTPUT: Risk-Return scatter plot
  # ==========================================================================
  output$risk_return_plot = renderPlot({
    req(xs_data())
    XS = xs_data()
    validate(need(ncol(XS) > 0, "No data available."))

    sr          = compute_sharpe_ratios(XS)
    asset_names = intersect(raw_data()$asset_cols, colnames(XS))

    rr_df = data.frame(
      Asset          = asset_names,
      ExpectedReturn = as.numeric(sr$means[asset_names]),
      Volatility     = as.numeric(sr$sds[asset_names]),
      SharpeRatio    = as.numeric(sr$sharpes[asset_names])
    )

    ggplot(rr_df, aes(x = Volatility, y = ExpectedReturn, label = Asset)) +
      geom_point(aes(color = SharpeRatio), size = 5, alpha = 0.9) +
      geom_text(hjust = -0.2, vjust = 0.5, size = 3.5) +
      scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
      scale_color_gradient2(
        low      = "blue",
        mid      = "grey70",
        high     = "red",
        midpoint = 0,
        name     = "Sharpe"
      ) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      labs(
        title = "Risk-Return Tradeoff: Avg. Real Excess Return vs. Volatility",
        x     = "Volatility (SD of Real Excess Returns, %)",
        y     = "Average Real Excess Return (%)"
      ) +
      theme_minimal()
  })


  # ==========================================================================
  # OUTPUT: Correlation heatmap
  # ==========================================================================
  output$corr_heatmap = renderPlot({
    req(display_data())
    DD = display_data()
    validate(need(ncol(DD) >= 2, "Need at least 2 assets for a correlation matrix."))

    ret_label = if (input$return_type == "real") "Real" else "Nominal"
    make_corr_heatmap(DD, paste("Correlation Matrix –", ret_label, "Returns"))
  })


  # ==========================================================================
  # OUTPUT: Sharpe ratio bar chart + table
  # ==========================================================================
  output$sharpe_plot = renderPlot({
    req(xs_data())
    XS = xs_data()
    validate(need(ncol(XS) > 0, "No data available."))

    asset_names = intersect(raw_data()$asset_cols, colnames(XS))
    sr          = compute_sharpe_ratios(XS)
    sharpes     = sr$sharpes[asset_names]

    sharpe_df = data.frame(
      Asset  = names(sharpes),
      Sharpe = as.numeric(sharpes)
    )
    sharpe_df$Asset = factor(sharpe_df$Asset,
                              levels = sharpe_df$Asset[order(sharpe_df$Sharpe)])

    ggplot(sharpe_df, aes(x = Asset, y = Sharpe, fill = Sharpe)) +
      geom_col(alpha = 0.9) +
      geom_hline(yintercept = 0, color = "grey40") +
      coord_flip() +
      scale_fill_gradient2(
        low      = "blue",
        mid      = "grey80",
        high     = "red",
        midpoint = 0,
        guide    = "none"
      ) +
      labs(
        title = "Sharpe Ratio by Asset",
        x     = "",
        y     = "Sharpe Ratio (Real Excess Return / Volatility)"
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 11))
  })

  output$sharpe_table = renderDT({
    req(xs_data())
    XS = xs_data()
    validate(need(ncol(XS) > 0, "No data available."))

    asset_names = intersect(raw_data()$asset_cols, colnames(XS))
    sr          = compute_sharpe_ratios(XS)

    tbl = data.frame(
      Asset              = asset_names,
      Avg_Excess_Ret_pct = round(as.numeric(sr$means[asset_names]),   2),
      Volatility_pct     = round(as.numeric(sr$sds[asset_names]),     2),
      Sharpe_Ratio       = round(as.numeric(sr$sharpes[asset_names]), 2)
    )

    datatable(tbl, rownames = FALSE, options = list(pageLength = 15, dom = "t"))
  })


  # ==========================================================================
  # OUTPUT: CAPM faceted scatter plots (market factor)
  # ==========================================================================
  output$capm_plot = renderPlot({
    req(xs_data())
    XS           = xs_data()
    market_index = input$market_index
    asset_cols   = raw_data()$asset_cols

    validate(
      need(market_index %in% colnames(XS),
           paste("Market index '", market_index, "' not found in excess returns."))
    )

    # All assets including the market index itself
    capm_assets = intersect(asset_cols, colnames(XS))

    # Build long data frame for faceted plot
    plot_data = data.frame()
    for (asset in capm_assets) {
      tmp = data.frame(
        Asset        = asset,
        MarketReturn = as.numeric(XS[, market_index]),
        AssetReturn  = as.numeric(XS[, asset])
      )
      plot_data = rbind(plot_data, tmp)
    }

    ggplot(plot_data, aes(x = MarketReturn, y = AssetReturn)) +
      geom_point(alpha = 0.5, size = 2, color = "grey40") +
      geom_smooth(method = "lm", color = "darkred", se = TRUE, linewidth = 1) +
      facet_wrap(~ Asset, scales = "free_y", ncol = 3) +
      labs(
        title = paste("CAPM Regressions: All Assets vs.", market_index,
                      "(Real Excess Returns)"),
        x     = paste(market_index, "Real Excess Returns (%)"),
        y     = "Asset Real Excess Returns (%)"
      ) +
      theme_minimal() +
      theme(
        strip.text   = element_text(size = 10, face = "bold"),
        axis.text.x  = element_text(size = 8)
      )
  })


  # ==========================================================================
  # OUTPUT: Crypto CAPM faceted scatter plot
  # ==========================================================================
  output$crypto_capm_plot = renderPlot({
    req(xs_data())
    XS           = xs_data()
    crypto_index = input$crypto_index

    validate(
      need(crypto_index %in% colnames(XS),
           paste("Crypto index '", crypto_index, "' not found in excess returns."))
    )

    # Potential crypto altcoins: all assets except the crypto index itself
    # and the equity market index
    asset_cols   = raw_data()$asset_cols
    market_index = input$market_index
    crypto_alts  = setdiff(asset_cols, c(crypto_index, market_index))
    crypto_alts  = intersect(crypto_alts, colnames(XS))

    if (length(crypto_alts) == 0) {
      return(ggplot() +
               labs(title = "No altcoin assets available for crypto CAPM") +
               theme_minimal())
    }

    plot_data = data.frame()
    for (asset in crypto_alts) {
      tmp = data.frame(
        Asset        = asset,
        CryptoReturn = as.numeric(XS[, crypto_index]),
        AssetReturn  = as.numeric(XS[, asset])
      )
      plot_data = rbind(plot_data, tmp)
    }

    ggplot(plot_data, aes(x = CryptoReturn, y = AssetReturn)) +
      geom_point(alpha = 0.5, size = 2, color = "steelblue") +
      geom_smooth(method = "lm", color = "darkblue", se = TRUE, linewidth = 1) +
      facet_wrap(~ Asset, scales = "free_y", ncol = 3) +
      labs(
        title = paste("Crypto CAPM: Assets vs.", crypto_index,
                      "(Real Excess Returns)"),
        x     = paste(crypto_index, "Real Excess Returns (%)"),
        y     = "Asset Real Excess Returns (%)"
      ) +
      theme_minimal() +
      theme(
        strip.text  = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 8)
      )
  })

}  # end server


# =============================================================================
# Run the app
# =============================================================================
shinyApp(ui = ui, server = server)
