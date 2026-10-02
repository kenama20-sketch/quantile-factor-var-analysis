# ===============================================================
# Quantile Factor VaR Analysis for Bitcoin
# Combined R Workflow: Workbook + Yahoo Finance + Visualization
# ===============================================================
# This script integrates:
# - Bitcoin workbook ingestion and preprocessing
# - Yahoo Finance-based quantile factor generation (CMKT, CMOM, CLIQ)
# - GARCH and quantile regression VaR estimation
# - Rolling benchmark VaR comparison (Historic, Normal, Student-t)
# - Quantile-varying factor sensitivity visualization
# - CSV and PNG outputs for analysis and reporting
#
# Edit only the configuration block at the top to change the asset,
# workbook path, output folder, and date range.
# ===============================================================

# -------------------------
# 0. INSTALL / LOAD PACKAGES
# -------------------------
required_pkgs <- c(
  "readxl", "jsonlite", "dplyr", "tidyr", "zoo", "readr",
  "httr", "lubridate", "ggplot2", "quantreg", "rugarch",
  "scales", "purrr", "quantmod"
)

missing_pkgs <- required_pkgs[!(required_pkgs %in% rownames(installed.packages()))]
if (length(missing_pkgs) > 0L) {
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(zoo)
library(readr)
library(httr)
library(lubridate)
library(ggplot2)
library(quantreg)
library(rugarch)
library(scales)
library(purrr)
library(quantmod)

# -------------------------
# 1. EASY-ACCESS CONFIGURATION
# -------------------------
SOURCE_XLSX <- "C:/Users/Thoko/Downloads/BHD2.xlsx"
BTC_SHEET <- "BHD2"
ASSET_TICKER <- "BTC-USD"
START_DATE <- "2013-01-01"
END_DATE <- Sys.Date()

OUTPUT_DIR <- "C:/Users/Thoko/OneDrive/Documents/qfvar_outputs"
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
}

STRICT_FACTOR_MODE <- FALSE
ROLLING_WINDOW <- 250L
MIN_SEGMENT_OBS <- 300L
CONFIDENCE_LEVELS <- c(0.95, 0.99, 0.995)
TARGET_TAUS <- 1 - CONFIDENCE_LEVELS
TAU_GRID <- sort(unique(c(seq(0.01, 0.99, by = 0.01), 0.005, 0.025, 0.995)))

# For sensitivity visualization (smoother quantile range)
QUANTILE_RANGE_SMOOTH <- seq(0.05, 0.95, length.out = 19)

REGIMES <- data.frame(
  Regime = c(
    "2015-2017 early_bull",
    "2018-2020 post_bubble_covid",
    "2021-2022 bull_to_winter",
    "2023-2025 recovery"
  ),
  Start = as.Date(c("2015-01-01", "2018-01-01", "2021-01-01", "2023-01-01")),
  End = as.Date(c("2017-12-31", "2020-12-31", "2022-12-31", "2025-12-31")),
  stringsAsFactors = FALSE
)

# -------------------------
# 2. UTILITY FUNCTIONS
# -------------------------
normalize_name <- function(x) {
  tolower(gsub("[^A-Za-z0-9]+", "", trimws(x)))
}

find_column <- function(nms, candidates, required = TRUE) {
  nms_norm <- normalize_name(nms)
  cand_norm <- normalize_name(candidates)
  hit <- which(nms_norm %in% cand_norm)
  if (length(hit) > 0L) return(nms[hit[1L]])
  if (required) stop(sprintf("Required column not found. Expected one of: %s",
                            paste(candidates, collapse = ", ")))
  NULL
}

parse_date_any <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x_chr <- as.character(x)
  out <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  missing <- is.na(out)
  if (any(missing)) {
    out[missing] <- suppressWarnings(as.Date(
      x_chr[missing],
      tryFormats = c("%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y")
    ))
  }
  out
}

safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- gsub("%", "", as.character(x), fixed = TRUE)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

simple_return <- function(price) {
  c(NA_real_, price[-1L] / price[-length(price)] - 1)
}

# -------------------------
# 3. FACTOR GENERATION FUNCTIONS
# -------------------------
# Fetches OHLC data from Yahoo Finance and calculates the asset-specific
# market, momentum, and range-change liquidity proxies for quantile regression.
generate_quantile_factors <- function(ticker, start_date, end_date) {
  # 1. Download raw OHLCV data
  symbol <- tryCatch({
    quantmod::getSymbols(
      ticker,
      src = "yahoo",
      from = start_date,
      to = end_date,
      auto.assign = FALSE
    )
  }, error = function(e) e)

  if (inherits(symbol, "error")) {
    stop(sprintf("Unable to download '%s' from Yahoo Finance: %s",
                 ticker, symbol$message))
  }

  if (is.null(symbol) || nrow(symbol) == 0L) {
    stop(sprintf("No data returned for ticker '%s'. Check symbol or dates.", ticker))
  }

  df <- as.data.frame(symbol)
  if (is.null(df) || nrow(df) == 0L) stop("Downloaded data is empty.")

  if ("Date" %in% names(df)) {
    df$Date <- as.Date(df$Date)
  } else {
    df$Date <- as.Date(index(symbol))
  }

  # Flatten multi-index columns if present
  names(df) <- make.names(names(df), unique = TRUE)
  if ("Adjusted.Close" %in% names(df)) df$Close <- df$Adjusted.Close
  if (!"Close" %in% names(df)) stop("Downloaded data missing Close/Adjusted.Close.")

  # 2. Calculate Market Factor Proxy (CMKT)
  # Derived from the asset's own daily return dynamics
  df <- df %>%
    dplyr::select(Date, High, Low, Close) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(
      CMKT = Close / dplyr::lag(Close) - 1
    )

  # 3. Calculate 30-Day Momentum Proxy (CMOM)
  # 30-day rolling cumulative return (approx. 30 trading days)
  df <- df %>%
    dplyr::mutate(
      CMOM = zoo::rollapplyr(CMKT, 30L, sum, fill = NA_real_, partial = FALSE)
    )

  # 4. Calculate OHLC Range-Change Liquidity Proxy (CLIQ)
  # Captures (High - Low) relative to the magnitude of the absolute return.
  # To prevent division by zero when absolute return is 0, we add a tiny epsilon.
  price_range <- df$High - df$Low
  abs_return <- abs(df$CMKT)
  epsilon <- 1e-8

  df <- df %>%
    dplyr::mutate(
      CLIQ = price_range / (abs_return + epsilon)
    )

  # 5. Clean up dataset
  # Drop rows containing NaNs resulting from rolling windows and pct_change
  final_df <- df %>%
    dplyr::select(Date, CMKT, CMOM, CLIQ) %>%
    dplyr::filter(stats::complete.cases(.))

  final_df
}

# -------------------------
# 4. READ BITCOIN DATA
# -------------------------
read_BHD2 <- function(path, sheet) {
  if (!file.exists(path)) {
    stop(sprintf("Bitcoin workbook not found: %s", path))
  }

  raw <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  names(raw) <- normalize_name(names(raw))

  date_col <- find_column(names(raw), c("date", "datetime", "timestamp"))
  price_col <- find_column(names(raw), c("price", "close", "adjclose"), required = FALSE)
  return_col <- find_column(names(raw), c("returns", "return", "change"), required = FALSE)
  open_col <- find_column(names(raw), c("open"), required = FALSE)
  high_col <- find_column(names(raw), c("high"), required = FALSE)
  low_col <- find_column(names(raw), c("low"), required = FALSE)

  out <- data.frame(
    Date = parse_date_any(raw[[date_col]]),
    Price = if (!is.null(price_col)) safe_numeric(raw[[price_col]]) else NA_real_,
    Open = if (!is.null(open_col)) safe_numeric(raw[[open_col]]) else NA_real_,
    High = if (!is.null(high_col)) safe_numeric(raw[[high_col]]) else NA_real_,
    Low = if (!is.null(low_col)) safe_numeric(raw[[low_col]]) else NA_real_,
    WorkbookReturn = if (!is.null(return_col)) safe_numeric(raw[[return_col]]) else NA_real_
  )

  out <- out %>%
    dplyr::filter(!is.na(Date)) %>%
    dplyr::arrange(Date) %>%
    dplyr::distinct(Date, .keep_all = TRUE)

  if (all(is.na(out$Price))) {
    out$Return <- out$WorkbookReturn
  } else {
    out$Return <- simple_return(out$Price)
  }

  out %>%
    dplyr::filter(is.finite(Return)) %>%
    dplyr::select(Date, Price, Open, High, Low, Return)
}

# -------------------------
# 5. CREATE PROXY FACTORS (DEMO MODE)
# -------------------------
make_proxy_factors <- function(BHD2) {
  cat("Creating proxy factors for demonstration...\n")
  range_proxy <- with(BHD2, (High - Low) / ifelse(Price > 0, Price, 1))
  range_proxy[!is.finite(range_proxy)] <- NA_real_
  data.frame(
    Date = BHD2$Date,
    CMKT = BHD2$Return,
    CMOM = dplyr::lag(zoo::rollapplyr(BHD2$Return, 30L, sum,
                                      fill = NA_real_, partial = FALSE)),
    CLIQ = range_proxy - dplyr::lag(range_proxy)
  )
}

# -------------------------
# 6. GARCH AND QUANTILE MODEL FUNCTIONS
# -------------------------
fit_student_t_garch <- function(returns) {
  spec <- rugarch::ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1L, 1L)),
    mean.model = list(armaOrder = c(0L, 0L), include.mean = TRUE),
    distribution.model = "std"
  )
  fit <- rugarch::ugarchfit(
    spec = spec,
    data = returns,
    solver = "hybrid",
    solver.control = list(trace = 0)
  )
  sigma_hat <- as.numeric(rugarch::sigma(fit))
  residual <- as.numeric(rugarch::residuals(fit))
  z <- residual / sigma_hat
  list(fit = fit, sigma = sigma_hat, z = z)
}

fit_quantile_factor_model <- function(z, factors, tau_grid) {
  df <- data.frame(z = z, factors)
  fits <- lapply(tau_grid, function(tau) {
    quantreg::rq(z ~ CMKT + CMOM + CLIQ, tau = tau, data = df, method = "br")
  })
  names(fits) <- sprintf("%.6f", tau_grid)

  coefficient_table <- dplyr::bind_rows(lapply(seq_along(fits), function(i) {
    cf <- stats::coef(fits[[i]])
    data.frame(
      Tau = tau_grid[i],
      Alpha = unname(cf["(Intercept)"]),
      Beta_CMKT = unname(cf["CMKT"]),
      Beta_CMOM = unname(cf["CMOM"]),
      Beta_CLIQ = unname(cf["CLIQ"])
    )
  }))

  list(fits = fits, coefficients = coefficient_table)
}

predict_qf_var <- function(qr_model, sigma, factors, tau) {
  key <- sprintf("%.6f", tau)
  pred_z <- as.numeric(stats::predict(qr_model$fits[[key]], newdata = factors))
  q_return <- sigma * pred_z
  coef_row <- qr_model$coefficients[which.min(abs(qr_model$coefficients$Tau - tau)), ]
  contributions <- data.frame(
    Intercept = -sigma * coef_row$Alpha,
    CMKT = -sigma * coef_row$Beta_CMKT * factors$CMKT,
    CMOM = -sigma * coef_row$Beta_CMOM * factors$CMOM,
    CLIQ = -sigma * coef_row$Beta_CLIQ * factors$CLIQ
  )

  list(
    VaR = -q_return,
    PredictedReturnQuantile = q_return,
    coefficients = coef_row,
    contributions = contributions
  )
}

# -------------------------
# 7. ROLLING BENCHMARK VaR
# -------------------------
fit_student_t_window <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 30L || sd(x) <= 0) {
    return(c(location = mean(x), scale = sd(x), df = 30))
  }

  objective <- function(par) {
    location <- par[1L]
    scale <- exp(par[2L])
    df <- 2 + exp(par[3L])
    -sum(stats::dt((x - location) / scale, df = df, log = TRUE) - log(scale))
  }

  start <- c(mean(x), log(sd(x)), log(8))
  opt <- tryCatch(stats::optim(start, objective, method = "Nelder-Mead"),
                  error = function(e) NULL)
  if (is.null(opt) || !is.finite(opt$value)) {
    return(c(location = mean(x), scale = sd(x), df = 30))
  }

  c(location = opt$par[1L], scale = exp(opt$par[2L]), df = 2 + exp(opt$par[3L]))
}

rolling_benchmark_vars <- function(returns, window, confidence_levels) {
  n <- length(returns)
  out <- matrix(NA_real_, nrow = n,
                ncol = 3L * length(confidence_levels))
  colnames(out) <- c(
    paste0("Historic_", confidence_levels * 100),
    paste0("Normal_", confidence_levels * 100),
    paste0("StudentT_", confidence_levels * 100)
  )

  for (i in seq_len(n)) {
    if (i <= window) next
    x <- returns[(i - window):(i - 1L)]
    mu <- mean(x)
    s <- sd(x)
    st <- fit_student_t_window(x)
    for (j in seq_along(confidence_levels)) {
      tau <- 1 - confidence_levels[j]
      out[i, j] <- -as.numeric(stats::quantile(x, probs = tau, names = FALSE, type = 7))
      out[i, length(confidence_levels) + j] <- -(mu + s * stats::qnorm(tau))
      out[i, 2L * length(confidence_levels) + j] <- -(
        st["location"] + st["scale"] * stats::qt(tau, df = st["df"])
      )
    }
  }
  as.data.frame(out)
}

# -------------------------
# 8. BACKTESTING FUNCTIONS
# -------------------------
kupiec_test <- function(violations, expected_probability) {
  violations <- as.logical(violations)
  violations <- violations[!is.na(violations)]
  n <- length(violations)
  x <- sum(violations)
  phat <- min(max(x / n, 1e-10), 1 - 1e-10)
  p <- min(max(expected_probability, 1e-10), 1 - 1e-10)
  lr_uc <- -2 * ((n - x) * log((1 - p) / (1 - phat)) +
                   x * log(p / phat))
  data.frame(
    N = n,
    Violations = x,
    ExpectedProbability = p,
    ObservedRate = x / n,
    LR_uc = lr_uc,
    PValue_uc = stats::pchisq(lr_uc, df = 1L, lower.tail = FALSE)
  )
}

christoffersen_test <- function(violations, expected_probability) {
  v <- as.integer(violations[!is.na(violations)])
  if (length(v) < 2L) {
    return(data.frame(LR_ind = NA, PValue_ind = NA,
                      LR_cc = NA, PValue_cc = NA))
  }
  v0 <- v[-length(v)]
  v1 <- v[-1L]
  n00 <- sum(v0 == 0L & v1 == 0L)
  n01 <- sum(v0 == 0L & v1 == 1L)
  n10 <- sum(v0 == 1L & v1 == 0L)
  n11 <- sum(v0 == 1L & v1 == 1L)
  safe_rate <- function(num, den) if (den == 0L) 0 else num / den
  p01 <- min(max(safe_rate(n01, n00 + n01), 1e-10), 1 - 1e-10)
  p11 <- min(max(safe_rate(n11, n10 + n11), 1e-10), 1 - 1e-10)
  p <- min(max(safe_rate(n01 + n11, length(v) - 1L), 1e-10), 1 - 1e-10)
  ll_iid <- (n00 + n10) * log(1 - p) + (n01 + n11) * log(p)
  ll_markov <- n00 * log(1 - p01) + n01 * log(p01) +
    n10 * log(1 - p11) + n11 * log(p11)
  lr_ind <- max(0, -2 * (ll_iid - ll_markov))
  uc <- kupiec_test(v, expected_probability)
  lr_cc <- uc$LR_uc + lr_ind
  data.frame(
    N = length(v),
    N00 = n00,
    N01 = n01,
    N10 = n10,
    N11 = n11,
    LR_ind = lr_ind,
    PValue_ind = stats::pchisq(lr_ind, df = 1L, lower.tail = FALSE),
    LR_cc = lr_cc,
    PValue_cc = stats::pchisq(lr_cc, df = 2L, lower.tail = FALSE)
  )
}

backtest_one <- function(returns, var_series, confidence, regime, model) {
  keep <- is.finite(returns) & is.finite(var_series)
  r <- returns[keep]
  v <- var_series[keep]
  breaches <- r < -v
  uc <- kupiec_test(breaches, 1 - confidence)
  cc <- christoffersen_test(breaches, 1 - confidence)
  data.frame(
    Regime = regime,
    Model = model,
    Confidence = confidence,
    N = uc$N,
    ExpectedBreaches = uc$N * (1 - confidence),
    ObservedBreaches = uc$Violations,
    ObservedBreachRate = uc$ObservedRate,
    LR_uc = uc$LR_uc,
    PValue_uc = uc$PValue_uc,
    LR_ind = cc$LR_ind,
    PValue_ind = cc$PValue_ind,
    LR_cc = cc$LR_cc,
    PValue_cc = cc$PValue_cc
  )
}

# -------------------------
# 9. SEGMENT FITTING
# -------------------------
fit_segment <- function(segment_data, regime_name) {
  if (nrow(segment_data) < MIN_SEGMENT_OBS) {
    warning(sprintf("Skipping %s: only %d observations.", regime_name, nrow(segment_data)))
    return(NULL)
  }

  cat("Fitting", regime_name, "with", nrow(segment_data), "observations...\n")

  garch <- fit_student_t_garch(segment_data$Return)
  qr <- fit_quantile_factor_model(
    z = garch$z,
    factors = segment_data[, c("CMKT", "CMOM", "CLIQ")],
    tau_grid = TAU_GRID
  )

  qf <- data.frame(
    Date = segment_data$Date,
    Regime = regime_name,
    Return = segment_data$Return,
    CMKT = segment_data$CMKT,
    CMOM = segment_data$CMOM,
    CLIQ = segment_data$CLIQ,
    Sigma = garch$sigma,
    StandardizedReturn = garch$z
  )

  loading_tbl <- qr$coefficients %>%
    dplyr::mutate(Regime = regime_name, .before = 1L)

  contribution_tbl <- list()
  for (j in seq_along(CONFIDENCE_LEVELS)) {
    conf <- CONFIDENCE_LEVELS[j]
    tau <- 1 - conf
    pred <- predict_qf_var(qr, garch$sigma,
                           segment_data[, c("CMKT", "CMOM", "CLIQ")], tau)
    suffix <- as.character(conf * 100)
    qf[[paste0("QFVaR_", suffix)]] <- pred$VaR
    qf[[paste0("QFReturnQuantile_", suffix)]] <- pred$PredictedReturnQuantile
    qf[[paste0("QFBreach_", suffix)]] <- qf$Return < -pred$VaR
    ctbl <- pred$contributions
    ctbl$Date <- segment_data$Date
    ctbl$Regime <- regime_name
    ctbl$Confidence <- conf
    ctbl$TotalQFVaR <- pred$VaR
    contribution_tbl[[j]] <- ctbl
  }

  bench <- rolling_benchmark_vars(segment_data$Return, ROLLING_WINDOW,
                                  CONFIDENCE_LEVELS)
  qf <- dplyr::bind_cols(qf, bench)

  for (model in c("Historic", "Normal", "StudentT")) {
    for (conf in CONFIDENCE_LEVELS) {
      suffix <- as.character(conf * 100)
      bcol <- paste0(model, "Breach_", suffix)
      qf[[bcol]] <- qf$Return < -qf[[paste0(model, "_", suffix)]]
    }
  }

  list(data = qf,
       loadings = loading_tbl,
       contributions = dplyr::bind_rows(contribution_tbl),
       garch = garch,
       qr = qr)
}

# -------------------------
# 10. LOAD / PREPARE DATA
# -------------------------
load_btc_data <- function(source_xlsx, sheet_name, ticker, start_date, end_date) {
  if (file.exists(source_xlsx)) {
    cat("Reading Bitcoin workbook from:", source_xlsx, "\n")
    btc_df <- read_BHD2(source_xlsx, sheet_name)
    cat("✓ Bitcoin workbook loaded:", nrow(btc_df), "observations\n")
    return(btc_df)
  }

  cat("Workbook not found. Falling back to Yahoo Finance ticker:", ticker, "\n")
  factor_df <- generate_quantile_factors(ticker, start_date, end_date)
  if (nrow(factor_df) == 0L) stop("Yahoo fallback produced zero rows.")

  approx_ret <- factor_df$CMKT
  price <- cumprod(1 + c(0, approx_ret[-1]))
  data.frame(
    Date = factor_df$Date,
    Price = price,
    Open = NA_real_,
    High = NA_real_,
    Low = NA_real_,
    Return = approx_ret
  )
}

# -------------------------
# 11. QUANTILE SENSITIVITY VISUALIZATION
# -------------------------
plot_factor_sensitivities <- function(data, quantiles, regime_name = "Full sample") {
  """Fits quantile regressions over a range of quantiles and plots the
  evolution of beta coefficients (factor sensitivity paths)."""

  # Prepare data matrices
  X <- data[, c("CMKT", "CMOM", "CLIQ")]
  X$Intercept <- 1.0
  X <- X[, c("Intercept", "CMKT", "CMOM", "CLIQ")]
  y <- data$Return

  # Lists to store computed beta paths
  betas_alpha <- c()
  betas_mkt <- c()
  betas_mom <- c()
  betas_liq <- c()

  # Fit the model for each individual quantile level
  for (q in quantiles) {
    model <- quantreg::rq(y ~ X[, "CMKT"] + X[, "CMOM"] + X[, "CLIQ"],
                         tau = q, method = "br")
    params <- stats::coef(model)
    betas_alpha <- c(betas_alpha, params["(Intercept)"])
    betas_mkt <- c(betas_mkt, params["X[, \"CMKT\"]"])
    betas_mom <- c(betas_mom, params["X[, \"CMOM\"]"])
    betas_liq <- c(betas_liq, params["X[, \"CLIQ\"]"])
  }

  # Create multi-panel visualization
  plot_data <- data.frame(
    Quantile = rep(quantiles, 4),
    Beta = c(betas_mkt, betas_mom, betas_liq, betas_alpha),
    Factor = rep(c("CMKT", "CMOM", "CLIQ", "Intercept"), each = length(quantiles))
  )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(Quantile, Beta, colour = Factor)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~Factor, scales = "free_y", ncol = 2) +
    ggplot2::geom_hline(yintercept = 0, colour = "black", linetype = "--", alpha = 0.5) +
    ggplot2::labs(
      title = sprintf("Quantile-Varying Factor Sensitivities: %s", regime_name),
      subtitle = "Beta coefficients evolve across the conditional distribution",
      x = expression(tau ~ " (Quantile)"),
      y = "Beta Coefficient",
      colour = "Factor"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  p
}

# -------------------------
# 12. MAIN EXECUTION
# -------------------------
run_quantile_factor_var <- function() {
  btc_df <- load_btc_data(SOURCE_XLSX, BTC_SHEET, ASSET_TICKER, START_DATE, END_DATE)

  if (!"Price" %in% names(btc_df) || all(is.na(btc_df$Price))) {
    price <- cumprod(1 + c(0, btc_df$Return[-1]))
    btc_df$Price <- price
  }

  factors <- make_proxy_factors(btc_df)
  factor_source <- "WORKBOOK + DEMO PROXY FACTORS"

  model_data <- btc_df %>%
    dplyr::left_join(factors, by = "Date") %>%
    dplyr::filter(is.finite(Return), is.finite(CMKT), is.finite(CMOM), is.finite(CLIQ)) %>%
    dplyr::arrange(Date)

  if (nrow(model_data) < MIN_SEGMENT_OBS) {
    stop("Too few complete observations after joining Bitcoin and factor data.")
  }

  cat("✓ Model data prepared:", nrow(model_data), "observations\n")

  segment_list <- list()
  segment_list[["Full sample"]] <- model_data
  for (i in seq_len(nrow(REGIMES))) {
    segment_list[[REGIMES$Regime[i]]] <- model_data %>%
      dplyr::filter(Date >= REGIMES$Start[i], Date <= REGIMES$End[i])
  }

  fits <- list()
  for (nm in names(segment_list)) {
    fits[[nm]] <- fit_segment(segment_list[[nm]], nm)
  }
  fits <- fits[!vapply(fits, is.null, logical(1))]

  visualisation_data <- dplyr::bind_rows(lapply(fits, `[[`, "data"))
  factor_loadings <- dplyr::bind_rows(lapply(fits, `[[`, "loadings"))
  factor_contributions <- dplyr::bind_rows(lapply(fits, `[[`, "contributions"))

  model_columns <- c(
    paste0("Historic_", CONFIDENCE_LEVELS * 100),
    paste0("Normal_", CONFIDENCE_LEVELS * 100),
    paste0("StudentT_", CONFIDENCE_LEVELS * 100),
    paste0("QFVaR_", CONFIDENCE_LEVELS * 100)
  )

  model_names <- c(
    rep("Historic", length(CONFIDENCE_LEVELS)),
    rep("Normal", length(CONFIDENCE_LEVELS)),
    rep("StudentT", length(CONFIDENCE_LEVELS)),
    rep("QuantileFactor", length(CONFIDENCE_LEVELS))
  )

  backtest_results <- dplyr::bind_rows(lapply(seq_along(model_columns), function(k) {
    col <- model_columns[k]
    dplyr::bind_rows(lapply(unique(visualisation_data$Regime), function(regime) {
      x <- visualisation_data[visualisation_data$Regime == regime, ]
      dplyr::bind_rows(lapply(seq_along(CONFIDENCE_LEVELS), function(j) {
        suffix <- as.character(CONFIDENCE_LEVELS[j] * 100)
        if (!grepl(paste0("_", suffix, "$"), col)) {
          return(NULL)
        }
        backtest_one(x$Return, x[[col]], CONFIDENCE_LEVELS[j], regime, model_names[k])
      }))
    }))
  }))

  metadata <- data.frame(
    Item = c("SourceWorkbook", "AssetTicker", "FactorSource", "StrictFactorMode",
             "RollingWindow", "ConfidenceLevels", "QuantileGrid", "RVersion"),
    Value = c(SOURCE_XLSX, ASSET_TICKER, factor_source, STRICT_FACTOR_MODE,
              ROLLING_WINDOW, paste(CONFIDENCE_LEVELS, collapse = ", "),
              paste(TAU_GRID, collapse = ", "), R.version.string)
  )

  readr::write_csv(visualisation_data, file.path(OUTPUT_DIR, "visualisation_data.csv"))
  readr::write_csv(factor_loadings, file.path(OUTPUT_DIR, "factor_loadings.csv"))
  readr::write_csv(factor_contributions, file.path(OUTPUT_DIR, "factor_contributions.csv"))
  readr::write_csv(backtest_results, file.path(OUTPUT_DIR, "var_backtest_results.csv"))
  readr::write_csv(metadata, file.path(OUTPUT_DIR, "model_metadata.csv"))

  # VaR 95 comparison plot
  full_plot <- visualisation_data %>%
    dplyr::filter(Regime == "Full sample") %>%
    tidyr::pivot_longer(
      cols = c(Historic_95, Normal_95, StudentT_95, QFVaR_95),
      names_to = "Model",
      values_to = "VaR"
    ) %>%
    dplyr::mutate(ReturnThreshold = -VaR)

  p_var <- ggplot2::ggplot(full_plot, ggplot2::aes(Date)) +
    ggplot2::geom_line(ggplot2::aes(y = Return), colour = "grey35", alpha = 0.55) +
    ggplot2::geom_line(ggplot2::aes(y = ReturnThreshold, colour = Model), linewidth = 0.45,
                       na.rm = TRUE) +
    ggplot2::labs(
      title = "Bitcoin daily returns and 95% VaR thresholds",
      subtitle = "Negative return breaches occur below the model-implied threshold",
      x = NULL,
      y = "Return / VaR threshold",
      colour = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(file.path(OUTPUT_DIR, "var_95_comparison.png"), p_var,
                  width = 11, height = 6, dpi = 300)

  # QF breach plot
  p_breach <- visualisation_data %>%
    dplyr::filter(Regime == "Full sample") %>%
    ggplot2::ggplot(ggplot2::aes(Date, Return)) +
    ggplot2::geom_line(colour = "grey40", alpha = 0.55) +
    ggplot2::geom_point(
      data = function(d) d[d$QFBreach_99.5 %in% TRUE, ],
      colour = "firebrick",
      size = 1.3
    ) +
    ggplot2::labs(
      title = "99.5% Quantile Factor VaR breaches",
      subtitle = "Red points indicate returns below the QF-VaR threshold",
      x = NULL,
      y = "Bitcoin return"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(file.path(OUTPUT_DIR, "qfvar_99_5_breaches.png"), p_breach,
                  width = 11, height = 5, dpi = 300)

  # Loadings plot (all quantiles)
  loadings_plot <- factor_loadings %>%
    tidyr::pivot_longer(c(Beta_CMKT, Beta_CMOM, Beta_CLIQ),
                        names_to = "Factor", values_to = "Loading") %>%
    dplyr::filter(Regime == "Full sample")

  p_loadings <- ggplot2::ggplot(loadings_plot, ggplot2::aes(Tau, Loading, colour = Factor)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_vline(xintercept = TARGET_TAUS, linetype = "dashed", alpha = 0.5) +
    ggplot2::labs(
      title = "Quantile-varying Bitcoin factor loadings (Full Tau Grid)",
      subtitle = "Dashed lines mark the lower-tail quantiles used for VaR",
      x = expression(tau),
      y = "Estimated loading",
      colour = "Factor"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(file.path(OUTPUT_DIR, "quantile_factor_loadings.png"), p_loadings,
                  width = 10, height = 6, dpi = 300)

  # Smooth quantile sensitivity visualization
  full_sample_data <- visualisation_data %>%
    dplyr::filter(Regime == "Full sample")
  p_sensitivity <- plot_factor_sensitivities(full_sample_data, QUANTILE_RANGE_SMOOTH, "Full Sample")
  ggplot2::ggsave(file.path(OUTPUT_DIR, "quantile_sensitivities_smooth.png"), p_sensitivity,
                  width = 12, height = 10, dpi = 300)

  # Contributions plot
  contrib_plot <- factor_contributions %>%
    dplyr::filter(Regime == "Full sample", Confidence == 0.99) %>%
    tidyr::pivot_longer(c(Intercept, CMKT, CMOM, CLIQ),
                        names_to = "Component", values_to = "Contribution") %>%
    dplyr::group_by(Component) %>%
    dplyr::summarise(MeanContribution = mean(Contribution, na.rm = TRUE),
                     MeanAbsoluteContribution = mean(abs(Contribution), na.rm = TRUE),
                     .groups = "drop")

  p_contrib <- ggplot2::ggplot(
    contrib_plot,
    ggplot2::aes(reorder(Component, MeanContribution), MeanContribution, fill = Component)
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Average 99% QF-VaR component contribution",
      subtitle = "Full sample; contributions sum to the model-implied QF-VaR day by day",
      x = NULL,
      y = "Mean contribution to VaR"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(file.path(OUTPUT_DIR, "qfvar_factor_contributions.png"), p_contrib,
                  width = 9, height = 5, dpi = 300)

  writeLines(c(
    "Quantile Factor VaR workflow completed successfully.",
    paste("Factor source:", factor_source),
    paste("Observations used:", nrow(model_data)),
    paste("Date range:", min(model_data$Date), "to", max(model_data$Date)),
    paste("Output directory:", normalizePath(OUTPUT_DIR)),
    "Primary files: visualisation_data.csv, var_backtest_results.csv, factor_loadings.csv, factor_contributions.csv",
    "Quantile sensitivity plot: quantile_sensitivities_smooth.png",
    "Interpret backtesting as in-sample unless a rolling out-of-sample refit is added."
  ), con = file.path(OUTPUT_DIR, "README.txt"))

  cat("\n========== ANALYSIS SUMMARY ==========" , "\n")
  cat("Factor source:", factor_source, "\n")
  cat("Observations used:", nrow(model_data), "\n")
  cat("Date range:", min(model_data$Date), "to", max(model_data$Date), "\n")
  cat("Output directory:", normalizePath(OUTPUT_DIR), "\n")
  cat("✓ Analysis completed. Outputs saved to:", normalizePath(OUTPUT_DIR), "\n")

  invisible(list(
    data = model_data,
    fits = fits,
    visualisation_data = visualisation_data,
    factor_loadings = factor_loadings,
    factor_contributions = factor_contributions,
    backtest_results = backtest_results,
    metadata = metadata
  ))
}

# -------------------------
# 13. RUN
# -------------------------
cat("\n=== Starting Bitcoin Quantile Factor VaR Analysis ===\n")
results <- run_quantile_factor_var()
cat("\n=== Analysis Complete ===\n")
