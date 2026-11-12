# out-of-sample R2 in percent (positive = better than the benchmark)
# statistical benchmark used by Risse (2019)
calculate_r2oos <- function(actual, forecast, benchmark) {
  100 * (1 - sum((actual - forecast)^2) / sum((actual - benchmark)^2))
}

# Clark-West (2007) test for nested models; one-sided H1: the model beats the benchmark.
# statistical benchmark used by Risse (2019)
calculate_cw <- function(actual, forecast, benchmark, h = 1) {
  f   <- (actual - benchmark)^2 - (actual - forecast)^2 + (benchmark - forecast)^2
  fit <- lm(f ~ 1)
  V   <- NeweyWest(fit, lag = max(h - 1, 0), prewhite = FALSE, adjust = TRUE)
  stat <- unname(coef(fit)[1] / sqrt(V[1, 1]))
  c(CW_stat = stat, CW_p = 1 - pnorm(stat))
}

# Diebold-Mariano on profits
calculate_dm <- function(d) {
  n <- length(d)
  if (sd(d) == 0) return(c(DM_stat = NA_real_, DM_p = NA_real_))
  stat <- mean(d) / sqrt(var(d) / n) * sqrt((n - 1) / n)
  c(DM_stat = stat, DM_p = 2 * pt(-abs(stat), df = n - 1))
}

# Directional accuracy: hit rate (% of periods where the forecast has the right sign) and 
# the Pesaran-Timmermann (1992) test; 
# one-sided H1: predicted signs carry information about realised signs. The test is undefined (NA) 
# when the forecast always has the same sign (e.g. the historical average); 
# compare that model's hit rate with the share of up periods
calculate_direction <- function(actual, forecast) {
  up_a <- actual > 0; up_f <- forecast > 0; n <- length(actual)
  P  <- mean(up_a == up_f); Py <- mean(up_a); Px <- mean(up_f)
  Ps <- Py * Px + (1 - Py) * (1 - Px) # hit rate expected if signs were independent
  vP  <- Ps * (1 - Ps) / n
  vPs <- ((2 * Py - 1)^2 * Px * (1 - Px) + (2 * Px - 1)^2 * Py * (1 - Py)) / n + 4 * Py * Px * (1 - Py) * (1 - Px) / n^2
  stat <- if (vP - vPs > 0) (P - Ps) / sqrt(vP - vPs) else NA_real_
  c(HitRate = 100 * P, PT_stat = stat, PT_p = 1 - pnorm(stat))
}

# long/cash strategy: long gold if forecast > 0 
# cost (0.1%) is paid on every switch between gold and cash
calculate_strategy <- function(gret, rf, pos, cost = 0.001, h = 1, periods_per_year = 252) {
  trades <- abs(diff(c(0, pos)))
  R  <- pos * gret + (1 - pos) * rf - cost * trades
  W  <- c(1, cumprod(1 + R)) # wealth path
  sd_ex <- sd(R - rf); ppy <- periods_per_year / h # holding periods per year
  list(R = R,
       metrics = c(CumRet = 100 * (W[length(W)] - 1), # cumulative return, %
                   CAGR = 100 * (W[length(W)]^(ppy / length(R)) - 1), # annualised return, %
                   AnnVol = 100 * sd(R) * sqrt(ppy), # annualised volatility, %
                   Sharpe = if (sd_ex > 0) sqrt(ppy) * mean(R - rf) / sd_ex else NA_real_,
                   MaxDrawdown  = 100 * min(W / cummax(W) - 1), # maximum drawdown, %
                   Trades = sum(trades),
                   PctInGold = 100 * mean(pos)))
}

# Risse (2019) mean-variance investor (gamma = 3):
# weight on gold = (forecast - rf) / (gamma * variance), bounded to [w_min, w_max]
# returns the annualised certainty-equivalent return in % per year
calculate_econ_metrics <- function(gret, ret_forecast, rf, sig2, h = 1, gamma = 3,
                                   w_min = -0.5, w_max = 1.5, periods_per_year = 252) {
  w  <- pmin(pmax((ret_forecast - rf) / (gamma * sig2), w_min), w_max)
  Rp <- w * gret + (1 - w) * rf
  c(CER_Risse = 100 * (periods_per_year / h) * (mean(Rp) - 0.5 * gamma * var(Rp)))
}

evaluate_models <- function(forecast_results, benchmark = "HA", cost = 0.001, gamma = 3) {
  forecast_results <- forecast_results %>% 
    arrange(model, date)
  
  obj <- readRDS(paste0("outputs/model_objects_h", h, ".rds"))
  h  <- unique(forecast_results$horizon)
  stopifnot(length(h) == 1)
  
  b  <- forecast_results %>% 
    filter(model == benchmark)
  
  if (!nrow(b)) {
    stop("Benchmark '", benchmark, "' not found in forecast_results")
  }
  
  s     <- seq(1, nrow(b), by = h)
  strat <- function(pos) calculate_strategy(b$gret[s], b$rf[s], pos, h = h)
  bench <- strat(as.numeric(b$ret_forecast[s] > 0))
  
  res <- lapply(unique(forecast_results$model), function(m) {
    d <- forecast_results %>% 
      filter(model == m)
    
    stopifnot(nrow(d) == nrow(b), all(d$date == b$date), !anyNA(d$forecast))
    st <- strat(as.numeric(d$ret_forecast[s] > 0))
    
    stats <- if (m == benchmark) {
      c(R2_OOS = NA, CW_stat = NA, CW_p = NA, DM_stat = NA, DM_p = NA)
    } 
    else {
      c(R2_OOS = calculate_r2oos(d$actual, d$forecast, b$forecast),
        calculate_cw(d$actual, d$forecast, b$forecast, h), calculate_dm(st$R - bench$R))
    }
    
    dirn <- calculate_direction(d$gret[s], d$ret_forecast[s])
    cer  <- calculate_econ_metrics(d$gret[s], d$ret_forecast[s], d$rf[s], d$sig2[s], h, gamma)
    as_tibble(as.list(c(stats, dirn, st$metrics, cer))) %>% 
      mutate(model = m, .before = 1)
  }) %>% 
    bind_rows()
  
  # reference strategies: buy and hold gold (its hit rate = share of up periods, the bar a model has to clear), buy and hold cash
  ref <- function(name, pos) {
    as_tibble(as.list(c(if (pos == 1) calculate_direction(b$gret[s], rep(1, length(s))),
                        strat(rep(pos, length(s)))$metrics))) %>% 
      mutate(model = name, .before = 1)
  }
  
  bind_rows(res, ref("Buy & hold gold", 1), ref("Buy & hold cash", 0)) %>% 
    arrange(desc(CumRet))
}