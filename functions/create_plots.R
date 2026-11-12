plots <- function(forecast_results, metrics) {
  h <- unique(forecast_results$horizon)
  
  # helper: significance stars, blank (not NA) when the p-value is missing
  stars_of <- function(p) {
    s <- as.character(cut(p, c(-Inf, 0.01, 0.05, 0.10, Inf), labels = c("***", "**", "*", "")))
    s[is.na(s)] <- ""
    s
  }
  
  out <- here("outputs", "plots")
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  
  save_plot <- function(p, name, w = 10, ht = 6) {
    ggsave(file.path(out, paste0(name, ".png")), p, width = w, height = ht, dpi = 150, bg = "white")
  }
  
  forecast_results <- forecast_results %>% 
    mutate(model = factor(model, levels = unique(model)))
  
  obj <- readRDS(paste0("outputs/model_objects_h", h, ".rds"))
  theme_set(theme_minimal(base_size = 11))
  
  # actual vs predicted gold price (price at t+h) over the last 90 days
  p1 <- forecast_results %>% 
    filter(date > max(date) - 90) %>%
    ggplot(aes(date)) +
    geom_line(aes(y = price_actual, colour = "Actual"), linewidth = 0.4) +
    geom_line(aes(y = price_forecast, colour = "Predicted"), linewidth = 0.4) +
    facet_wrap(~ model, ncol = 2) +
    scale_colour_manual(values = c(Actual = "grey55", Predicted = "#1f77b4"), name = NULL) +
    labs(
      title = sprintf("Actual vs. Predicted gold price (%d-day ahead, last 90 days of the test period)", h),
      x = "Forecast origin", y = "USD per troy oz") + theme(legend.position = "top")
  
  save_plot(p1, paste0("h", h, "_01_actual_vs_predicted_price"), ht = 8)
  
  # actual vs predicted target
  p2 <- forecast_results %>%
    ggplot(aes(forecast, actual)) +
    geom_point(alpha = 0.25, size = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, formula = y ~ x) +
    facet_wrap(~ model, ncol = 3, scales = "free_x") +
    labs(title = paste0("Predicted vs. Realised Log Returns (Test Period), h = ", h), 
         x = "Predicted", y = "Actual")
  
  save_plot(p2, paste0("h", h, "_02_actual_vs_predicted_scatter"), ht = 7)
  
  # cumulative squared-error difference vs. historical average
  ha  <- forecast_results %>% 
    filter(model == "HA") %>% 
    select(date, ha_forecast = forecast)
  
  p3 <- forecast_results %>% 
    filter(model != "HA") %>% 
    left_join(ha, by = "date") %>%
    group_by(model) %>% 
    arrange(date) %>%
    mutate(csfe = cumsum((actual - ha_forecast)^2 - (actual - forecast)^2)) %>%
    ggplot(aes(date, csfe, colour = model)) +
    geom_hline(yintercept = 0, colour = "grey50") + geom_line(linewidth = 0.6) +
    labs(title = paste0("Cumulative Squared-Error Difference vs. Historical Average, h = ", h),
         x = NULL, y = "Cumulative (e_HA^2 - e_model^2)", colour = NULL) + 
    theme(legend.position = "top")
  
  save_plot(p3, paste0("h", h, "_03_cumulative_error_difference"))
  
  # out-of-sample R^2 vs. the historical average (stars: Clark-West test, * 10%, ** 5%, *** 1%)
  p4 <- metrics %>%
    filter(!is.na(R2_OOS)) %>%
    mutate(label = paste0(sprintf("%.2f", R2_OOS)),# stars_of(CW_p)),
           model = reorder(model, R2_OOS)) %>%
    ggplot(aes(R2_OOS, model, fill = R2_OOS > 0)) +
    geom_col(show.legend = FALSE) +
    geom_vline(xintercept = 0, linewidth = 0.6) +
    geom_text(aes(label = label, hjust = ifelse(R2_OOS > 0, -0.1, 1.1)), size = 3.2) +
    scale_fill_manual(values = c(`TRUE` = "#2ca02c", `FALSE` = "#d62728")) +
    scale_x_continuous(expand = expansion(mult = c(0.15, 0.15))) +              # room for labels on both sides
    labs(title = paste0("Out-of-Sample R^2 vs. Historical Average (%), h = ", h),
         #subtitle = "Stars: Clark-West test (* 10%, ** 5%, *** 1%)", 
         x = "R2_OOS (%)", y = NULL)
  
  save_plot(p4, paste0("h", h, "_04_r2_oos"), ht = 4.5)
  
  # trading strategy: growth of 1 invested
  b <- forecast_results %>% 
    filter(model == "HA") %>% 
    arrange(date)
  
  s <- seq(1, nrow(b), by = h)
  
  wealth <- function(name, pos) {
    tibble(model = name, date = b$date[s],
           wealth = cumprod(1 + calculate_strategy(b$gret[s], b$rf[s], pos, h = h)$R))
  }
  
  ref <- bind_rows(wealth("Buy & hold gold", 
                          rep(1, length(s))), wealth("Buy & hold cash", rep(0, length(s))))
  
  p5 <- bind_rows(lapply(
    setdiff(unique(as.character(forecast_results$model)), 
            c("HA", "Buy & hold gold", "Buy & hold cash")),
    function(m) {
      d <- forecast_results %>%
        filter(as.character(model) == m) %>%
        arrange(date)
      
      wealth(m, as.numeric(d$ret_forecast[s] > 0))
    } )) %>%
    ggplot(aes(date, wealth)) +
    geom_line(data = ref, aes(linetype = model), colour = "black", linewidth = 1.1, alpha = 0.35) +
    geom_line(aes(colour = model), linewidth = 0.6) +
    labs(title = paste0("Long/Cash Strategy, 0.1% Cost per Switch, h = ", h),
         subtitle = "Models that take the same positions as buy-and-hold gold lie on top of its line",
         x = NULL, y = "Value of 1 invested", colour = NULL, linetype = NULL)
  
  save_plot(p5, paste0("h", h, "_05_strategy_wealth"))
  
  # bagging and random forest: out-of-bag error vs number of trees (average over the refits)
  p_x    <- length(obj$bagging$importance)
  
  rf_lab <- if (!is.null(obj$rf$mtry)) {
    paste0("Random Forest (tuned, mtry = ", obj$rf$mtry, ")")
  } 
  else {
    "Random Forest (tuned)"
  }
  
  oob <- bind_rows(
    tibble(model = paste0("Bagging (mtry = ", p_x, ")"), trees = seq_along(obj$bagging$oob_mse), 
           oob = obj$bagging$oob_mse),
    tibble(model = rf_lab, trees = seq_along(obj$rf$oob_mse), oob = obj$rf$oob_mse))
  
  if (!is.null(obj$rf_default)) {
    oob <- bind_rows(oob, tibble(model = paste0("Random Forest (default, mtry = ", floor(p_x / 3), ")"),
                                 trees = seq_along(obj$rf_default$oob_mse), oob = obj$rf_default$oob_mse))
  }
  
  p6 <- ggplot(oob, aes(trees, oob, colour = model)) +
    geom_line(linewidth = 0.7) +
    labs(title = paste0("Out-of-Bag MSE vs. Number of Trees, h = ", h),
         x = "Number of Trees", y = "OOB MSE (Target Units Squared)", colour = NULL)
  save_plot(p6, paste0("h", h, "_06_oob_vs_trees"), ht = 4.5)
  
  # predictor importance in the forests (IncNodePurity, share of the total, top 15)
  p7 <- bind_rows(
    tibble(model = "Bagging", var = names(obj$bagging$importance), imp = obj$bagging$importance),
    tibble(model = "Random Forest", var = names(obj$rf$importance), imp = obj$rf$importance)) %>%
    group_by(model) %>% 
    mutate(share = 100 * imp / sum(imp)) %>% 
    slice_max(share, n = 15) %>% 
    ungroup() %>%
    mutate(key = reorder(paste(model, var, sep = "___"), share)) %>%
    ggplot(aes(share, key)) + 
    geom_col(fill = "#1f77b4") +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    facet_wrap(~ model, scales = "free_y") +
    labs(title = paste0("Predictor importance (top 15), h = ", h), 
         x = "% of total node-purity increase", y = NULL)
  
  save_plot(p7, paste0("h", h, "_07_predictor_importance"), ht = 6)
  
  # LASSO / post-LASSO: how often each predictor is selected
  sel_freq <- function(m, name) { 
    f <- colMeans(m$selected)
    tibble(model = name, var = names(f), freq = 100 * f) 
  }
  
  sf <- bind_rows(sel_freq(obj$lasso, "LASSO"), sel_freq(obj$postlasso, "Post-LASSO")) %>% 
    filter(freq > 0)
  
  p8 <- if (nrow(sf) == 0) {
    ggplot() + annotate("text", x = 0, y = 0, label = "rlasso selected no predictors in any refit\n(forecast = intercept = historical average)") +
      theme_void() + labs(title = "LASSO selection frequency")
  } 
  else {
    sf %>% 
      group_by(model) %>% 
      slice_max(freq, n = 20, with_ties = FALSE) %>% 
      ungroup() %>%
      mutate(key = reorder(paste(model, var, sep = "___"), freq)) %>%
      ggplot(aes(freq, key)) + 
      geom_col(fill = "#ff7f0e") +
      scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
      facet_wrap(~ model, scales = "free_y") +
      labs(title = paste0("LASSO Selection Frequency (Top 20), h = ", h), 
           x = "% of Refits in which the Predictor is Selected", y = NULL)
  }
  
  save_plot(p8, paste0("h", h, "_08_lasso_selection_frequency"), ht = 6)
  
  # LASSO / post-LASSO: number of predictors selected at each refit
  p9 <- bind_rows(
    tibble(model = "LASSO", date = as.Date(rownames(obj$lasso$selected)), 
           n = rowSums(obj$lasso$selected)),
    tibble(model = "Post-LASSO", date = as.Date(rownames(obj$postlasso$selected)), 
           n = rowSums(obj$postlasso$selected))) %>%
    ggplot(aes(date, n, colour = model)) + geom_step(linewidth = 0.6) +
    labs(title = paste0("Number of Predictors Selected at Each Refit, h = ", h), 
         x = NULL, y = "Selected Predictors", colour = NULL)
  
  save_plot(p9, paste0("h", h, "_09_lasso_n_selected"), ht = 4)
  
  # directional accuracy: hit rate vs. the share of up periods (= hit rate of always predicting "up")
  up <- metrics$HitRate[metrics$model == "Buy & hold gold"]
  
  p10 <- metrics %>%
    filter(!is.na(HitRate), !model %in% c("Buy & hold gold", "Buy & hold cash")) %>%
    mutate(label = paste0(sprintf("%.1f", HitRate), stars_of(PT_p)),
           model = reorder(model, HitRate)) %>%
    ggplot(aes(HitRate, model)) +
    geom_col(fill = "#1f77b4") +
    geom_text(aes(label = label), hjust = -0.1, size = 3.2) +
    geom_vline(xintercept = up, linetype = "dashed", colour = "red") +
    annotate("text", x = up, y = 0.6, label = sprintf(" up periods: %.1f%%", up),
             hjust = 0, colour = "red", size = 3.2) +
    coord_cartesian(xlim = c(40, max(62, up + 7))) +
    labs(title = paste0("Directional Accuracy (% of Periods with the Correct Sign), h = ", h),
         subtitle = "Stars: Pesaran-Timmermann test (* 10%, ** 5%, *** 1%); dashed line = always predicting 'up'",
         x = "Hit Rate (%)", y = NULL)
  
  save_plot(p10, paste0("h", h, "_10_directional_accuracy"), ht = 4.5)
  
  # recursive combination weights over the test period
  w <- obj$ensemble$weights
  p11 <- bind_rows(
    as_tibble(w$dmspe) %>% 
      mutate(date = w$date, method = "DMSPE"),
    as_tibble(w$gr_constrained) %>% 
      mutate(date = w$date, method = "GR constrained (>= 0, sum to 1)"),
    as_tibble(w$gr_unconstrained[, -1, drop = FALSE]) %>% 
      mutate(date = w$date, method = "GR unconstrained (slopes; intercept not shown)")) %>%
    pivot_longer(-c(date, method), names_to = "model", values_to = "weight") %>%
    ggplot(aes(date, weight, colour = model)) +
    geom_hline(yintercept = 0, colour = "grey70") + 
    geom_line(linewidth = 0.6) +
    facet_wrap(~ method, ncol = 1, scales = "free_y") +
    labs(title = paste0("Combination weights at each test origin, h = ", h), 
         x = NULL, y = "Weight", colour = NULL) + 
    theme(legend.position = "top")
  
  save_plot(p11, paste0("h", h, "_11_combination_weights"), ht = 8)
  
  # trading metrics: CAGR, Sharpe ratio and maximum drawdown, buy-and-hold gold as the dashed reference
  tm <- metrics %>%
    select(model, CAGR = matches("^(CAGR|AnnRet)$"), Sharpe, MaxDrawdown) %>%
    rename(`CAGR (%)` = CAGR, `Sharpe ratio` = Sharpe, `Max drawdown (%)` = MaxDrawdown)
  
  ord <- tm %>% 
    filter(!grepl("^Buy", model)) %>% 
    arrange(`CAGR (%)`) %>% pull(model)
  
  tm_long <- tm %>%
    pivot_longer(-model, names_to = "metric", values_to = "value") %>%
    mutate(metric = factor(metric, levels = c("CAGR (%)", "Sharpe ratio", "Max drawdown (%)")))
  
  ref <- tm_long %>% 
    filter(model == "Buy & hold gold")
  
  p12 <- tm_long %>%
    filter(model %in% ord, !is.na(value)) %>%
    mutate(model = factor(model, levels = ord)) %>%
    ggplot(aes(value, model)) +
    geom_col(fill = "steelblue") +
    geom_vline(xintercept = 0, linewidth = 0.4) +
    geom_vline(data = ref, aes(xintercept = value), linetype = "dashed", colour = "red") +
    geom_text(aes(label = sprintf("%.2f", value), hjust = ifelse(value >= 0, -0.1, 1.1)), size = 3) +
    scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
    facet_wrap(~ metric, scales = "free_x") +
    labs(title = paste0("Trading Strategy Performance, h = ", h),
         subtitle = "Gold if forecast > 0, otherwise cash, 0.1% cost per switch; dashed red line = buy-and-hold gold",
         x = NULL, y = NULL)
  
  save_plot(p12, paste0("h", h, "_12_trading_metrics"), w = 12, ht = 5)
  
  message("Saved 12 plots to ", here)
}