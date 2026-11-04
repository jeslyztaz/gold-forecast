# Plots for the gold-forecast project. Run AFTER main.R (it reads outputs/forecast_results.csv, metrics.csv and
# model_objects.rds) -- so the plots can be redone without re-running the models. PNGs go to outputs/plots/.
source("setup.R")
source(here("functions", "compute_metrics.R"))      # calculate_strategy()

cost <- 0.001                                        # same transaction cost as in main.R

fr  <- read_csv(output_file("forecast_results.csv"), show_col_types = FALSE)
mt  <- read_csv(output_file("metrics.csv"), show_col_types = FALSE)
obj <- readRDS(output_file("model_objects.rds"))
h   <- unique(fr$horizon)

show <- intersect(c("HA", "LASSO", "Post-LASSO", "Bagging", "Random Forest", "Ens: GR constrained"), unique(fr$model))
fr   <- fr %>% mutate(model = factor(model, levels = unique(c(show, model))))
theme_set(theme_minimal(base_size = 11))
save_plot <- function(p, name, w = 10, ht = 6) ggsave(output_file("plots", paste0(name, ".png")), p, width = w, height = ht, dpi = 150, bg = "white")

# ---- 1. actual vs predicted gold price (price at t+h, backed out from each model's forecast) -----------------------------
# Zoomed to the last 90 days: over the whole test period the daily error is invisible next to the price level.
p1 <- fr %>% filter(model %in% show, date > max(date) - 90) %>%
  ggplot(aes(date)) +
  geom_line(aes(y = price_actual, colour = "Actual"), linewidth = 0.4) +
  geom_line(aes(y = price_forecast, colour = "Predicted"), linewidth = 0.4) +
  facet_wrap(~ model, ncol = 2) +
  scale_colour_manual(values = c(Actual = "grey55", Predicted = "#1f77b4"), name = NULL) +
  labs(title = sprintf("Actual vs predicted gold price (%d-day ahead, last 90 days of the test period)", h),
       x = "Forecast origin", y = "USD per troy oz") + theme(legend.position = "top")
save_plot(p1, "1_actual_vs_predicted_price", ht = 8)

# ---- 2. actual vs predicted target (log return or dollar change): a point on the dashed line = a perfect forecast -------
p2 <- fr %>% filter(model %in% show) %>%
  ggplot(aes(forecast, actual)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, formula = y ~ x) +
  facet_wrap(~ model, ncol = 3, scales = "free_x") +
  labs(title = "Predicted vs realised target (test period)", x = "Predicted", y = "Actual")
save_plot(p2, "2_actual_vs_predicted_scatter", ht = 7)

# ---- 3. error comparison: cumulative squared-error difference vs the historical average (rising = model beating HA) -----
ha  <- fr %>% filter(model == "HA") %>% select(date, ha_forecast = forecast)
p3 <- fr %>% filter(model %in% setdiff(show, "HA")) %>% left_join(ha, by = "date") %>%
  group_by(model) %>% arrange(date) %>%
  mutate(csfe = cumsum((actual - ha_forecast)^2 - (actual - forecast)^2)) %>%
  ggplot(aes(date, csfe, colour = model)) +
  geom_hline(yintercept = 0, colour = "grey50") + geom_line(linewidth = 0.6) +
  labs(title = "Cumulative squared-error difference vs historical average",
       subtitle = "Above zero: the model has predicted better than the historical average so far",
       x = NULL, y = "Cumulative (e_HA^2 - e_model^2)", colour = NULL) + theme(legend.position = "top")
save_plot(p3, "3_cumulative_error_difference")

# ---- 4. out-of-sample R2 vs the historical average (stars: Clark-West test, * 10%, ** 5%, *** 1%) -----------------------
p4 <- mt %>% filter(!is.na(R2_OOS)) %>%
  mutate(stars = cut(CW_p, c(-Inf, 0.01, 0.05, 0.10, Inf), labels = c("***", "**", "*", "")),
         model = reorder(model, R2_OOS)) %>%
  ggplot(aes(R2_OOS, model, fill = R2_OOS > 0)) +
  geom_col(show.legend = FALSE) + geom_text(aes(label = stars), hjust = -0.2) +
  scale_fill_manual(values = c(`TRUE` = "#2ca02c", `FALSE` = "#d62728")) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "Out-of-sample R2 vs historical average (%)", x = "R2_OOS (%)", y = NULL)
save_plot(p4, "4_r2_oos", ht = 4.5)

# ---- 5. trading strategy: growth of 1 invested (long gold if forecast > 0, otherwise cash; costs included) -------------
b <- fr %>% filter(model == "HA") %>% arrange(date)
s <- seq(1, nrow(b), by = h)                         # non-overlapping holding periods
wealth <- function(name, pos) tibble(model = name, date = b$date[s],
                                     wealth = cumprod(1 + calculate_strategy(b$gret[s], b$rf[s], pos, cost, h)$R))
ref <- bind_rows(wealth("Buy & hold gold", rep(1, length(s))), wealth("Buy & hold cash", rep(0, length(s))))
p5 <- bind_rows(lapply(show, function(m) { d <- fr %>% filter(model == m) %>% arrange(date); wealth(m, as.numeric(d$ret_forecast[s] > 0)) })) %>%
  ggplot(aes(date, wealth)) +
  geom_line(data = ref, aes(linetype = model), colour = "black", linewidth = 1.1, alpha = 0.35) +   # drawn underneath
  geom_line(aes(colour = model), linewidth = 0.6) +
  labs(title = sprintf("Long/cash strategy, rebalanced every %d day(s), %.1f%% cost per switch", h, 100 * cost),
       subtitle = "Models that take the same positions as buy-and-hold gold lie on top of its line",
       x = NULL, y = "Value of 1 invested", colour = NULL, linetype = NULL)
save_plot(p5, "5_strategy_wealth")

# ---- 6. bagging and random forest: out-of-bag error vs number of trees (average over the refits) ------------------------
p6 <- bind_rows(tibble(model = "Bagging", trees = seq_along(obj$bagging$oob_mse), oob = obj$bagging$oob_mse),
                tibble(model = "Random Forest", trees = seq_along(obj$rf$oob_mse), oob = obj$rf$oob_mse)) %>%
  ggplot(aes(trees, oob, colour = model)) + geom_line(linewidth = 0.7) +
  labs(title = "Out-of-bag MSE vs number of trees", x = "Number of trees", y = "OOB MSE (target units squared)", colour = NULL)
save_plot(p6, "6_oob_vs_trees", ht = 4.5)

# ---- 7. predictor importance in the forests (IncNodePurity, share of the total, top 15) -------------------------------
p7 <- bind_rows(tibble(model = "Bagging", var = names(obj$bagging$importance), imp = obj$bagging$importance),
                tibble(model = "Random Forest", var = names(obj$rf$importance), imp = obj$rf$importance)) %>%
  group_by(model) %>% mutate(share = 100 * imp / sum(imp)) %>% slice_max(share, n = 15) %>% ungroup() %>%
  mutate(key = reorder(paste(model, var, sep = "___"), share)) %>%
  ggplot(aes(share, key)) + geom_col(fill = "#1f77b4") +
  scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
  facet_wrap(~ model, scales = "free_y") +
  labs(title = "Predictor importance (top 15)", x = "% of total node-purity increase", y = NULL)
save_plot(p7, "7_predictor_importance", ht = 6)

# ---- 8. LASSO / post-LASSO: how often each predictor is selected (share of refits, validation + test) ------------------
sel_freq <- function(m, name) { f <- colMeans(m$selected); tibble(model = name, var = names(f), freq = 100 * f) }
sf <- bind_rows(sel_freq(obj$lasso, "LASSO"), sel_freq(obj$postlasso, "Post-LASSO")) %>% filter(freq > 0)
p8 <- if (nrow(sf) == 0) {
  ggplot() + annotate("text", x = 0, y = 0, label = "rlasso selected no predictors in any refit\n(forecast = intercept = historical average)") +
    theme_void() + labs(title = "LASSO selection frequency")
} else {
  sf %>% group_by(model) %>% slice_max(freq, n = 20, with_ties = FALSE) %>% ungroup() %>%
    mutate(key = reorder(paste(model, var, sep = "___"), freq)) %>%
    ggplot(aes(freq, key)) + geom_col(fill = "#ff7f0e") +
    scale_y_discrete(labels = function(x) sub("^.*___", "", x)) +
    facet_wrap(~ model, scales = "free_y") +
    labs(title = "LASSO selection frequency (top 20)", x = "% of refits in which the predictor is selected", y = NULL)
}
save_plot(p8, "8_lasso_selection_frequency", ht = 6)

# ---- 9. number of predictors selected at each refit ---------------------------------------------------------------------
p9 <- bind_rows(tibble(model = "LASSO", date = as.Date(rownames(obj$lasso$selected)), n = rowSums(obj$lasso$selected)),
                tibble(model = "Post-LASSO", date = as.Date(rownames(obj$postlasso$selected)), n = rowSums(obj$postlasso$selected))) %>%
  ggplot(aes(date, n, colour = model)) + geom_step(linewidth = 0.6) +
  labs(title = "Number of predictors selected at each refit", x = NULL, y = "Selected predictors", colour = NULL)
save_plot(p9, "9_lasso_n_selected", ht = 4)

message("Saved 9 plots to ", output_file("plots"))
