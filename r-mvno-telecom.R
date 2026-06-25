# Econometrics coursework: MVNO market share and household telecommunication expenditure
# This script loads quarterly telecom expenditure data, cleans variables,
# produces descriptive statistics and plots, estimates regression models,
# runs diagnostic tests, computes robust/Newey-West standard errors,
# and saves tables and figures.
# The Excel data file should be placed at data/raw/telecom_quarterly_data.xlsx.
# If the file is not found, the user is asked to choose it manually.

pkgs <- c(
  "tidyverse",
  "readxl",
  "psych",
  "moments",
  "tseries",
  "lmtest",
  "sandwich",
  "car",
  "broom",
  "modelsummary",
  "here",
  "zoo"
)

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if (length(new_pkgs) > 0) {
  install.packages(new_pkgs)
}

invisible(lapply(pkgs, library, character.only = TRUE))

dir.create(here("data"), showWarnings = FALSE)
dir.create(here("data", "raw"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("data", "processed"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs"), showWarnings = FALSE)
dir.create(here("outputs", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("outputs", "tables"), showWarnings = FALSE, recursive = TRUE)


data_path <- here("telecom_quarterly_data.xlsx")

if (!file.exists(data_path)) {
  message("Could not find data/raw/telecom_quarterly_data.xlsx.")
  message("Please choose the Excel file manually.")
  data_path <- file.choose()
}

df_raw <- read_excel(data_path)

names(df_raw) <- c(
  "quarter",
  "tel_exp",
  "mvno_share",
  "income",
  "fiveg_share",
  "household_size"
)

glimpse(df_raw)
summary(df_raw)
head(df_raw)


df <- df_raw %>%
  mutate(
    quarter_chr = as.character(quarter),
    quarter_clean = str_replace_all(str_trim(quarter_chr), "\\s+", ""),
    
    year = as.integer(str_extract(quarter_clean, "^\\d{4}")),
    qtr = as.integer(str_extract(quarter_clean, "(?<=\\.)\\d(?=/4)")),
    quarter_yq = zoo::as.yearqtr(paste(year, qtr, sep = " Q"), format = "%Y Q%q"),
    
    tel_exp = readr::parse_number(as.character(tel_exp)),
    mvno_share = readr::parse_number(as.character(mvno_share)),
    income = readr::parse_number(as.character(income)),
    fiveg_share = readr::parse_number(as.character(fiveg_share)),
    household_size = readr::parse_number(as.character(household_size))
  )

if (all(!is.na(df$quarter_yq))) {
  df <- df %>%
    arrange(quarter_yq)
} else {
  message("Quarter variable could not be fully parsed as year-quarter. Original order is used.")
}

stopifnot(all(df$tel_exp > 0, na.rm = TRUE))
stopifnot(all(df$income > 0, na.rm = TRUE))

df <- df %>%
  mutate(
    time = row_number(),
    log_tel_exp = log(tel_exp),
    log_income = log(income)
  )

if (any(df$fiveg_share > 0, na.rm = TRUE)) {
  first_5g_time <- min(df$time[df$fiveg_share > 0], na.rm = TRUE)
} else {
  first_5g_time <- Inf
}

df <- df %>%
  mutate(
    post5g = ifelse(time >= first_5g_time, 1, 0),
    
    mvno_c = mvno_share - mean(mvno_share, na.rm = TRUE),
    fiveg_c = fiveg_share - mean(fiveg_share, na.rm = TRUE),
    log_income_c = log_income - mean(log_income, na.rm = TRUE),
    
    mvno_post5g = mvno_share * post5g,
    mvno_c_post5g = mvno_c * post5g,
    mvno_fiveg = mvno_share * fiveg_share,
    mvno_c_fiveg_c = mvno_c * fiveg_c,
    mvno_c_income_c = mvno_c * log_income_c
  )

colSums(is.na(df))
table(df$post5g)

analysis_df <- df %>%
  drop_na(
    quarter,
    tel_exp,
    log_tel_exp,
    mvno_share,
    mvno_c,
    income,
    log_income,
    log_income_c,
    fiveg_share,
    fiveg_c,
    household_size,
    post5g,
    mvno_c_post5g,
    mvno_c_fiveg_c
  )

message("Number of observations in the raw data: ", nrow(df))
message("Number of observations in the analysis sample: ", nrow(analysis_df))

write_csv(analysis_df, here("data", "processed", "final_analysis_data.csv"))


vars_level <- analysis_df %>%
  select(tel_exp, mvno_share, income, fiveg_share, household_size)

vars_log <- analysis_df %>%
  select(log_tel_exp, log_income, mvno_share, fiveg_share, household_size)

make_desc_table <- function(data) {
  map_dfr(names(data), function(v) {
    x <- data[[v]]
    
    tibble(
      Variable = v,
      N = sum(!is.na(x)),
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE),
      Skewness = moments::skewness(x, na.rm = TRUE),
      Kurtosis = moments::kurtosis(x, na.rm = TRUE)
    )
  })
}

desc_table <- make_desc_table(vars_level)
log_desc_table <- make_desc_table(vars_log)

print(desc_table)
print(log_desc_table)

write_csv(desc_table, here("outputs", "tables", "descriptive_statistics_level.csv"))
write_csv(log_desc_table, here("outputs", "tables", "descriptive_statistics_log.csv"))

cor_table <- cor(vars_level, use = "pairwise.complete.obs")

print(cor_table)
write_csv(
  as.data.frame(cor_table) %>%
    rownames_to_column("Variable"),
  here("outputs", "tables", "correlation_table.csv")
)


p_tel <- ggplot(analysis_df, aes(x = time, y = tel_exp)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend in Household Telecom Expenditure",
    x = "Quarter",
    y = "Telecom expenditure"
  ) +
  scale_x_continuous(
    breaks = analysis_df$time,
    labels = analysis_df$quarter
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p_tel)

ggsave(
  filename = here("outputs", "figures", "tel_exp_trend.png"),
  plot = p_tel,
  width = 10,
  height = 5
)

p_mvno <- ggplot(analysis_df, aes(x = time, y = mvno_share)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend in MVNO Share",
    x = "Quarter",
    y = "MVNO share"
  ) +
  scale_x_continuous(
    breaks = analysis_df$time,
    labels = analysis_df$quarter
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p_mvno)

ggsave(
  filename = here("outputs", "figures", "mvno_share_trend.png"),
  plot = p_mvno,
  width = 10,
  height = 5
)

p_5g <- ggplot(analysis_df, aes(x = time, y = fiveg_share)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Trend in 5G Share",
    x = "Quarter",
    y = "5G share"
  ) +
  scale_x_continuous(
    breaks = analysis_df$time,
    labels = analysis_df$quarter
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(p_5g)

ggsave(
  filename = here("outputs", "figures", "fiveg_share_trend.png"),
  plot = p_5g,
  width = 10,
  height = 5
)

p_scatter_mvno <- ggplot(analysis_df, aes(x = mvno_share, y = tel_exp)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "MVNO Share and Household Telecom Expenditure",
    x = "MVNO share",
    y = "Telecom expenditure"
  ) +
  theme_minimal()

print(p_scatter_mvno)

ggsave(
  filename = here("outputs", "figures", "scatter_mvno_tel_exp.png"),
  plot = p_scatter_mvno,
  width = 7,
  height = 5
)

p_hist <- analysis_df %>%
  select(tel_exp, mvno_share, income, fiveg_share, household_size) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 10) +
  facet_wrap(~ variable, scales = "free") +
  labs(
    title = "Distributions of Main Variables",
    x = "Value",
    y = "Frequency"
  ) +
  theme_minimal()

print(p_hist)

ggsave(
  filename = here("outputs", "figures", "hist_variables.png"),
  plot = p_hist,
  width = 10,
  height = 7
)


# Model 1: Simple level regression
m1_simple <- lm(
  tel_exp ~ mvno_share,
  data = analysis_df
)

# Model 2: Multiple level regression
m2_level <- lm(
  tel_exp ~ mvno_share + income + fiveg_share + household_size,
  data = analysis_df
)

# Model 3: Basic log model
m3_log <- lm(
  log_tel_exp ~ mvno_share + log_income + fiveg_share + household_size,
  data = analysis_df
)

# Model 4: Log model with Post-5G dummy
m4_post5g <- lm(
  log_tel_exp ~ mvno_share + log_income + fiveg_share + household_size + post5g,
  data = analysis_df
)

# Model 5: MVNO x Post-5G interaction model
m5_mvno_post5g <- lm(
  log_tel_exp ~ mvno_c + log_income + fiveg_share + household_size +
    post5g + mvno_c_post5g,
  data = analysis_df
)

# Model 6: MVNO x 5G interaction model
m6_mvno_5g <- lm(
  log_tel_exp ~ mvno_c + log_income + fiveg_c + household_size +
    mvno_c_fiveg_c,
  data = analysis_df
)

# Model 7: Main model with linear time trend
m7_mvno_5g_time <- lm(
  log_tel_exp ~ mvno_c + log_income + fiveg_c + household_size +
    mvno_c_fiveg_c + time,
  data = analysis_df
)

# Model 8: Main model with quadratic time trend
m8_mvno_5g_time2 <- lm(
  log_tel_exp ~ mvno_c + log_income + fiveg_c + household_size +
    mvno_c_fiveg_c + time + I(time^2),
  data = analysis_df
)

summary(m1_simple)
summary(m2_level)
summary(m3_log)
summary(m4_post5g)
summary(m5_mvno_post5g)
summary(m6_mvno_5g)
summary(m7_mvno_5g_time)
summary(m8_mvno_5g_time2)


ci_m3 <- broom::tidy(
  m3_log,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  mutate(Model = "Model 3: Basic log model")

ci_m6 <- broom::tidy(
  m6_mvno_5g,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  mutate(Model = "Model 6: MVNO x 5G model")

ci_m7 <- broom::tidy(
  m7_mvno_5g_time,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  mutate(Model = "Model 7: MVNO x 5G + time trend")

confidence_interval_table <- bind_rows(
  ci_m3,
  ci_m6,
  ci_m7
) %>%
  select(
    Model,
    term,
    estimate,
    std.error,
    statistic,
    p.value,
    conf.low,
    conf.high
  )

print(confidence_interval_table)

write_csv(
  confidence_interval_table,
  here("outputs", "tables", "confidence_intervals_key_models.csv")
)


compare_level <- tibble(
  Model = c("Simple level", "Multiple level"),
  R2 = c(
    summary(m1_simple)$r.squared,
    summary(m2_level)$r.squared
  ),
  Adj_R2 = c(
    summary(m1_simple)$adj.r.squared,
    summary(m2_level)$adj.r.squared
  ),
  AIC = c(
    AIC(m1_simple),
    AIC(m2_level)
  ),
  BIC = c(
    BIC(m1_simple),
    BIC(m2_level)
  )
)

compare_log <- tibble(
  Model = c(
    "Basic log",
    "Log + Post5G",
    "Log + MVNO x Post5G",
    "Log + MVNO x 5G"
  ),
  R2 = c(
    summary(m3_log)$r.squared,
    summary(m4_post5g)$r.squared,
    summary(m5_mvno_post5g)$r.squared,
    summary(m6_mvno_5g)$r.squared
  ),
  Adj_R2 = c(
    summary(m3_log)$adj.r.squared,
    summary(m4_post5g)$adj.r.squared,
    summary(m5_mvno_post5g)$adj.r.squared,
    summary(m6_mvno_5g)$adj.r.squared
  ),
  AIC = c(
    AIC(m3_log),
    AIC(m4_post5g),
    AIC(m5_mvno_post5g),
    AIC(m6_mvno_5g)
  ),
  BIC = c(
    BIC(m3_log),
    BIC(m4_post5g),
    BIC(m5_mvno_post5g),
    BIC(m6_mvno_5g)
  )
)

spec_compare <- tibble(
  Model = c(
    "Main: MVNO x 5G",
    "Main + linear time trend",
    "Main + quadratic time trend"
  ),
  R2 = c(
    summary(m6_mvno_5g)$r.squared,
    summary(m7_mvno_5g_time)$r.squared,
    summary(m8_mvno_5g_time2)$r.squared
  ),
  Adj_R2 = c(
    summary(m6_mvno_5g)$adj.r.squared,
    summary(m7_mvno_5g_time)$adj.r.squared,
    summary(m8_mvno_5g_time2)$adj.r.squared
  ),
  AIC = c(
    AIC(m6_mvno_5g),
    AIC(m7_mvno_5g_time),
    AIC(m8_mvno_5g_time2)
  ),
  BIC = c(
    BIC(m6_mvno_5g),
    BIC(m7_mvno_5g_time),
    BIC(m8_mvno_5g_time2)
  )
)

print(compare_level)
print(compare_log)
print(spec_compare)

write_csv(compare_level, here("outputs", "tables", "model_comparison_level.csv"))
write_csv(compare_log, here("outputs", "tables", "model_comparison_log.csv"))
write_csv(spec_compare, here("outputs", "tables", "time_trend_robustness_check.csv"))


level_models <- list(
  "Simple" = m1_simple,
  "Multiple" = m2_level
)

log_models <- list(
  "Basic log" = m3_log,
  "Post5G" = m4_post5g,
  "MVNO x Post5G" = m5_mvno_post5g,
  "MVNO x 5G" = m6_mvno_5g
)

time_models <- list(
  "Main" = m6_mvno_5g,
  "Main + time" = m7_mvno_5g_time,
  "Main + time + time^2" = m8_mvno_5g_time2
)

modelsummary(
  level_models,
  stars = TRUE,
  output = here("outputs", "tables", "regression_results_level.html")
)

modelsummary(
  log_models,
  stars = TRUE,
  output = here("outputs", "tables", "regression_results_log.html")
)

modelsummary(
  time_models,
  stars = TRUE,
  output = here("outputs", "tables", "time_trend_regression_results.html")
)


save_lm_diagnostic_plot <- function(model, filename) {
  png(
    filename = here("outputs", "figures", filename),
    width = 1200,
    height = 900,
    res = 150
  )
  
  par(mfrow = c(2, 2))
  plot(model)
  dev.off()
}

save_lm_diagnostic_plot(m2_level, "diagnostic_m2_level.png")
save_lm_diagnostic_plot(m3_log, "diagnostic_m3_log.png")
save_lm_diagnostic_plot(m5_mvno_post5g, "diagnostic_m5_mvno_post5g.png")
save_lm_diagnostic_plot(m6_mvno_5g, "diagnostic_m6_mvno_5g.png")
save_lm_diagnostic_plot(m7_mvno_5g_time, "diagnostic_m7_mvno_5g_time.png")


safe_p <- function(expr) {
  tryCatch(
    expr,
    error = function(e) NA_real_
  )
}

get_max_vif <- function(model) {
  v <- tryCatch(
    car::vif(model),
    error = function(e) NA_real_
  )
  
  if (length(v) == 1 && is.na(v)) {
    return(NA_real_)
  }
  
  if (is.matrix(v)) {
    return(max(v[, "GVIF^(1/(2*Df))"], na.rm = TRUE))
  } else {
    return(max(as.numeric(v), na.rm = TRUE))
  }
}

diagnostic_models <- list(
  "Level multiple" = m2_level,
  "Basic log" = m3_log,
  "Post5G" = m4_post5g,
  "MVNO x Post5G" = m5_mvno_post5g,
  "MVNO x 5G" = m6_mvno_5g,
  "MVNO x 5G + time" = m7_mvno_5g_time
)

diagnostic_table <- imap_dfr(
  diagnostic_models,
  function(model, model_name) {
    tibble(
      Model = model_name,
      RESET_p_value = safe_p(resettest(model)$p.value),
      BP_p_value = safe_p(bptest(model)$p.value),
      White_type_p_value = safe_p(
        bptest(model, ~ fitted(model) + I(fitted(model)^2))$p.value
      ),
      JB_p_value = safe_p(
        jarque.bera.test(residuals(model))$p.value
      ),
      DW_p_value = safe_p(
        dwtest(model)$p.value
      ),
      BG_order1_p_value = safe_p(
        bgtest(model, order = 1)$p.value
      ),
      BG_order4_p_value = safe_p(
        bgtest(model, order = 4)$p.value
      ),
      Max_VIF = get_max_vif(model)
    )
  }
)

print(diagnostic_table)

write_csv(
  diagnostic_table,
  here("outputs", "tables", "diagnostic_test_summary.csv")
)


robust_models <- list(
  "Level multiple" = m2_level,
  "Basic log" = m3_log,
  "Post5G" = m4_post5g,
  "MVNO x Post5G" = m5_mvno_post5g,
  "MVNO x 5G" = m6_mvno_5g
)

robust_vcov <- lapply(
  robust_models,
  function(model) vcovHC(model, type = "HC1")
)

modelsummary(
  robust_models,
  vcov = robust_vcov,
  stars = TRUE,
  output = here("outputs", "tables", "robust_standard_errors.html")
)

# Quarterly data are used, so lag = 4 allows for within-year serial correlation.

main_model <- m6_mvno_5g
robust_model_time <- m7_mvno_5g_time

main_nw <- coeftest(
  main_model,
  vcov = NeweyWest(
    main_model,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  )
)

time_nw <- coeftest(
  robust_model_time,
  vcov = NeweyWest(
    robust_model_time,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  )
)

print(main_nw)
print(time_nw)

coeftest_to_table <- function(ct) {
  mat <- as.matrix(ct)
  
  tibble(
    Variable = rownames(mat),
    Estimate = mat[, 1],
    NW_SE = mat[, 2],
    t_value = mat[, 3],
    p_value = mat[, 4]
  )
}

nw_main_table <- coeftest_to_table(main_nw)
nw_time_table <- coeftest_to_table(time_nw)

print(nw_main_table)
print(nw_time_table)

write_csv(
  nw_main_table,
  here("outputs", "tables", "newey_west_main_model.csv")
)

write_csv(
  nw_time_table,
  here("outputs", "tables", "newey_west_time_trend_model.csv")
)

nw_models <- list(
  "Main: MVNO x 5G" = main_model,
  "Main + time trend" = robust_model_time
)

nw_vcov <- list(
  NeweyWest(
    main_model,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  ),
  NeweyWest(
    robust_model_time,
    lag = 4,
    prewhite = FALSE,
    adjust = TRUE
  )
)

modelsummary(
  nw_models,
  vcov = nw_vcov,
  stars = TRUE,
  output = here("outputs", "tables", "newey_west_results.html")
)


final_diagnostic <- diagnostic_table %>%
  filter(Model %in% c("MVNO x 5G", "MVNO x 5G + time")) %>%
  left_join(
    tibble(
      Model = c("MVNO x 5G", "MVNO x 5G + time"),
      Adj_R2 = c(
        summary(main_model)$adj.r.squared,
        summary(robust_model_time)$adj.r.squared
      ),
      AIC = c(
        AIC(main_model),
        AIC(robust_model_time)
      ),
      BIC = c(
        BIC(main_model),
        BIC(robust_model_time)
      )
    ),
    by = "Model"
  ) %>%
  select(
    Model,
    Adj_R2,
    AIC,
    BIC,
    RESET_p_value,
    BP_p_value,
    White_type_p_value,
    JB_p_value,
    DW_p_value,
    BG_order1_p_value,
    BG_order4_p_value,
    Max_VIF
  )

print(final_diagnostic)

write_csv(
  final_diagnostic,
  here("outputs", "tables", "final_model_diagnostic.csv")
)


# This is a model-based prediction exercise.
# It should not be interpreted as a causal policy effect.

final_model <- main_model

base <- analysis_df %>%
  slice(n())

scenario_values <- c(20, 25, 30)

sim_data <- bind_rows(
  base %>%
    mutate(
      scenario = "Baseline: latest observed",
      mvno_share = mvno_share
    ),
  base %>%
    slice(rep(1:n(), each = length(scenario_values))) %>%
    mutate(
      scenario = paste0("MVNO ", scenario_values, "%"),
      mvno_share = scenario_values
    )
) %>%
  mutate(
    mvno_c = mvno_share - mean(analysis_df$mvno_share, na.rm = TRUE),
    fiveg_c = fiveg_share - mean(analysis_df$fiveg_share, na.rm = TRUE),
    mvno_c_fiveg_c = mvno_c * fiveg_c
  )

sim_data$pred_log_tel_exp <- predict(final_model, newdata = sim_data)

smearing_factor <- mean(exp(residuals(final_model)), na.rm = TRUE)

sim_data$pred_tel_exp <- exp(sim_data$pred_log_tel_exp) * smearing_factor

baseline_prediction <- sim_data$pred_tel_exp[sim_data$scenario == "Baseline: latest observed"][1]

simulation_result <- sim_data %>%
  mutate(
    change_from_baseline = pred_tel_exp - baseline_prediction,
    pct_change_from_baseline = 100 * (pred_tel_exp / baseline_prediction - 1)
  ) %>%
  select(
    scenario,
    mvno_share,
    income,
    fiveg_share,
    household_size,
    pred_log_tel_exp,
    pred_tel_exp,
    change_from_baseline,
    pct_change_from_baseline
  )

print(simulation_result)

write_csv(
  simulation_result,
  here("outputs", "tables", "policy_simulation_result.csv")
)


saveRDS(
  list(
    data = analysis_df,
    models = list(
      m1_simple = m1_simple,
      m2_level = m2_level,
      m3_log = m3_log,
      m4_post5g = m4_post5g,
      m5_mvno_post5g = m5_mvno_post5g,
      m6_mvno_5g = m6_mvno_5g,
      m7_mvno_5g_time = m7_mvno_5g_time,
      m8_mvno_5g_time2 = m8_mvno_5g_time2
    ),
    diagnostics = diagnostic_table,
    final_diagnostic = final_diagnostic,
    simulation = simulation_result
  ),
  here("outputs", "analysis_objects.rds")
)

