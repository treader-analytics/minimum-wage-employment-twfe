# R/04_models_and_plots.R
# PURPOSE: Run baseline TWFE models + export results + plots (post-1990 sample)
# INPUT  :
#   data_clean/state_year_panel_post1990.csv
# OUTPUT :
#   outputs/regression_results.csv
#   outputs/coef_plot.png
#   outputs/trends_plot.png
#
# NOTE: This script is finalized. Any changes may alter analytical results.
# LAST UPDATED: 2025-12-13

library(dplyr)
library(readr)
library(ggplot2)
library(fixest)
library(tibble)

dir.create("outputs", showWarnings = FALSE)

infile <- "data_clean/state_year_panel_post1990.csv"
out_results <- "outputs/regression_results.csv"
out_coef    <- "outputs/coef_plot.png"
out_trends  <- "outputs/trends_plot.png"

# Exit early if outputs already exist (delete to rebuild)
if (file.exists(out_results) && file.exists(out_coef) && file.exists(out_trends)) {
  message("Outputs already exist:")
  message("- ", out_results)
  message("- ", out_coef)
  message("- ", out_trends)
  message("Delete them if you want to rebuild.")
  quit(save = "no", status = 0)
}

# Fail fast if input missing
if (!file.exists(infile)) {
  stop("Missing input file: ", infile, "\nRun merge script (R/03_merge_state_year_panel.R) first.")
}

# -----------------------
# Load data
# -----------------------
df <- read_csv(infile, show_col_types = FALSE)

stopifnot(all(c(
  "state_abbr",
  "year",
  "emp_all_employees_annual_avg",
  "min_wage_effective"
) %in% names(df)))

# -----------------------
# Variables
# -----------------------
df <- df %>%
  mutate(
    year   = as.integer(year),
    emp    = as.numeric(emp_all_employees_annual_avg),
    mw     = as.numeric(min_wage_effective),
    ln_emp = log(emp),
    ln_mw  = log(mw)
  ) %>%
  filter(is.finite(ln_emp), is.finite(ln_mw))

# -----------------------
# Descriptives
# -----------------------
print(df %>% summarise(
  states   = n_distinct(state_abbr),
  years    = n_distinct(year),
  n        = n(),
  min_year = min(year),
  max_year = max(year)
))

# -----------------------
# Models (TWFE, clustered by state)
# -----------------------
m1 <- feols(
  ln_emp ~ ln_mw | state_abbr + year,
  cluster = ~state_abbr,
  data = df
)

m2 <- feols(
  emp ~ mw | state_abbr + year,
  cluster = ~state_abbr,
  data = df
)

m3 <- feols(
  ln_emp ~ mw | state_abbr + year,
  cluster = ~state_abbr,
  data = df
)

# Optional robustness (DO NOT include in baseline outputs unless you want it)
# m1_trend <- feols(
#   ln_emp ~ ln_mw | state_abbr + year + state_abbr[year],
#   cluster = ~state_abbr,
#   data = df
# )

# -----------------------
# Extractors (robust across fixest versions)
# -----------------------
safe_nobs <- function(m) {
  tryCatch(nobs(m), error = function(e) {
    tryCatch(length(fixest::resid(m)), error = function(e2) NA_integer_)
  })
}

get_within_r2 <- function(m) {
  s <- summary(m)
  
  # Some fixest versions store within R2 here
  if (!is.null(s$r2_within)) return(unname(s$r2_within))
  
  # Others store r.squared as a named vector
  if (!is.null(s$r.squared)) {
    if ("within" %in% names(s$r.squared)) return(unname(s$r.squared["within"]))
    if ("w" %in% names(s$r.squared))      return(unname(s$r.squared["w"]))
  }
  
  NA_real_
}

tidy_fixest <- function(model_obj, model_name) {
  ct <- as.data.frame(coeftable(model_obj))
  
  tibble(
    model_label = model_name,
    term        = rownames(ct),
    estimate    = ct$Estimate,
    std_error   = ct$`Std. Error`,
    statistic   = ct$`t value`,
    p_value     = ct$`Pr(>|t|)`,
    nobs        = safe_nobs(model_obj),
    r2_within   = get_within_r2(model_obj)
  )
}

# -----------------------
# Collect results + write CSV
# -----------------------
results <- bind_rows(
  tidy_fixest(m1, "TWFE: ln(emp) ~ ln(mw_effective)"),
  tidy_fixest(m2, "TWFE: emp ~ mw_effective"),
  tidy_fixest(m3, "TWFE: ln(emp) ~ mw_effective")
)

write_csv(results, out_results)
cat("Wrote:", out_results, "\n")

# -----------------------
# Coefficient plot (ggplot2 4.0+ compliant)
# -----------------------
plot_terms <- results %>%
  filter(term %in% c("ln_mw", "mw")) %>%
  mutate(
    conf_low  = estimate - 1.96 * std_error,
    conf_high = estimate + 1.96 * std_error
  )

p_coef <- ggplot(plot_terms, aes(x = estimate, y = model_label)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = model_label,
      ymax = model_label,
      xmin = conf_low,
      xmax = conf_high
    ),
    orientation = "y",
    height = 0.2,
    linewidth = 0.6
  ) +
  labs(
    title = "Effect of Minimum Wage on Employment (TWFE)",
    x = "Coefficient (95% CI)",
    y = ""
  ) +
  theme_minimal()

ggsave(out_coef, p_coef, width = 9, height = 4.8, units = "in", dpi = 300)
cat("Wrote:", out_coef, "\n")

# -----------------------
# Trends plot (indexed, 1990 = 100)
# -----------------------
trends <- df %>%
  group_by(year) %>%
  summarise(avg_emp = mean(emp, na.rm = TRUE), .groups = "drop") %>%
  mutate(emp_index = 100 * avg_emp / avg_emp[year == min(year)])

p_trends <- ggplot(trends, aes(x = year, y = emp_index)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 100, linetype = "dashed", alpha = 0.6) +
  labs(
    title = paste0("Average State Employment Over Time (Indexed, ", min(trends$year), " = 100)"),
    x = "Year",
    y = "Employment Index"
  ) +
  theme_minimal()

ggsave(out_trends, p_trends, width = 9, height = 4.8, units = "in", dpi = 300)
cat("Wrote:", out_trends, "\n")

# -----------------------
# Print summaries
# -----------------------
cat("\n--- Model summaries ---\n")
print(summary(m1))
print(summary(m2))
print(summary(m3))
