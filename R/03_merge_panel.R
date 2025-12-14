# R/03_merge_state_year_panel.R
# PURPOSE: Merge cleaned employment + minimum wage into a state-year panel
#          and ensure an "effective" minimum wage:
#          effective = max(state minimum wage, federal minimum wage)
#
# INPUTS :
#   data_clean/employment_state_year.csv
#   data_clean/min_wage_state_year.csv
#
# OUTPUT:
#   data_clean/state_year_panel.csv
#   data_clean/state_year_panel_post1990.csv
#
# NOTE: This script is finalized. Any changes may alter analytical results.
# LAST UPDATED: 2025-12-13

library(dplyr)
library(readr)
library(tidyr)
library(tibble)

dir.create("data_clean", showWarnings = FALSE)

emp_path <- "data_clean/employment_state_year.csv"
mw_path  <- "data_clean/min_wage_state_year.csv"

out1 <- "data_clean/state_year_panel.csv"
out2 <- "data_clean/state_year_panel_post1990.csv"

# Exit early if outputs exist (delete to rebuild)
if (file.exists(out1) && file.exists(out2)) {
  message("Outputs already exist:\n- ", out1, "\n- ", out2)
  message("Delete them if you want to rebuild.")
  quit(save = "no", status = 0)
}

# Fail fast if inputs missing
missing_inputs <- c(emp_path, mw_path)[!file.exists(c(emp_path, mw_path))]
if (length(missing_inputs) > 0) {
  stop("Missing input file(s):\n- ", paste(missing_inputs, collapse = "\n- "))
}

# -----------------------
# Load inputs
# -----------------------
emp <- read_csv(emp_path, show_col_types = FALSE) %>%
  mutate(year = as.integer(year))

mw  <- read_csv(mw_path, show_col_types = FALSE) %>%
  mutate(year = as.integer(year))

stopifnot(all(c("state_abbr", "year", "emp_all_employees_annual_avg") %in% names(emp)))
stopifnot(all(c("state_abbr", "year") %in% names(mw)))

# -----------------------
# Guardrails: uniqueness (one row per state-year)
# -----------------------
emp_dups <- emp %>% count(state_abbr, year) %>% filter(n > 1)
if (nrow(emp_dups) > 0) {
  stop("Employment file has duplicate state-year rows. Example:\n",
       paste(capture.output(print(head(emp_dups, 10))), collapse = "\n"))
}

mw_dups <- mw %>% count(state_abbr, year) %>% filter(n > 1)
if (nrow(mw_dups) > 0) {
  stop("Min wage file has duplicate state-year rows. Example:\n",
       paste(capture.output(print(head(mw_dups, 10))), collapse = "\n"))
}

# -----------------------
# Normalize minimum wage columns WITHOUT rename collisions
# Prefer existing columns if present.
# Expected (from R/02): min_wage_state, fed_min_wage, min_wage_effective
# -----------------------
# 1) state MW column
state_candidates <- c("min_wage_state", "mw_state", "mw", "min_wage")
state_col <- state_candidates[state_candidates %in% names(mw)][1]

if (is.na(state_col)) {
  # Allow state MW to be missing entirely if file only contains effective + federal
  mw$min_wage_state <- NA_real_
} else if (state_col != "min_wage_state") {
  # Create a new standardized column (no renaming -> avoids duplicates)
  mw <- mw %>% mutate(min_wage_state = .data[[state_col]])
}

# 2) federal MW column
fed_candidates <- c("fed_min_wage", "min_wage_federal", "fed_mw")
fed_col <- fed_candidates[fed_candidates %in% names(mw)][1]

if (!is.na(fed_col) && fed_col != "fed_min_wage") {
  mw <- mw %>% mutate(fed_min_wage = .data[[fed_col]])
}

# 3) effective MW column (preferred if already computed)
eff_candidates <- c("min_wage_effective", "mw_effective")
eff_col <- eff_candidates[eff_candidates %in% names(mw)][1]

if (!is.na(eff_col) && eff_col != "min_wage_effective") {
  mw <- mw %>% mutate(min_wage_effective = .data[[eff_col]])
}

# Coerce numeric where relevant
mw <- mw %>%
  mutate(
    min_wage_state     = suppressWarnings(as.numeric(min_wage_state)),
    fed_min_wage       = if ("fed_min_wage" %in% names(mw)) suppressWarnings(as.numeric(fed_min_wage)) else NA_real_,
    min_wage_effective = if ("min_wage_effective" %in% names(mw)) suppressWarnings(as.numeric(min_wage_effective)) else NA_real_
  )

# -----------------------
# If effective MW is missing, compute it.
# If federal MW is missing, build schedule (only for years in employment panel).
# -----------------------
yr_min <- min(emp$year, na.rm = TRUE)
yr_max <- max(emp$year, na.rm = TRUE)

if (!("fed_min_wage" %in% names(mw)) || all(is.na(mw$fed_min_wage))) {
  
  fed_changes <- tribble(
    ~start_year, ~end_year, ~fed_min_wage,
    1968L, 1973L, 1.60,
    1974L, 1974L, 2.00,
    1975L, 1975L, 2.10,
    1976L, 1977L, 2.30,
    1978L, 1978L, 2.65,
    1979L, 1979L, 2.90,
    1980L, 1980L, 3.10,
    1981L, 1989L, 3.35,
    1990L, 1990L, 3.80,
    1991L, 1995L, 4.25,
    1996L, 1996L, 4.75,
    1997L, 2006L, 5.15,
    2007L, 2007L, 5.85,
    2008L, 2008L, 6.55,
    2009L, 2100L, 7.25
  )
  
  fed_sched <- fed_changes %>%
    rowwise() %>%
    mutate(year = list(seq.int(start_year, end_year))) %>%
    unnest(year) %>%
    ungroup() %>%
    select(year, fed_min_wage) %>%
    filter(year >= yr_min, year <= yr_max) %>%
    arrange(year)
  
  # Merge federal MW onto mw by year (adds fed_min_wage)
  mw <- mw %>%
    select(-any_of("fed_min_wage")) %>%
    left_join(fed_sched, by = "year")
}

# Compute effective MW if missing
if (!("min_wage_effective" %in% names(mw)) || all(is.na(mw$min_wage_effective))) {
  mw <- mw %>%
    mutate(min_wage_effective = pmax(min_wage_state, fed_min_wage, na.rm = TRUE))
}

# -----------------------
# Merge panel
# -----------------------
panel <- emp %>%
  left_join(mw %>% select(state_abbr, year, min_wage_state, fed_min_wage, min_wage_effective),
            by = c("state_abbr", "year")) %>%
  mutate(
    mw_source = case_when(
      is.na(min_wage_state) ~ "federal_only",
      min_wage_state >= fed_min_wage ~ "state",
      TRUE ~ "federal_binding"
    )
  )

# -----------------------
# Diagnostics
# -----------------------
cat("\n--- Effective MW check ---\n")
print(panel %>% summarise(
  n = n(),
  missing_state_mw   = sum(is.na(min_wage_state)),
  missing_federal_mw = sum(is.na(fed_min_wage)),
  missing_effective  = sum(is.na(min_wage_effective))
))

cat("\n--- MW source breakdown (post-1990) ---\n")
print(panel %>%
        filter(year >= 1990) %>%
        count(mw_source) %>%
        mutate(share = n / sum(n)))

cat("\n--- Panel summary (post-1990) ---\n")
print(panel %>%
        filter(year >= 1990) %>%
        summarise(
          states = n_distinct(state_abbr),
          years  = n_distinct(year),
          n      = n(),
          min_year = min(year),
          max_year = max(year)
        ))

# -----------------------
# Write outputs
# -----------------------
write_csv(panel, out1)
cat("Wrote:", out1, "\n")

write_csv(panel %>% filter(year >= 1990), out2)
cat("Wrote:", out2, "\n")
