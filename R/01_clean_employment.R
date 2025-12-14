# R/01_clean_employment.R
# PURPOSE: Clean BLS SM (State & Metro CES) data into a state-year panel
# INPUTS :
#   data_raw/sm.data.1.AllData.txt
#   data_raw/sm.series.txt
#   data_raw/sm.state.txt
#   data_raw/sm.period.txt
# OUTPUT:
#   data_clean/employment_state_year.csv
#
# Target:
#   Total Nonfarm (industry_code == "00000000")
#   All Employees (data_type_code == "01")
#
# KEY FIX:
#   Enforce ONE seasonal definition to avoid mixing series over time.
#
# NOTE: This script is finalized. Any changes may alter analytical results.
# LAST UPDATED: 2025-12-13

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(tibble)

dir.create("data_clean", showWarnings = FALSE)

data_file   <- "data_raw/sm.data.1.AllData.txt"
series_file <- "data_raw/sm.series.txt"
state_file  <- "data_raw/sm.state.txt"
period_file <- "data_raw/sm.period.txt"

out_path <- "data_clean/employment_state_year.csv"

# Exit early if output already exists (delete to rebuild)
if (file.exists(out_path)) {
  message("File already exists: ", out_path)
  message("Delete it if you want to rebuild.")
  quit(save = "no", status = 0)
}

# Input existence checks (fail fast with clear message)
inputs <- c(data_file, series_file, state_file, period_file)
missing_inputs <- inputs[!file.exists(inputs)]
if (length(missing_inputs) > 0) {
  stop("Missing input file(s):\n- ", paste(missing_inputs, collapse = "\n- "))
}

# -----------------------
# Robust reader (auto-detect tab/comma/whitespace)
# -----------------------
detect_delim <- function(path) {
  first_line <- readLines(path, n = 1, warn = FALSE)
  if (length(first_line) == 0) stop("File appears empty: ", path)
  
  if (grepl("\t", first_line)) return("\t")
  if (grepl(",", first_line))  return(",")
  return("ws")
}

read_bls <- function(path) {
  d <- detect_delim(path)
  
  if (d == "ws") {
    df <- read_table(
      file = path,
      col_names = TRUE,
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE
    )
  } else {
    df <- read_delim(
      file = path,
      delim = d,
      col_names = TRUE,
      trim_ws = TRUE,
      show_col_types = FALSE,
      progress = FALSE
    )
  }
  
  names(df) <- names(df) |>
    tolower() |>
    str_replace_all("\\s+", "_")
  
  df
}

map_col <- function(df, options) {
  hit <- intersect(names(df), options)
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

# -----------------------
# Load raw files
# -----------------------
data    <- read_bls(data_file)
series  <- read_bls(series_file)
states  <- read_bls(state_file)
periods <- read_bls(period_file)  # kept for completeness

# -----------------------
# Standardize sm.data columns
# -----------------------
col_series_id <- map_col(data, c("series_id", "seriesid"))
col_year      <- map_col(data, c("year", "yr"))
col_period    <- map_col(data, c("period", "per"))
col_value     <- map_col(data, c("value", "val"))

if (any(is.na(c(col_series_id, col_year, col_period, col_value)))) {
  message("Could not find expected columns in sm.data file.")
  message("Found columns: ", paste(names(data), collapse = ", "))
  stop("Update column mappings (series/year/period/value) based on your file.")
}

data <- data %>%
  rename(
    series_id = all_of(col_series_id),
    year      = all_of(col_year),
    period    = all_of(col_period),
    value     = all_of(col_value)
  )

# -----------------------
# Keep monthly observations only (M01..M12)
# -----------------------
data_m <- data %>%
  mutate(
    year  = as.integer(year),
    value = suppressWarnings(as.numeric(value))
  ) %>%
  filter(
    str_detect(period, "^M\\d{2}$"),
    !is.na(value)
  )

# -----------------------
# Join series metadata
# -----------------------
needed_series <- c("series_id", "state_code", "industry_code", "data_type_code", "seasonal")
missing_series <- setdiff(needed_series, names(series))

if (length(missing_series) > 0) {
  message("Series table missing: ", paste(missing_series, collapse = ", "))
  message("Series columns: ", paste(names(series), collapse = ", "))
  stop("Update series column mappings to match sm.series.txt.")
}

df <- data_m %>%
  left_join(series %>% select(any_of(needed_series)), by = "series_id") %>%
  mutate(
    seasonal = toupper(trimws(as.character(seasonal))),
    industry_code = as.character(industry_code),
    data_type_code = as.character(data_type_code),
    state_code = as.character(state_code)
  )

# -----------------------
# Map state_code -> state name using sm.state.txt
# -----------------------
if (!all(c("state_code", "state_name") %in% names(states))) {
  message("sm.state.txt does not have expected columns state_code/state_name.")
  message("State columns: ", paste(names(states), collapse = ", "))
  stop("Update mapping based on your sm.state.txt column names.")
}

states_map <- states %>%
  transmute(
    state_code = as.character(state_code),
    state      = as.character(state_name)
  )

df <- df %>%
  left_join(states_map, by = "state_code")

abbr_map <- tibble(
  state      = c(state.name, "District of Columbia"),
  state_abbr = c(state.abb, "DC")
)

df <- df %>%
  left_join(abbr_map, by = "state") %>%
  filter(!is.na(state), !is.na(state_abbr))

# -----------------------
# Diagnostics
# -----------------------
message("Industry codes (sample): ", paste(head(sort(unique(df$industry_code)), 12), collapse = ", "))
message("Data type codes (sample): ", paste(head(sort(unique(df$data_type_code)), 12), collapse = ", "))
message("Seasonal codes (sample): ", paste(head(sort(unique(df$seasonal)), 12), collapse = ", "))

# -----------------------
# Target + seasonal choice (KEY FIX)
# -----------------------
if (!("00000000" %in% unique(df$industry_code))) {
  stop("industry_code '00000000' not found. Adjust filter after inspecting unique(df$industry_code).")
}
if (!("01" %in% unique(df$data_type_code))) {
  stop("data_type_code '01' not found. Adjust filter after inspecting unique(df$data_type_code).")
}

SEASONAL_CHOICE <- "S"  # "S" = seasonally adjusted; if your dump uses "U", change here.

if (!(SEASONAL_CHOICE %in% unique(df$seasonal))) {
  message("Requested seasonal code not found: ", SEASONAL_CHOICE)
  message("Available seasonal codes: ", paste(sort(unique(df$seasonal)), collapse = ", "))
  stop("Set SEASONAL_CHOICE to one of the available codes.")
}

target <- df %>%
  filter(
    industry_code == "00000000",
    data_type_code == "01",
    seasonal == SEASONAL_CHOICE
  )

# Guardrail: ensure at most 1 observation per state-year-month; if not, collapse safely
dups <- target %>%
  count(state_abbr, year, period) %>%
  filter(n > 1)

if (nrow(dups) > 0) {
  message("WARNING: Multiple series per state-year-month after filtering. Collapsing by mean(value).")
  print(head(dups, 10))
}

target_unique <- target %>%
  group_by(state, state_abbr, year, period) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    n_series = n(),
    .groups = "drop"
  )

# -----------------------
# Aggregate monthly -> annual average by state-year
# -----------------------
emp_state_year <- target_unique %>%
  group_by(state, state_abbr, year) %>%
  summarise(
    emp_all_employees_annual_avg = mean(value, na.rm = TRUE),
    n_months = n(),
    avg_series_per_month = mean(n_series),
    .groups = "drop"
  ) %>%
  mutate(incomplete_year = n_months < 12) %>%
  arrange(state, year)

# -----------------------
# Write output + summary
# -----------------------
write_csv(emp_state_year, out_path)

print(emp_state_year %>% summarise(
  states = n_distinct(state_abbr),
  years  = n_distinct(year),
  n      = n(),
  incomplete_years = sum(incomplete_year, na.rm = TRUE),
  min_year = min(year),
  max_year = max(year),
  avg_series_per_month_overall = mean(avg_series_per_month, na.rm = TRUE)
))

cat("\n--- Avg employment around 1988–1992 (sanity check) ---\n")
print(emp_state_year %>%
        group_by(year) %>%
        summarise(avg_emp = mean(emp_all_employees_annual_avg, na.rm = TRUE), .groups = "drop") %>%
        filter(year %in% c(1988, 1989, 1990, 1991, 1992)))

cat("\nWrote:", out_path, "\n")
