# R/02_clean_min_wage.R
# PURPOSE: Clean raw FRED min wage data into a state-year panel
# INPUT  : data_raw/fred_state_min_wage_raw.csv
# OUTPUT : data_clean/min_wage_state_year.csv
#
# NOTE: This script is finalized. Any changes may alter analytical results.
# LAST UPDATED: 2025-12-13

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(tibble)

# -----------------------
# Paths
# -----------------------
infile  <- "data_raw/fred_state_min_wage_raw.csv"
outfile <- "data_clean/min_wage_state_year.csv"

dir.create("data_clean", showWarnings = FALSE)

# Exit early if output exists (delete to rebuild)
if (file.exists(outfile)) {
  message("File already exists: ", outfile)
  message("Delete it if you want to rebuild.")
  quit(save = "no", status = 0)
}

# Fail fast if input missing
if (!file.exists(infile)) {
  stop("Missing input file: ", infile, "\nRun R/00_download_fred_min_wage.R first.")
}

# -----------------------
# Load raw data
# -----------------------
mw_raw <- read_csv(infile, show_col_types = FALSE)

stopifnot(all(c("series_id", "date", "value") %in% names(mw_raw)))

# -----------------------
# Parse -> state_abbr, year, numeric wage
# - FRED may have multiple observations per year: collapse to annual mean.
# -----------------------
mw <- mw_raw %>%
  mutate(
    # Most state MW series end in the 2-letter abbreviation
    state_abbr = str_extract(series_id, "[A-Z]{2}$"),
    year       = year(as.Date(date)),
    mw_value   = suppressWarnings(as.numeric(value))
  ) %>%
  filter(!is.na(state_abbr), !is.na(year), !is.na(mw_value)) %>%
  group_by(state_abbr, year) %>%
  summarise(
    min_wage_state = mean(mw_value, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------
# Map abbreviations to full names (+ DC)
# -----------------------
state_map <- tibble(
  state_abbr = c(state.abb, "DC"),
  state      = c(state.name, "District of Columbia")
)

mw <- mw %>%
  left_join(state_map, by = "state_abbr") %>%
  relocate(state, .before = state_abbr)

# -----------------------
# Federal minimum wage ranges (condensed)
# - Define "start year" with wage; carry forward until next start year.
# -----------------------
fed_ranges <- tibble(
  start_year = c(
    1938, 1939, 1945, 1950, 1956,
    1961, 1963, 1967, 1968,
    1974, 1975, 1976, 1978, 1979,
    1980, 1981,
    1990, 1991,
    1996, 1997,
    2007, 2008, 2009
  ),
  fed_min_wage = c(
    0.25, 0.30, 0.40, 0.75, 1.00,
    1.15, 1.25, 1.40, 1.60,
    2.00, 2.10, 2.30, 2.65, 2.90,
    3.10, 3.35,
    3.80, 4.25,
    4.75, 5.15,
    5.85, 6.55, 7.25
  )
) %>%
  arrange(start_year) %>%
  mutate(
    end_year = lead(start_year, default = Inf) - 1
  )

# Helper: create fed schedule across the observed state-year span
year_min <- min(mw$year, na.rm = TRUE)
year_max <- max(mw$year, na.rm = TRUE)

fed_full <- tibble(year = seq(year_min, year_max)) %>%
  left_join(
    fed_ranges %>%
      select(start_year, end_year, fed_min_wage),
    by = character()
  ) %>%
  filter(year >= start_year, year <= end_year) %>%
  select(year, fed_min_wage)

# -----------------------
# Merge + compute effective/binding minimum wage
# -----------------------
mw_clean <- mw %>%
  left_join(fed_full, by = "year") %>%
  mutate(
    # Effective minimum wage (binding): max(state, federal)
    min_wage_effective = pmax(min_wage_state, fed_min_wage, na.rm = TRUE)
  ) %>%
  arrange(state_abbr, year)

# -----------------------
# Write output
# -----------------------
write_csv(mw_clean, outfile)

# -----------------------
# Sanity checks
# -----------------------
summary_tbl <- mw_clean %>%
  summarise(
    states = n_distinct(state_abbr),
    years  = n_distinct(year),
    n      = n(),
    min_year = min(year),
    max_year = max(year),
    missing_state_names = sum(is.na(state)),
    missing_federal = sum(is.na(fed_min_wage)),
    missing_effective = sum(is.na(min_wage_effective))
  )

print(summary_tbl)
cat("Wrote:", outfile, "\n")

# Optional: quick check of the condensed federal schedule (first few rows)
cat("\n--- Federal MW ranges used (condensed) ---\n")
print(fed_ranges %>% filter(start_year >= year_min, start_year <= year_max))
