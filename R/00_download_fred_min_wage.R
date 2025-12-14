# R/00_download_fred_min_wage.R
# PURPOSE: Download raw state minimum wage data from FRED (Release 387)
# OUTPUT : data_raw/fred_state_min_wage_raw.csv
# NOTES  :
# - Requires FRED_API_KEY in your environment.
# - Uses release/observations pagination with backoff.
# - Output is a long table: series_id, date, value (value kept as character).
# - Script exits early if output file already exists.

library(httr2)
library(jsonlite)
library(dplyr)
library(tidyr)
library(readr)

# -----------------------
# Config
# -----------------------
release_id <- 387
base_url   <- "https://api.stlouisfed.org/fred/v2/release/observations"
out_path   <- "data_raw/fred_state_min_wage_raw.csv"

dir.create("data_raw", showWarnings = FALSE)

# Exit early if file already exists
if (file.exists(out_path)) {
  message("File already exists: ", out_path)
  message("Delete it if you want to re-download.")
  quit(save = "no", status = 0)
}

key <- trimws(Sys.getenv("FRED_API_KEY"))
if (nchar(key) == 0) {
  stop(
    "Missing FRED_API_KEY.\n",
    "Set it in your environment, e.g.:\n",
    "Sys.setenv(FRED_API_KEY = 'YOUR_KEY_HERE')"
  )
}

# -----------------------
# Helpers
# -----------------------
get_retry_after <- function(resp) {
  h <- resp_headers(resp)
  ra <- h[["retry-after"]]
  if (is.null(ra)) return(NA_real_)
  suppressWarnings(as.numeric(ra))
}

fetch_page <- function(next_cursor = NULL, limit = 1000,
                       max_tries = 8, base_sleep = 1) {
  tries <- 0
  
  repeat {
    tries <- tries + 1
    
    req <- request(base_url) |>
      req_url_query(
        release_id = release_id,
        api_key    = key,
        file_type  = "json",
        limit      = limit
      )
    
    if (!is.null(next_cursor)) {
      req <- req |> req_url_query(next_cursor = next_cursor)
    }
    
    resp <- try(req_perform(req), silent = TRUE)
    
    # Network error / timeout
    if (inherits(resp, "try-error")) {
      if (tries >= max_tries) stop(resp)
      sleep_s <- min(60, base_sleep * 2^(tries - 1))
      message("Request error; retrying in ", sleep_s, "s (try ", tries, "/", max_tries, ")")
      Sys.sleep(sleep_s)
      next
    }
    
    status <- resp_status(resp)
    
    if (status == 200) {
      parsed <- fromJSON(resp_body_string(resp), flatten = TRUE)
      
      # Validate expected structure
      if (!("series" %in% names(parsed))) {
        stop(
          "Unexpected response schema. Top-level fields: ",
          paste(names(parsed), collapse = ", ")
        )
      }
      
      return(parsed)
    }
    
    if (status %in% c(429, 503)) {
      if (tries >= max_tries) {
        cat("Status:", status, "\n")
        cat(substr(resp_body_string(resp), 1, 1200), "\n")
        stop("Too many retries; still rate-limited.")
      }
      
      ra <- get_retry_after(resp)
      sleep_s <- if (!is.na(ra)) ra else min(60, base_sleep * 2^(tries - 1))
      message("HTTP ", status, " — backing off ", sleep_s, "s (try ", tries, "/", max_tries, ")")
      Sys.sleep(sleep_s)
      next
    }
    
    cat("Status:", status, "\n")
    cat(substr(resp_body_string(resp), 1, 1200), "\n")
    stop("FRED download failed.")
  }
}

# -----------------------
# Paginate
# -----------------------
pages  <- list()
cursor <- NULL
i      <- 1

repeat {
  message("Fetching page ", i, " ...")
  res <- fetch_page(next_cursor = cursor, limit = 1000)
  
  pages[[i]] <- res$series
  
  # Courtesy pause to reduce chance of 429
  Sys.sleep(0.5)
  
  if (!isTRUE(res$has_more)) break
  cursor <- res$next_cursor
  i <- i + 1
}

# -----------------------
# Flatten to raw table
# -----------------------
mw_raw <- bind_rows(pages) |>
  select(series_id, observations) |>
  unnest(observations) |>
  transmute(
    series_id = as.character(series_id),
    date      = as.Date(date),
    value     = as.character(value)
  )

write_csv(mw_raw, out_path)

cat("\nSaved:", out_path, "\n")
cat("Rows:", nrow(mw_raw), "\n")
cat("Series:", n_distinct(mw_raw$series_id), "\n")
cat("Date range:", format(min(mw_raw$date)), "to", format(max(mw_raw$date)), "\n")
