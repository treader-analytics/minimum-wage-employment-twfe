# Minimum Wage and Employment

**Two-Way Fixed Effects Evidence from U.S. States (Post-1990)**

This repository contains a reproducible applied econometrics analysis examining the relationship between minimum wage policy and total nonfarm employment across U.S. states using a two-way fixed effects (TWFE) panel design.

The project is designed as a transparent, policy-relevant empirical exercise using publicly available data and modern panel data methods in R, with an emphasis on reproducible workflows and clear economic interpretation.

## Motivation

This project was developed to demonstrate applied analytical skills at the intersection of **data analysis, economics, and public policy**. Policy questions such as minimum wage regulation sit at the core of real-world decision-making, where quantitative evidence must be translated into clear, defensible conclusions.

The analysis emphasizes: 
- Rigorous data cleaning and panel construction
- Modern fixed effects estimation
- Interpretation of results in a policy-relevant context
- Fully reproducible analytical workflows

The project is intended to reflect the type of empirical reasoning used in **policy analysis, applied economics, and data-driven decision support roles**, rather than purely theoretical modeling.

## Overview

Understanding the employment effects of minimum wage policy is central to labor market and distributional policy debates. This project estimates the association between state-level minimum wage changes and total nonfarm employment using:

-   U.S. state–year panel data
-   State and year fixed effects
-   State-clustered standard errors
-   Robustness checks with state-specific linear trends

The analysis focuses on the post-1990 period to ensure consistency in employment measurement following changes in BLS CES series definitions.

## Data Sources

All data used in this project are publicly available.

**Employment**

Bureau of Labor Statistics (BLS), *Current Employment Statistics (CES)*
Aggregated to the state–year level.

**Minimum Wage**

Federal Reserve Economic Data (FRED)
Minimum wage series used in the project pipeline for each state-year.

Cleaned panel data used for estimation is stored in:

```         
data_clean/state_year_panel_post1990.csv
```

## Empirical Strategy

The baseline specification uses a two-way fixed effects (TWFE) model of the form:

```         
log(emp_st) = β · log(mw_st) + α_s + γ_t + ε_st
```

where: 
- α_s captures time-invariant state characteristics
- γ_t captures common macroeconomic shocks
- ε_st is the idiosyncratic error term

Identification comes from differential timing and magnitude of minimum wage changes across states, net of state and year fixed effects.

Additional specifications include: 
- Log–level models
- Level models
- State-specific linear time trends (robustness check)

Standard errors are clustered at the state level throughout.

## Key Findings

Across specifications, estimated effects of minimum wage changes on total employment are small in magnitude and statistically indistinguishable from zero in baseline TWFE models. Including state-specific linear trends further attenuates estimates, consistent with much of the recent empirical literature.

These results should be interpreted as **reduced-form average effects**, not structural labor demand estimates.

## Repository Structure

```         
├── data_clean
│   └── state_year_panel_post1990.csv
├── reports
│   └── 04_twfe_report.Rmd
├── scripts
│   └── (data cleaning and construction scripts)
└── README.md
```

## Reproducibility

The analysis is fully reproducible using the included data and scripts.

To render the main report:

``` r
rmarkdown::render("reports/04_twfe_report.Rmd")
```

All results, tables, and figures in the report are generated directly from the project pipeline.

## Tools & Packages

Key R packages used include: 
- `fixest` — high-performance fixed effects estimation
- `dplyr`, `readr`, `tibble` — data manipulation
- `ggplot2` — visualization
- `rmarkdown` — reproducible reporting

## Author

**Trevor Reader**
B.A. Mathematics & Statistics
Applied Econometrics • Policy Analysis • Data-Driven Decision Support

## Notes

This project is intended as a portfolio-quality applied econometrics example rather than a definitive causal evaluation. It is designed to demonstrate analytical judgment, methodological rigor, and the ability to translate quantitative results into policy-relevant insights.
