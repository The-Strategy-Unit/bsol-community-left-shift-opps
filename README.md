# BSol WP2 data report

Modelled potential avoidable activity by 2035-36, Birmingham & Solihull ICB
(BSol) .

Charts to show potential activity avoidance across 29 community services-based
mitigators as applied to hospital sites within the BSol area.

## Outline

1. Custom parameters JSON files were generated and passed to the NHP API v2
1. The generated results files were downloaded from Azure storage and processed
1. The processed data was then prepared in various formats as the basis for the charts

## Reproducibility

* Data preparation functions can be found in the `R` sub-directory
* Various environment variables must be provided, e.g. in a `.Renviron` file
* The data preparation process is summarised in
`init_create_and_post_custom_params.R`
* The data preparation and chart production is handled within the `bsol_wp2_report.qmd` file.
