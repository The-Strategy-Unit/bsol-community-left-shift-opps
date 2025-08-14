create_params_json_file <- function(scheme, scenario_type) {
  base_params <- readr::read_rds(here::here("base_params.rds"))
  mitigator_lookup <- readr::read_rds(here::here("mitigator_lookup.rds"))
  n_mitigators <- nrow(mitigator_lookup)
  scenario <- glue::glue("bsol-ndg3-{scenario_type}")
  run_dttm <- substr(sub(" ", "_", gsub("[:-]", "", Sys.time())), 1L, 15L)
  custom_params <- create_custom_params(
    scheme,
    scenario,
    create_datetime = run_dttm
  )
  strategy_intervals_data <- if (scenario_type == "ambitious") {
    readr::read_rds(here::here("ambitious_intervals_data.rds"))
  } else if (scenario_type == "planned") {
    readr::read_rds(here::here("planned_intervals_data.rds"))
  } else if (scenario_type == "steady") {
    mitigator_lookup |>
      dplyr::mutate(interval = purrr::map(seq(n_mitigators), \(x) c(1, 1)))
  }
  filename <- glue::glue("{tolower(scheme)}-{scenario_type}-{run_dttm}.json")
  json_out <- here::here("json", filename)
  base_params |>
    modify_base_params(custom_params) |>
    negate_covid_adjustment() |>
    modify_demographic_factors_list() |>
    set_ndg3_values() |>
    set_linear_time_profiles(mitigator_lookup) |>
    modify_community_mitigator_params(strategy_intervals_data) |>
    jsonlite::write_json(json_out, pretty = TRUE, auto_unbox = TRUE)
}
