# source() all functions from files under `R/`
withr::with_options(list(verbose = FALSE), {
  purrr::walk(dir("R", "\\.R$"), \(x) source(here::here("R", x)))
})

base_params <- get_base_params()
ambitious_intervals_data <- compile_ambitious_intervals()
planned_intervals_data <- compile_planned_intervals()
mitigator_lookup <- get_mitigator_lookup()

readr::write_rds(base_params, "base_params.rds")
readr::write_rds(ambitious_intervals_data, "ambitious_intervals_data.rds")
readr::write_rds(planned_intervals_data, "planned_intervals_data.rds")
readr::write_rds(mitigator_lookup, "mitigator_lookup.rds")

# example (repeat for all 6 combinations)
create_params_json_file("RRK", "ambitious")
post_params_to_api(here::here("json", "rrk-ambitious-20250709_151753.json"))
