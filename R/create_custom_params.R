get_base_params <- function() {
  req <- httr2::request("https://api.github.com")
  resp <- req |>
    httr2::req_url_path_append("repos") |>
    httr2::req_url_path_append("The-Strategy-Unit") |>
    httr2::req_url_path_append("nhp_inputs") |>
    httr2::req_url_path_append("contents") |>
    httr2::req_url_path_append("default_params.json") |>
    httr2::req_url_query(ref = "inputs_selection_app") |>
    httr2::req_perform()

  httr2::resp_check_status(resp)
  httr2::resp_check_content_type(resp, "application/json")

  httr2::resp_body_json(resp) |>
    purrr::pluck("content") |>
    # GitHub API returns file content base64-encoded
    base64enc::base64decode() |>
    yyjsonr::read_json_raw()
}


create_custom_params <- function(dataset, scenario, ...) {
  supplied_params <- rlang::dots_list(...)
  nhp_user <- supplied_params[["user"]] %||% Sys.getenv("NHP_API_USER", NA)
  assertthat::assert_that(!is.na(nhp_user))
  param_defaults <- list(
    user = nhp_user,
    model_runs = 256L,
    start_year = 2023L,
    end_year = 2035L,
    app_version = Sys.getenv("NHP_VERSION", "dev"),
    viewable = FALSE,
    health_status_adjustment = TRUE
  )
  param_defaults |>
    purrr::list_modify(dataset = dataset, scenario = scenario) |>
    purrr::list_modify(!!!supplied_params)
}


insert_strategy_intervals <- function(lst, interval_data) {
  interval_data_lst <- interval_data |>
    tidyr::nest(.by = "change_factor") |>
    tibble::deframe() |>
    purrr::map(\(x) tidyr::nest(x, .by = "type")) |>
    purrr::map(tibble::deframe) |>
    purrr::map_depth(2, tibble::deframe) |>
    # efficiencies have to have a type as well as an interval
    purrr::modify_at("efficiencies", \(x) {
      x |>
        # most have type = "all"...
        purrr::map_depth(2, \(x) purrr::list_merge(x, type = "all")) |>
        # but some are different
        purrr::modify_at("ip", adjust_ip_efficiencies)
    })
  purrr::list_modify(lst, !!!interval_data_lst)
}


adjust_ip_efficiencies <- function(efficiencies_ip_list) {
  efficiencies_ip_list |>
    purrr::modify_at(\(x) grepl("^same_day_emergency_care", x), \(x) {
      purrr::list_modify(x, type = "sdec")
    }) |>
    purrr::modify_at(\(x) grepl("^day_procedures.*dc$", x), \(x) {
      purrr::list_modify(x, type = "day_procedures_daycase")
    }) |>
    purrr::modify_at(\(x) grepl("^day_procedures.*op$", x), \(x) {
      purrr::list_modify(x, type = "day_procedures_outpatients")
    }) |>
    purrr::modify_at(\(x) grepl("^pre-op_los", x), \(x) {
      purrr::list_modify(x, type = "pre-op")
    }) |>
    purrr::modify_at("pre-op_los_1-day", \(x) {
      purrr::list_merge(x, `pre-op_days` = 1L)
    }) |>
    purrr::modify_at("pre-op_los_2-day", \(x) {
      purrr::list_merge(x, `pre-op_days` = 2L)
    })
}


modify_demographic_factors <- function(lst, ...) {
  default_demogr_list <- list(migration_category = 1L)
  custom_demogr_list <- purrr::list_modify(default_demogr_list, ...)
  lst |>
    purrr::modify_in("demographic_factors", \(x) {
      purrr::assign_in(x, "variant_probabilities", custom_demogr_list)
    })
}


negate_covid_adjustment <- function(lst) {
  # Using NULL fails JSON validation as the schema requires `"minItems": 2`
  # hence here we use `c(1, 1)` which negates adjustment, though this is
  # a less neat solution than just using NULL to generate an empty `{}`.
  op_list <- purrr::map(seq(3), \(x) c(1, 1)) |>
    rlang::set_names(c("first", "followup", "procedure"))
  ip_list <- purrr::map(seq(3), \(x) c(1, 1)) |>
    rlang::set_names(c("elective", "non-elective", "maternity"))
  aae_list <- purrr::map(seq(2), \(x) c(1, 1)) |>
    rlang::set_names(c("ambulance", "walk-in"))
  lst |>
    purrr::modify_at("covid_adjustment", \(x) {
      x |>
        purrr::assign_in("op", op_list) |>
        purrr::assign_in("ip", ip_list) |>
        purrr::assign_in("aae", aae_list)
    })
}


insert_linear_time_profiles <- function(lst, interval_data) {
  time_profiles_lst <- interval_data |>
    dplyr::select(!"interval") |>
    dplyr::mutate(linear = "linear") |>
    tidyr::nest(.by = "change_factor") |>
    tibble::deframe() |>
    purrr::map(\(x) tidyr::nest(x, .by = "type")) |>
    purrr::map(tibble::deframe) |>
    purrr::map_depth(2, tibble::deframe)

  lst |>
    purrr::modify_in("time_profile_mappings", \(x) {
      purrr::list_merge(x, !!!time_profiles_lst)
    })
}


set_ndg3_values <- function(lst) {
  new_ip_values <- list(c(0.9999, 1.0271), c(0.9963, 1.0268), c(1, 1)) |>
    rlang::set_names(c("elective", "non-elective", "maternity"))
  new_op_values <- purrr::map(seq(3), \(x) c(1.0002, 1.0487)) |>
    rlang::set_names(c("first", "followup", "procedure"))
  new_aae_values <- purrr::map(seq(2), \(x) c(0.9946, 1.0352)) |>
    rlang::set_names(c("ambulance", "walk-in"))
  values_list <- list(
    ip = new_ip_values,
    op = new_op_values,
    aae = new_aae_values
  )
  assign_list <- list("ndg3", "year-on-year-growth", values_list) |>
    rlang::set_names(c("variant", "value-type", "values"))
  purrr::assign_in(lst, "non-demographic_adjustment", assign_list)
}
