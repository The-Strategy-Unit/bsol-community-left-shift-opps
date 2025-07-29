# R code to take base.json and produce custom params files to feed into the API
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

  withr::with_tempfile("out", {
    httr2::resp_body_json(resp) |>
      purrr::pluck("content") |>
      # GitHub API returns file content base64-encoded
      base64enc::base64decode() |>
      writeBin(out)
    jsonlite::fromJSON(out)
  })
}

create_custom_params <- function(dataset, scenario, ...) {
  assertthat::assert_that(
    rlang::is_scalar_character(dataset),
    rlang::is_scalar_character(scenario)
  )
  supplied_params <- rlang::dots_list(...)
  run_dttm <- substr(sub(" ", "_", gsub("[:-]", "", Sys.time())), 1L, 15L)
  param_defaults <- list(
    user = Sys.getenv("NHP_API_USER"),
    seed = 43447,
    model_runs = 256,
    start_year = 2022,
    end_year = 2035,
    app_version = Sys.getenv("NHP_APP_VERSION") %||% "dev",
    viewable = FALSE,
    create_datetime = run_dttm,
    health_status_adjustment = TRUE
  )
  assertthat::assert_that(purrr::none(param_defaults, is.null))
  param_defaults |>
    purrr::list_modify(dataset = dataset, scenario = scenario) |>
    purrr::list_modify(!!!supplied_params)
}

modify_demographic_factors_list <- function(params, ...) {
  default_list <- list(principal_proj = 1)
  custom_list <- purrr::list_modify(default_list, ...)
  params |>
    purrr::modify_in("demographic_factors", \(x) {
      purrr::assign_in(x, "variant_probabilities", custom_list)
    })
}


modify_base_params <- function(base_params, custom_params) {
  purrr::list_modify(base_params, !!!custom_params)
}


negate_covid_adjustment <- function(params_list) {
  # Using NULL fails JSON validation as the schema requires `"minItems": 2`
  # hence here we use `c(1, 1)` which negates adjustment, though this is
  # a less neat solution than just using NULL to generate an empty `{}`.
  op_list <- purrr::map(seq(3), \(x) c(1, 1)) |>
    rlang::set_names(c("first", "followup", "procedure"))
  ip_list <- purrr::map(seq(3), \(x) c(1, 1)) |>
    rlang::set_names(c("elective", "non-elective", "maternity"))
  aae_list <- purrr::map(seq(2), \(x) c(1, 1)) |>
    rlang::set_names(c("ambulance", "walk-in"))
  params_list |>
    purrr::modify_at("covid_adjustment", \(x) {
      x |>
        purrr::assign_in("op", op_list) |>
        purrr::assign_in("ip", ip_list) |>
        purrr::assign_in("aae", aae_list)
    })
}


set_linear_time_profiles <- function(lst, mitigator_lookup) {
  deframe_tbl <- function(tbl) {
    tbl |>
      dplyr::select("strategy") |>
      dplyr::mutate(linear = "linear") |>
      tibble::deframe() |>
      as.list()
  }
  activity_avoidance_list <- mitigator_lookup |>
    dplyr::filter(.data[["mitigator_type"]] == "activity_avoidance") |>
    deframe_tbl()
  efficiencies_list <- mitigator_lookup |>
    dplyr::filter(.data[["mitigator_type"]] == "efficiencies") |>
    deframe_tbl()
  lst |>
    purrr::modify_at("time_profile_mappings", \(x) {
      x |>
        purrr::assign_in(c("efficiencies", "ip"), efficiencies_list) |>
        purrr::assign_in(c("activity_avoidance", "ip"), activity_avoidance_list)
    })
}


set_ndg3_values <- function(params) {
  new_ip_values <- list(c(1.000, 1.027), c(0.996, 1.027), c(1, 1)) |>
    rlang::set_names(c("elective", "non-elective", "maternity"))
  new_op_values <- purrr::map(seq(3), \(x) c(1.000, 1.049)) |>
    rlang::set_names(c("first", "followup", "procedure"))
  new_aae_values <- purrr::map(seq(2), \(x) c(0.995, 1.035)) |>
    rlang::set_names(c("ambulance", "walk-in"))
  values_list <- list(
    ip = new_ip_values,
    op = new_op_values,
    aae = new_aae_values
  )
  assign_list <- list("ndg3", "year-on-year-growth", values_list) |>
    rlang::set_names(c("variant", "value-type", "values"))
  params |>
    purrr::assign_in("non-demographic_adjustment", assign_list)
}
