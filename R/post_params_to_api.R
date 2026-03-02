  req <- httr2::request(check_getenv("NHP_API_URI"))
  req |>
    httr2::req_url_path_append("api") |>
    httr2::req_url_path_append("run_model") |>
    httr2::req_url_query(
      app_version = check_getenv("NHP_VERSION"),
      code = check_getenv("NHP_API_KEY")
    ) |>
    httr2::req_body_json(params_json) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json()
}


check_getenv <- function(var) {
  if (is.na(Sys.getenv(var, NA))) {
    cli::cli_abort("Environment variable {.var {var}} is not set")
  } else {
    Sys.getenv(var)
  }
}
