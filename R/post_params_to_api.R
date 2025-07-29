post_params_to_api <- function(params_json) {
  req <- httr2::request(Sys.getenv("NHP_API_URI"))
  req |>
    httr2::req_url_path_append("api") |>
    httr2::req_url_path_append("run_model") |>
    httr2::req_url_query(
      app_version = Sys.getenv("NHP_APP_VERSION"),
      code = Sys.getenv("NHP_API_KEY")
    ) |>
    httr2::req_body_json(params_json) |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    httr2::resp_body_json()
}
