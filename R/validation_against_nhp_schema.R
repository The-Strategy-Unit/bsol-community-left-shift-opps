get_nhp_schema <- function(app_ver = Sys.getenv("NHP_VERSION", "dev")) {
  req <- httr2::request("https://api.github.com")
  resp <- req |>
    httr2::req_url_path_append("repos") |>
    httr2::req_url_path_append("The-Strategy-Unit") |>
    httr2::req_url_path_append("nhp_model") |>
    httr2::req_url_path_append("contents") |>
    httr2::req_url_path_append(app_ver) |>
    httr2::req_url_path_append("params-schema.json") |>
    httr2::req_url_query(ref = "schemas") |>
    httr2::req_perform()

  httr2::resp_check_status(resp)
  httr2::resp_check_content_type(resp, "application/json")

  httr2::resp_body_json(resp) |>
    purrr::pluck("content") |>
    # GitHub API returns file content base64-encoded
    base64enc::base64decode() |>
    yyjsonr::read_json_raw() |>
    yyjsonr::write_json_str(yyjsonr::opts_write_json(pretty = TRUE))
}


create_json_validator_fn <- function(schema_text) {
  withr::with_tempfile("sch", {
    yyj_opts <- yyjsonr::opts_write_json(pretty = TRUE)
    yyjsonr::write_json_file(schema_text, sch, yyj_opts)
    cat("\n", file = sch, append = TRUE)
    jsonvalidate::json_validator(sch)
  })
}
