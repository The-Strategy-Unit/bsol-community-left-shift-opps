write_params_json_file <- function(json_data, basename, save_dir = "json") {
  file_out <- here::here(save_dir, paste0(basename, ".json"))
  yyj_write_opts <- yyjsonr::opts_write_json(pretty = TRUE, auto_unbox = TRUE)
  yyjsonr::write_json_file(json_data, file_out, yyj_write_opts)
  file_out
}
