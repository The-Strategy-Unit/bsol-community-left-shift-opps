#' Read in BSol inputs data
read_inputs_data_pqt <- function(file) {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  az_container <- Sys.getenv("AZ_INPUTS_CONTAINER")
  nhp_version <- Sys.getenv("NHP_VERSION")
  uhb <- "RRK" # University Hospitals Birmingham
  mmu <- "RXK" # Sandwell and West Birmingham (MMU only)

  adls_endpoint <- AzureStor::adls_endpoint(
    endpoint_uri,
    token = get_azure_token()
  )
  fs <- AzureStor::adls_filesystem(adls_endpoint, az_container)
  filepath <- glue::glue("{nhp_version}/{file}.parquet")
  assertthat::assert_that(AzureStor::adls_file_exists(fs, filepath))

  withr::with_tempfile("dl", {
    AzureStor::download_adls_file(fs, filepath, dest = dl)
    arrow::read_parquet(dl) |>
      dplyr::filter(.data[["provider"]] %in% c(uhb, mmu))
  })
}

read_results_pqt <- function(scheme, scenario, tables = NULL) {
  tbl_names <- c(
    "acuity",
    "age",
    "attendance_category",
    "avoided_activity",
    "default",
    "sex+age_group",
    "sex+tretspef",
    "step_counts",
    "tretspef_raw+los_group",
    "tretspef_raw"
  )
  if (is.null(tables)) {
    keep_all <- TRUE
    tables <- tbl_names
  } else {
    keep_all <- FALSE
    tables <- rlang::arg_match(tables, values = tbl_names, multiple = TRUE)
  }

  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  az_container <- Sys.getenv("AZ_RESULTS_CONTAINER")
  nhp_version <- Sys.getenv("NHP_VERSION")

  adls_endpoint <- AzureStor::adls_endpoint(
    endpoint_uri,
    token = get_azure_token()
  )
  fs <- AzureStor::adls_filesystem(adls_endpoint, az_container)
  scenario_path <- glue::glue(
    "aggregated-model-results/{nhp_version}/{scheme}/bsol-ndg3-{scenario}"
  )
  assertthat::assert_that(AzureStor::adls_dir_exists(fs, scenario_path))
  blob_names <- AzureStor::list_blobs(fs, scenario_path, recursive = FALSE) |>
    dplyr::pull("name")
  latest <- max(basename(blob_names))
  pqt_path <- glue::glue("{scenario_path}/{latest}")
  assertthat::assert_that(AzureStor::adls_dir_exists(fs, pqt_path))
  file_paths <- AzureStor::list_blobs(fs, pqt_path, recursive = FALSE) |>
    dplyr::filter(grepl("\\.parquet$", .data[["name"]])) |>
    dplyr::pull("name")
  pqt_names <- sub("^(.*/)([[:graph:]]+)(\\.parquet)$", "\\2", file_paths)

  absent <- setdiff(tables, pqt_names)
  assertthat::assert_that(
    rlang::is_empty(absent),
    msg = cli::cli_alert("Table{?s} {.val {absent}} {?is/are} not present")
  )

  named_paths <- rlang::set_names(file_paths, pqt_names)
  named_paths <- if (!keep_all) purrr::keep_at(named_paths, tables)
  named_paths |>
    purrr::map(\(p) {
      assertthat::assert_that(AzureStor::adls_file_exists(fs, p))
      withr::with_tempfile("dl", {
        # Ideally don't do this through the VPN! It's much faster outside.
        AzureStor::download_adls_file(fs, p, dest = dl)
        arrow::read_parquet(dl)
      })
    }) |>
    rlang::set_names(names(named_paths))
}

read_inputs_data_rds <- function(file) {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  az_container <- Sys.getenv("AZ_INPUTS_CONTAINER")
  nhp_version <- Sys.getenv("NHP_VERSION")

  adls_endpoint <- AzureStor::adls_endpoint(
    endpoint_uri,
    token = get_azure_token()
  )
  fs <- AzureStor::adls_filesystem(adls_endpoint, az_container)
  filepath <- glue::glue("{nhp_version}/{file}.rds")
  assertthat::assert_that(AzureStor::adls_file_exists(fs, filepath))

  withr::with_tempfile("dl", {
    AzureStor::download_adls_file(fs, filepath, dest = dl)
    readr::read_rds(dl)
  })
}

read_azure_rds <- function(container_name, file) {
  get_blob_container(container_name) |>
    AzureStor::storage_load_rds(glue::glue("{file}.rds"))
}

read_azure_data_csv <- function(container, file) {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")

  adls_endpoint <- AzureStor::adls_endpoint(
    endpoint_uri,
    token = get_azure_token()
  )
  fs <- AzureStor::adls_filesystem(adls_endpoint, container)
  filepath <- glue::glue("{file}.csv")
  assertthat::assert_that(AzureStor::adls_file_exists(fs, filepath))

  AzureStor::storage_read_csv(container, filepath)

  withr::with_tempfile("dl", {
    AzureStor::download_adls_file(fs, filepath, dest = dl)
    readr::read_csv(dl)
  })
}

#' Get Azure storage blob container
#' @param container_name string: name of the container
#' @param ... arguments to be passed on to `get_azure_token()`
get_blob_container <- function(container_name, ...) {
  endpoint_uri <- Sys.getenv("AZ_STORAGE_EP")
  token <- get_azure_token(...)
  endpoint_uri |>
    AzureStor::blob_endpoint(token = token) |>
    AzureStor::storage_container(container_name)
}
