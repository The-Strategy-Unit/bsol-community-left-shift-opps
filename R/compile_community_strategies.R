compile_ambitious_intervals <- function() {
  # provides 23 of the 29 mitigators
  neecom_strategies <- get_blob_container(Sys.getenv("AZ_SUPPORT_CONTAINER")) |>
    AzureStor::storage_load_rds("neecom_table.rds", type = "gzip") |>
    dplyr::select(c(strategy = "strategy_variable", "p10", "p90")) |>
    dplyr::filter(.data[["strategy"]] %in% community_strategies())

  # provides 6 of the 29 mitigators
  nee_subset <- stringr::str_subset(community_strategies(), "medicines|child")
  nee_strategies <- get_blob_container(Sys.getenv("AZ_SUPPORT_CONTAINER")) |>
    AzureStor::storage_load_rds("nee_table.rds", type = "none") |>
    dplyr::select(c(
      strategy = "param_name",
      p10 = "LowerCI",
      p90 = "UpperCI"
    )) |>
    dplyr::filter(.data[["strategy"]] %in% nee_subset)

  combined_strategies <- dplyr::bind_rows(neecom_strategies, nee_strategies)
  get_mitigator_lookup() |>
    dplyr::left_join(combined_strategies, "strategy") |>
    dplyr::mutate(dplyr::across(c("p10", "p90"), \(x) x / 100)) |>
    dplyr::mutate(
      interval = purrr::map2(.data[["p10"]], .data[["p90"]], c),
      .keep = "unused"
    )
}

get_mitigator_lookup <- function() {
  get_blob_container(Sys.getenv("AZ_SUPPORT_CONTAINER")) |>
    AzureStor::storage_read_csv(
      "mitigator-lookup.csv",
      col_types = "-ccc-----"
    ) |>
    dplyr::rename(
      change_factor = "mitigator_type",
      strategy = "mitigator_variable",
      strategy_name = "mitigator_name"
    ) |>
    dplyr::filter(.data[["strategy"]] %in% community_strategies())
}


modify_community_mitigator_params <- function(lst, interval_data) {
  deframe_tbl <- function(tbl) {
    tbl |>
      dplyr::select(!"mitigator_type") |>
      dplyr::mutate(dplyr::across("interval", \(x) {
        purrr::map(x, \(x) rlang::set_names(list(x), "interval"))
      })) |>
      tibble::deframe()
  }
  activity_avoidance_list <- interval_data |>
    dplyr::filter(.data[["mitigator_type"]] == "activity_avoidance") |>
    deframe_tbl()
  efficiencies_list <- interval_data |>
    dplyr::filter(.data[["mitigator_type"]] == "efficiencies") |>
    deframe_tbl() |>
    # efficiencies have to have a type as well as an interval
    # all 5 currently included can just have type = "all"
    purrr::map(\(x) purrr::list_merge(x, type = "all"))
  lst |>
    purrr::modify_in(c("activity_avoidance", "ip"), \(x) {
      purrr::list_modify(x, !!!activity_avoidance_list)
    }) |>
    purrr::modify_in(c("efficiencies", "ip"), \(x) {
      purrr::list_modify(x, !!!efficiencies_list)
    })
}


compile_planned_intervals <- function() {
  board <- pins::board_connect(auth = "envvar")

  selected_schemes <- c(
    "RNQ",
    "RGP",
    "RHW",
    "RCX",
    "RGN",
    "RVR",
    "RXC",
    "RWG",
    "RGR",
    "RCF",
    "RAS",
    "RDU",
    "RH5",
    "RQW",
    "RBT",
    "R1H",
    "RTX",
    "RXN"
  )

  base_tbl <- tidyr::expand_grid(
    scheme = selected_schemes,
    strategy = community_strategies()
  )

  params_data_init <- board |>
    pins::pin_read("matt.dray/nhp_tagged_runs_params") |>
    purrr::keep_at(selected_schemes)

  activity_avoidance_intervals <- params_data_init |>
    purrr::map(c("activity_avoidance", "ip"))
  efficiencies_intervals <- params_data_init |>
    purrr::map(c("efficiencies", "ip"))

  intervals_tbl <- activity_avoidance_intervals |>
    purrr::list_merge(!!!efficiencies_intervals) |>
    purrr::map_depth(2, \(x) list(purrr::pluck(x, "interval"))) |>
    purrr::imap(\(x, y) y = tibble::as_tibble_row(x)) |>
    purrr::list_rbind(names_to = "scheme") |>
    tidyr::pivot_longer(
      !"scheme",
      names_to = "strategy",
      values_drop_na = TRUE
    ) |>
    tidyr::unnest_wider("value", names_sep = "_") |>
    dplyr::rename(p10 = "value_1", p90 = "value_2")

  get_mitigator_lookup() |>
    dplyr::left_join(base_tbl, "strategy") |>
    dplyr::left_join(intervals_tbl, c("scheme", "strategy")) |>
    # Rationale for these exclusions is here: https://github.com/The-Strategy-Unit/nhp_analysis/blob/cbf7e809419b516d6d8e2977fbf5d5c950263b7d/2025-02-28-wp1a-communties/wp1a.qmd#L159
    dplyr::filter(
      dplyr::if_any("strategy", \(x) !grepl("^virtual", x)) |
        dplyr::if_any(c("p10", "p90"), \(x) !is.na(x))
    ) |>
    dplyr::filter(
      .data[["scheme"]] != "RH5" |
        .data[["strategy"]] != "virtual_wards_activity_avoidance_ari"
    ) |>
    dplyr::mutate(dplyr::across(c("p10", "p90"), \(x) {
      tidyr::replace_na(x, 1)
    })) |>
    dplyr::filter(dplyr::if_all(c("p10", "p90"), \(x) {
      dplyr::between(x, 0, 1)
    })) |>
    create_scheme_mixtures() |>
    dplyr::mutate(
      interval = purrr::map2(.data[["p10"]], .data[["p90"]], c),
      .keep = "unused"
    )
}
