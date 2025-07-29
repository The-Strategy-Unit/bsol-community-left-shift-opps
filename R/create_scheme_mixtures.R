#' Combine all planned intervals from 18 schemes into a single distribution
#' for each strategy
create_scheme_mixtures <- function(params_tbl) {
  params_tbl |>
    dplyr::mutate(
      mu = (.data[["p10"]] + .data[["p90"]]) / 2,
      sigma = (.data[["p90"]] - .data[["mu"]]) /
        qnorm(p = 0.90, mean = 0, sd = 1),
      dplyr::across("sigma", \(x) dplyr::if_else(x == 0, 0.0001, x)),
      dist = purrr::map2(.data[["mu"]], .data[["sigma"]], \(m, s) {
        distr::Truncate(distr::Norm(mean = m, sd = s), lower = 0, upper = 1)
      })
    ) |>
    dplyr::summarise(
      dplyr::across("dist", list),
      .by = c("mitigator_type", "strategy")
    ) |>
    dplyr::mutate(
      mixture = purrr::map(.data[["dist"]], \(x) {
        distr::UnivarMixingDistribution(Dlist = x)
      }),
      .keep = "unused"
    ) |>
    dplyr::mutate(
      p10 = purrr::map_dbl(.data[["mixture"]], \(m) m@q(p = 0.1)),
      # p50 = purrr::map_dbl(.data[["mixture"]], \(m) m@q(p = 0.5)),
      p90 = purrr::map_dbl(.data[["mixture"]], \(m) m@q(p = 0.9)),
      .keep = "unused"
    )
}
