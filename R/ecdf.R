#' Compute ECDF values
#'
#' @description
#' Function computes ECDF values and put results into data frame.
#' @param benchmark_data previously loaded and parsed data from files :: [numeric]
#' @param idpath filepath to benchmark results :: character
#' @param probnums problems for which ECDF will be computed :: [integer]
#' @param rep benchmark repetitions :: integer
#' @return data frame with ECDF values

compute_ecdf <- function(benchmark_data, idpath, probnums, rep) {
  bsteps <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0) * log10(10000)
  ecdf_vals <- purrr::map(benchmark_data, get_ecdf)
  ecdf_ms <- get_ms(ecdf_vals, rep)
  get_mincnt(
    extract_id(idpath),
    benchmark_data,
    ecdf_vals,
    probnums,
    bsteps,
    rep,
    .max_succ = ecdf_ms
  )
}


#' Compute partial ECDF values
#' TODO

ecdf_leval <- function(.result, .maxb, .eps) {
  lhs <-
    .result %>%
    purrr::map_dbl(function(method) {
      min(method[.maxb, ])
    }) %>%
    min() %>%
    max(., .eps)
  lhs_log <-
    lhs %>%
    log10() %>%
    `/`(0.2)
  lhs_log
}

#' Compute partial ECDF values
#' TODO

ecdf_reval <- function(.result, .minb) {
  rhs <-
    .result %>%
    purrr::map_dbl(function(method) {
      max(method[.minb, ])
    }) %>%
    max()
  rhs_log <-
    rhs %>%
    log10() %>%
    `/`(0.2)
  rhs_log
}

#' Compute ECDF values
#' TODO

get_ecdf <- function(.result, .maxb = 14, .minb = 1, .eps = 10^-8) {
  lseq <- ecdf_leval(.result, .maxb, .eps)
  rseq <- ecdf_reval(.result, .minb)
  rev(c(1 %o% (10)^(0.2 * lseq:rseq)))
}

#' ugly af and needs refactoring
#' TODO

get_mincnt <- function(.methods, .results, .ecdf, .probnums, .bsteps, .rep, .max_succ) {
  future::plan(future::multiprocess)
  .methods %>%
    furrr::future_map(function(met) {
      min_cnt <- rep(0, length(.bsteps))
      for (problem in 1:length(.probnums)) {
        for (bstep in 1:length(.bsteps)) {
          for (estep in 1:length(.ecdf[[problem - length(problem) + 1]])) {
            min_cnt[bstep] <- min_cnt[bstep] + sum(.results[[problem - length(problem) + 1]][[met]][bstep, ] < .ecdf[[problem - length(problem) + 1]][estep])
          }
        }
      }
      min_cnt
    }, .progress = FALSE) %>%
    purrr::set_names(.methods) %>%
    tibble::as_tibble() %>%
    tidyr::gather(key = "Method") %>%
    dplyr::group_by(Method) %>%
    dplyr::mutate(
      Bstep = .bsteps,
      Value = value / .max_succ
    ) %>%
    dplyr::select(-value) %>%
    dplyr::ungroup()
}


#' TODO

get_ms <- function(.ecdf, .rep) {
  1:length(.ecdf) %>%
    purrr::map(function(prob) {
      length(.ecdf[[prob]]) * .rep
    }) %>%
    purrr::reduce(sum)
}
