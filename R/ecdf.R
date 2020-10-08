#' TODO

compute_ms <- function(dfx) {
  reps <-
    base::length(dfx)
  dfx %>%
    get_group_param(
      c("Dimension"),
      function(x) {
        x %>%
          dplyr::select(Ecdf) %>%
          get_ms(reps) %>%
          tibble::tibble(
            Ms = .
          )
      }
    )
}


#' Compute partial ECDF values

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

#' TODO

get_ecdf <- function(.result, .maxb = 14, .minb = 1, .eps = 10^-8) {
  lseq <- ecdf_leval(.result, .maxb, .eps)
  rseq <- ecdf_reval(.result, .minb)
  rev(c(1 %o% (10)^(0.2 * lseq:rseq)))
}

#' TODO

get_mincnt <- function(.methods, .results, .ecdf, .probnums, .bsteps, .rep, .max_succ) {
  future::plan(multiprocess)
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
    }, .progress = TRUE) %>%
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

compute_mincnt <- function(dfx, steps) {
  dfx
}

#' TODO

get_ms <- function(.ecdf, .rep) {
  1:length(.ecdf) %>%
    purrr::map(function(prob) {
      length(.ecdf[[prob]]) * .rep
    }) %>%
    purrr::reduce(sum)
}
