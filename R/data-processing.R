save_tables = function(df, dirpath) {
  df %>%
    dplyr::group_by(Dim, Func, Benchmark) %>%
    dplyr::group_walk(function(data, info) {
      datapath = dirpath %++%
        "/" %++%
        info$Benchmark %++%
        "/M"
      dir.create(datapath, showWarnings = FALSE)
      data %>% readr::write_csv(
       file = 
         datapath %++% "/" %++%
         "M_" %++% 
         info$Func %++% "_" %++% info$Dim %++% ".csv"
      )
    })
}

generate_error_table = function(input, dims, probs, reps, cec) {
  bench_id = input %>% purrr::pluck("benchmark_id")
  expand.grid(
    d = dims,
    p = probs
  ) %>%
  purrr::pmap(function(d, p) {
    steps = get_budgetSteps(cec, d)
    scores = get_scores(cec)
    reps %>% purrr::map(function(r) {
      df = input %>%
        purrr::pluck(
          "data_comp",
          as.character(d),
          as.character(p),
          as.character(r)
      ) %>% 
      unlist() %>%
      matrix()
    dfx = tibble::tibble(
      !!rlang::sym(paste0("R", r)) := df[steps * nrow(df),] - scores[p]
    ) %>% padding_df(length(steps))
    }) %>%
    purrr::reduce(dplyr::bind_cols) %>%
    dplyr::mutate(
      Dim = d,
      Func = p,
      Benchmark = bench_id
    )
  }) %>%
  purrr::reduce(dplyr::bind_rows)
}


padding_df = function(df, bound) {
  if (nrow(df) == bound) {
    df
  } else {
    nrows = nrow(df)
    padding = df %>% 
      dplyr::slice(dplyr::n()) %>% dplyr::slice(rep(1:dplyr::n(), each = bound - nrows))
    dplyr::bind_rows(df, padding)
  }
}

get_budgetSteps = function(cec, dim) {
  if (cec %in% c(13, 14, 17))
    c(0.01, 0.02, 0.03, 0.05, seq(0.1, 1, 0.1))
  else {
    dim^(((0:15) / 5) - 3)
  }
}

get_scores = function(cec) {
  if (cec == 13) {
     c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500
  }
  else if (cec %in% c(14, 17)) {
    seq(100, 3000, by = 100)
  }
  else {
    c(100, 1100, 700, 1900, 1700, 1600, 2100, 2200, 2400, 2500)
  }
}
