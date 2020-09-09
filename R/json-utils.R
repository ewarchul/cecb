parse_json = function(bench_json) {
  c(dims, probnums) %<~%
    get_bench_setting(bench_json)
  expand.grid(
              dim = dims,
              func = probnums
  ) %>%
  purrr::pmap(function(dim, func) {
   func_df = 
      bench_json %>%
      purrr::pluck(dim, func) %>%
      tibble::enframe() %>%
      tidyr::unnest(cols = c(value)) %>%
      tidyr::unnest(cols = c(value)) ## unnest column val :: [[dbl]]
   func_df %>%
      dplyr::group_by(name) %>%
      dplyr::group_map(function(val, ...) val) %>%
      purrr::reduce(dplyr::bind_cols) %>% 
      purrr::set_names(function(cname) {
        rep_num = 
          stringr::str_extract(cname, "[1-9]+") 
        "Rep" %++% rep_num
      }) %>%
      dplyr::mutate(Dimension = dim, Function = func)
  }) %>%
  dplyr::bind_rows()
}

get_bench_setting = function(json) {
  dims = 
    names(json)
  problems =
    json %>% 
      purrr::pluck(dims[1]) %>%
      names()
  list(dims, problems)
}

