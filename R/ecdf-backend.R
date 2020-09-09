get_group_param = function(dfx, vars, fn) {
  dfx %>%
    dplyr::group_by_at(vars) %>%
    dplyr::group_modify(function(val, ...) {
      fn(val)
  }) %>%
    dplyr::ungroup()
}


#' Extract method name
#'
#' @description 
#' Function uses regex to extract method name from benchmark ID.
#' @param id benchmark ID
#' @export

extract_method = function(id) {
    id %>%
    stringr::str_extract_all("\\b[a-z]+\\b") %>%
    unlist() %>%
    stringr::str_c(collapse = "-")
}

#' Load data
#' 
#' @description
#' Function reads results of given benchmark from file.
#' @param .probnum problem number :: [Int]
#' @param .ids benchmark ids :: [String]
#' @param .dim dimensionality of problem :: Int
#' @param .source version of CEC benchmark :: Int 
#' @export

get_result = function(.probnum, .ids, .dim, .source) {
    methods =
        .ids 
    .ids %>%
    purrr::map(function(id) {
      filepath = 
        stringr::str_glue("{.source}/{id}/M/M-{.probnum}-D-{.dim}.txt")
      print(filepath)
      read.table(file = filepath, sep = ",")
      }) %>% 
    purrr::set_names(methods)
}


generate_data = function(ids, dims, pnums) {
	data = 
		load_json(ids) %T>%
		validate_data(dims, pnum)
	c(ecdf, ms) %<~% 
		compute_ecdf(data)
	get_minct(data, ecdf, ms)
}

generate_df_json = function(idpaths) {
  bsteps = 
    c(0.01, 0.02, 0.03, 0.05, seq(0.1, 1.0, 0.1))*log10(10000)
  json_data = 
    load_json(idpaths)
  ecdf = 
    json_data %>%
    compute_ecdf()
  ms =
    ecdf %>%
    compute_ms()
  get_mincnt(
    .max_succ = ms,
    .bsteps = bsteps,
    .rep = reps,
    .probnums = fns,
    ecdf_vals = ecdf,
    results = 
  )
}


generate_df_txt = function(.dim, .ids, .probnums, .source, .rep = 51, .bsteps = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)) {
  results = 
    .probnums %>%
    purrr::map(get_result, .ids, .dim, .source) 
  return(results)
  ecdf_vals = 
    results %>%
    purrr::map(get_ecdf)
  ecdf_ms = 
    ecdf_vals %>% 
    get_ms(.rep)
  methods = 
    .ids
  get_mincnt(methods, results, ecdf_vals, .probnums, .bsteps, .rep, .max_succ = ecdf_ms)
}



