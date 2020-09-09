'%++%' = function(str1, str2) {
  paste0(str1, str2)
}

'%<~%' = zeallot::'%<-%'

get_bench_setting = function(json) {
  dims = 
    names(json)
  problems =
    json %>% 
      purrr::pluck(dims[1]) %>%
      names()
  list(dims, problems)
}

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

get_group_param = function(dfx, vars, fn) {
  dfx %>%
    dplyr::group_by_at(vars) %>%
    dplyr::group_modify(function(val, ...) {
      fn(val)
  }) %>%
    dplyr::ungroup()
}


compute_ecdf = function(dfx) {
  dfx %>%
    get_group_param(
      c("Dimension", "Function"),
      function(x) {
        x %>%
          list() %>%
          get_ecdf() %>%
          tibble::tibble(
          Ecdf = . 
        )
      }
    )
}

compute_ms = function(dfx) {
  reps = 
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

#' Compute partial ECDF values
#' @export

ecdf_leval = function(.result, .maxb, .eps) {
  lhs = 
    .result %>%
    purrr::map_dbl(function(method) {
        min(method[.maxb,])
      }) %>%
    min() %>%
    max(., .eps)
  lhs_log = 
    lhs %>% 
    log10() %>%
    `/`(0.2)
  lhs_log
}

#' Compute partial ECDF values
#' TODO
#' @export

ecdf_reval = function(.result, .minb) {
  rhs = 
    .result %>%
    purrr::map_dbl(function(method) {
        max(method[.minb,])
    }) %>%
    max()
  rhs_log = 
    rhs %>% 
    log10() %>%
    `/`(0.2)
  rhs_log
}

#' Compute ECDF values
#' TODO 
#' @export

get_ecdf = function(.result, .maxb = 14, .minb = 1, .eps = 10^-8) {
  lseq = ecdf_leval(.result, .maxb, .eps)
  rseq = ecdf_reval(.result, .minb)
  rev(c(1 %o% (10)^(0.2*lseq:rseq)))
}

#' ugly af and needs refactoring
#' TODO 
#' @export

get_mincnt = function(.methods, .results, .ecdf, .probnums, .bsteps, .rep, .max_succ) {
  future::plan(multiprocess)
  .methods %>%
    furrr::future_map(function(met) {
      min_cnt = rep(0,length(.bsteps))
      for(problem in 1:length(.probnums)) {
        for(bstep in 1:length(.bsteps)) {
          for(estep in 1:length(.ecdf[[problem - length(problem) + 1]])) {
            min_cnt[bstep] = min_cnt[bstep] + sum(.results[[problem - length(problem) + 1]][[met]][bstep, ] < .ecdf[[problem - length(problem) + 1]][estep])
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
                  Value = value/.max_succ
                  ) %>%
    dplyr::select(-value) %>%
    dplyr::ungroup()
}

compute_mincnt = function(dfx, steps) {
  dfx 
}

#' TODO 
#' @export

get_ms = function(.ecdf, .rep) {
  1:length(.ecdf) %>%
    purrr::map(function(prob) {
      length(.ecdf[[prob]])*.rep
    }) %>%
    purrr::reduce(sum)

}

#' ECDF data frame
#' 
#' @description 
#' Function generates data frame which is backend for ECDF plot function.
#' @param .dim dimensionality of problem :: Int
#' @param .ids benchmark ids :: [String]
#' @param .probnums problem numbers :: [Int]
#' @param .source version of CEC benchmark :: Int 
#' @param .rep number of repetition
#' @param .bsteps fraction of evaluation function budget
#' @export

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

generate_df_json = function(idpaths) {
  bsteps = 
    c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)*log10(10000)
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

#' Save plot
#' 
#' @description
#' Function saves ECDF plot to postcript.
#' @param .plot ggplot2 object
#' @param .name name of pllot
#' @param .x width of image
#' @param .y height of image
#' @export

save_eps = function(.plot, .name, .x = 6, .y = 6) {
  postscript(file = paste0("../doc/eps/", .name, "-",  ".eps"), width = .x, height = .y)
  print(.plot)
  dev.off()
}
