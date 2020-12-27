#' Check file format
#'
#' @description Function checks if given `filepath` has JSON or TXT format.
#' @param filepath :: [character]
#' @return type of format :: [character]

split_formats <- function(filepaths) {
  bool_flags = 
    filepaths %>%
    purrr::map_lgl(function(fpath) {
      has_json =
        base::list.files(fpath) %>%
        purrr::detect(stringr::str_detect, "json")
      if(length(has_json)) TRUE else FALSE
    })
  list(
    txt_format = 
      filepaths[which(!bool_flags)],
    json_format = 
      filepaths[which(bool_flags)]
  )
}

#' Area under curve
#' 
#' @description Function computes area under ECDF curve (AOC)
#' @param dfx data frame with ECDF values
#' @param method column name with algorithm(s) label(s) :: character
#' @param xarg column name of x-axis :: character
#' @param yarg column name of y-axis :: character
#' @return data table with `Aoc` column :: tibble

compute_aoc = function(dfx, method = "Method", xarg = "Bstep", yarg = "Value") {
  dfx %>%
    dplyr::group_by(!!rlang::sym(method)) %>%
    dplyr::mutate(Aoc = pracma::trapz(!!rlang::sym(xarg), !!rlang::sym(yarg))) %>%
    dplyr::slice(dplyr::n()) %>%
    dplyr::select(!!rlang::sym(method), Aoc)
}

#' Benchmark results data frame
#'
#' @description Function generates ready to plot data frame with benmchark results.
#' @param idpaths filepath to benchmark data :: [character]
#' @param config map with benchmark config: :: [dim :: integer, probnums :: [integer], reps :: integer]
#' @return data frame with benchmark data :: tibble
#' @export

get_dfr <- function(idpaths, config) {
  if (missing(config)) {
    cli::cli_alert_danger("User has to provide map with benchmark configuration. For more details check documentation.")
    base::stop("Missing config.")
  }
  c(dim, probnums, rep) %<~% config
  c(txts, jsons) %<~% split_formats(idpaths)
  json_table = 
    generate_table(jsons, load_result_json, probnums, dim)
  txt_table = 
    generate_table(txts, load_result_txt, probnums, dim)
  base::Map(c, json_table, txt_table) %>%
    compute_ecdf(c(jsons, txts), probnums, rep)

}


#' Compute data frame with ECDF
#'
#' @description
#' Function for given `idpaths` and benchmark setting generates data frame
#' with ECDF values using adequate format handler.
#' @param idpaths list of paths :: [character]
#' @param format_handler function to handle specific format file :: integer -> [character] -> integer -> [[numeric]]
#' @param probnums problem indices :: [numeric]
#' @param dim dimensionality of function :: numeric

generate_table <- function(idpaths, format_handler, probnums, dim) {
  purrr::map(probnums, format_handler, idpaths, dim)
}


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
  bsteps <-
    get_budget_step(14, dim = 10) * log10(10000)
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


#' Get benchmark data from JSON
#'
#' @description
#' Function gets benchmark results from previously parsed JSON.
#' @param json parsed JSON :: list
#' @param probnum number of function in benchmark :: integer
#' @param dim dimensionality of function :: integer

parse_json <- function(json, probnum, dim) {
  json %>%
    purrr::pluck(as.character(dim), as.character(probnum)) %>%
    purrr::flatten() %>%
    purrr::flatten_dbl()
}

#' Extract id
#'
#' @description Function extracts benchmarks ids from list of filepaths.
#' @param idpaths list of filepaths to benchmark :: [character]
#' @return list with raw ids of benchamrks :: [character]

extract_id <- function(idpaths) {
  idpaths %>%
    purrr::map(stringr::str_extract, "([^/]+$)")
}


#' Load data in TXT format
#'
#' @description
#' Function reads results of given benchmark from TXT file.
#' @param probnum problem number :: integer
#' @param idpaths benchmark ids :: [character]
#' @param dim dimensionality of problem :: integer

load_result_txt <- function(probnum, idpaths, dim) {
  idpaths %>%
    purrr::map(function(id) {
      filepath <-
        stringr::str_glue("{id}/M/M-{probnum}-D-{dim}.txt")
      read.table(file = filepath, sep = ",")
    }) %>%
    purrr::set_names(extract_id(idpaths))
}


#' Load data in JSON format
#'
#' @description
#' Function reads results of given benchmark from JSON file.
#' @param probnum problem number :: integer
#' @param idpaths benchmark ids :: [character]
#' @param dim dimensionality of problem :: integer

load_result_json <- function(probnum, idpaths, dim) {
  idpaths %>%
    purrr::map(function(id) {
      bench_id <-
        extract_id(id)
      filepath <-
        stringr::str_glue("{id}/{bench_id}.json")
      jsonlite::read_json(path = filepath) %>%
        parse_json(probnum, dim) %>%
        matrix(nrow = 16) %>%
        data.frame()
    }) %>%
    purrr::set_names(extract_id(idpaths))
}
