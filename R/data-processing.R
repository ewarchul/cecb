#' Extract method name
#'
#' @description 
#' Function uses regex to extract method name from benchmark ID.
#' @param id benchmark ID

extract_method = function(id) {
    id %>%
    stringr::str_extract_all("\\b[a-z]+\\b") %>%
    unlist() %>%
    stringr::str_c(collapse = "-")
}


extract_id <- function(idpaths) {
  idpaths %>%
    purrr::map(stringr::str_extract, "([^/]+$)")
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
  txt_table = generate_table(idpaths, load_result_txt, probnums, dim)
  base::Map(c, txt_table) %>%
    compute_ecdf(idpaths, probnums, rep)
}

generate_table <- function(idpaths, format_handler, probnums, dim) {
  purrr::map(probnums, format_handler, idpaths, dim)
}

