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
