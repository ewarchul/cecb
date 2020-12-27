#' Config parser
#'
#' @description
#' Function parses benchmark configuration file.
#' @param config config list

parse_config <- function(config) {
  if (is.list(config)) {
    config
  } else {
    parse_yaml_config(config)
  }
}

#' Verify config list
#' @description
#' Function checks if given config has only required fields.
#' @param config named list :: [character]
#' @return binary flag :: logical

verify_config_names = function(config) {
  required_fields = 
    c("methods", "ids", "probnum",
      "dims", "cec", "repnum",
      "cpupc", "source", "dest",
      "save", "suite"
    )
  diffs =
    purrr::prepend(
      dplyr::setdiff(names(config), required_fields),
      dplyr::setdiff(required_fields, names(config))
    )
  if (!length(diffs)) TRUE else FALSE
}

#' YAML config parser
#'
#' @description
#' Function parses YAML configuration file.
#' @param filename name of config file :: String

parse_yaml_config <- function(filename) {
  config <-
    yaml::read_yaml(filename)
  if (!verify_config_names(config))
    stop("Given config is incorrect.")
  alg_num <-
    length(config$methods)
  alg_names <-
    extract_names(alg_num, config$methods)

  alg_names %>%
    purrr::walk(function(method) {
      source(paste0(config$source, "/", stringr::str_replace_all(method, "_", "-"), ".R"))
    })

  config$methods_sym <-
    extract_algorithm(alg_num, config$methods)
  config
}

#' Extract algorithms names
#' @description
#' Function extracts names of algorithm from given config file.
#' @param amount amount of benchmarked algorithms
#' @param algs list of closures
#' @return list of algorithm names

extract_names <- function(amount, algs) {
  1:amount %>%
    purrr::map_chr(function(num) {
      alg <-
        algs %>% purrr::pluck(num)
      alg$algorithm
    })
}

#' Extract algorithm closure
#' @description
#' Function extracts algorithms as a closures and sets their parameters.
#' @param amount amount of benchmarked algorithms
#' @param algs list of closures
#' @return list of partial functions

extract_algorithm <- function(amount, algs) {
  1:amount %>%
    purrr::map(function(num) {
      alg <-
        algs %>% purrr::pluck(num)
      base_func <-
        base::get(alg$algorithm)
      param_set <-
        setNames(as.list(alg$values), alg$params)
      purrr::partial(base_func, control = param_set)
    })
}
