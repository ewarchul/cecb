#' YAML config parser
#'
#' @description
#' Function parses YAML configuration file.
#' @param filename name of config file :: String

parse_yaml_config <- function(filename) {
  config <-
    yaml::read_yaml(filename)
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

#' Verify config list
#' @description
#' Function checks if given config has only required fields.
#' @param config named list :: [character]
#' @return binary flag :: logical

verify_config_names <- function(config) {
  required_fields <-
    c(
      "methods", "ids", "probnum",
      "dims", "cec", "repnum",
      "cpupc", "source", "dest",
      "suite"
    )
  diffs <-
    purrr::prepend(
      dplyr::setdiff(names(config), required_fields),
      dplyr::setdiff(required_fields, names(config))
    )
  if (!length(diffs)) TRUE else FALSE
}



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
