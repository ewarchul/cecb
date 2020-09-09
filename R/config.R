#' YAML config parser
#'
#' @description 
#' Function parses YAML configuration file. 
#' @param filename name of config file :: String
#' @export

parse_yaml_config = function(filename) {
  config = 
    yaml::read_yaml(filename)
  alg_num = 
    length(config$methods)
  alg_names = 
    extract_names(alg_num, config$methods)

  alg_names %>%
    purrr::walk(function(method) {
      source(paste0(config$source, "/", stringr::str_replace_all(method, "_", "-"), ".R"))
    })

  config$methods_sym = 
    extract_algorithm(alg_num, config$methods)
  config
}

#' @export

extract_names = function(amount, algs) {
  1:amount %>%
    purrr::map_chr(function(num) {
      alg =
        algs %>% purrr::pluck(num)
      alg$algorithm
    })
}

#' @export

extract_algorithm = function(amount, algs) {
  1:amount %>%
    purrr::map(function(num) {
      alg =
        algs %>% purrr::pluck(num)
      base_func = 
        base::get(alg$algorithm)
      param_set = 
        setNames(as.list(alg$values), alg$params)
      purrr::partial(base_func, control = param_set)
    })
}

#' Config parser
#'
#' @description 
#' Function parses benchmark configuration file.
#' @param config config list
#' @export

parse_config = function(config) {
  if (is.list(config))
    config
  else
    parse_yaml_config(config)
}
