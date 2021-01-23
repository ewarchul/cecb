#' Extract method name
#'
#' @description
#' Function uses regex to extract method name from benchmark ID.
#' @param id benchmark ID

extract_method <- function(id) {
  id %>%
    stringr::str_extract_all("\\b[a-z]+\\b") %>%
    unlist() %>%
    stringr::str_c(collapse = "-")
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

#' Get function wrapper
#'
#' @param cec version of CEC (13, 14, 17, 21) :: Int
#' @param suite benchmark suite :: String

get_eval_func <- function(cec, suite) {
  if (cec == 13) {
    function(n, x) {
      cecs::cec2013(n, x) + 1500
    }
  }
  else if (cec == 14) {
    function(n, x) {
      cecs::cec2014(n, x)
    }
  }
  else if (cec == 17) {
    function(n, x) {
      cecs::cec2017(n, x)
    }
  }
  else if (cec == 21) {
    function(n, x) {
      cecs::cec2021(n, x, suite)
    }
  }
}

#' Get budget steps for CEC2021
#'
#' @param dim dimenstionality :: Int

get_recordedTimes <- function(dim) {
  dim^(((0:15) / 5) - 3)
}

#' Get values of global optimum
#'
#' @param cec version of CEC :: Int
#' @param suite benchmark suite :: String

get_scores <- function(cec, suite) {
  if (cec == 13) {
    c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500
  }
  else if (cec %in% c(14, 17)) {
    seq(100, 3000, by = 100)
  }
  else if (cec == 21) {
    if (suite %in% c("basic", "shift", "rot", "shift_rot")) {
      rep(0, 10)
    } else {
      c(100, 1100, 700, 1900, 1700, 1600, 2100, 2200, 2400, 2500)
    }
  }
}

#' Get problem partition in CECs
#'
#' @param cec version of CEC :: Int

get_class_div <- function(cec) {
  if (cec %in% c(14, 17)) {
    list(
      unimodal = 1:3,
      multimodal = 4:10,
      hybrid = 11:20,
      composition = 21:30
    )
  }
  else if (cec == 13) {
    list(
      unimodal = 1:5,
      multimodal = 6:10,
      hybrid = 11:20,
      composition = 21:28
    )
  } else {
    list(
      unimodal = 1,
      multimodal = 2:4,
      hybrid = 5:7,
      composition = 8:10
    )
  }
}
