#' Save benchmark results
#'
#' @description
#' Function saves result of benchmark to text file.
#' @param x result vector or matrix
#' @param cec CEC version :: Int
#' @param id benchmark id :: String
#' @param prob problem number :: Int
#' @param dim dimension of given problem :: Int
#' @param label label of algorithm :: String
#' @param type result type :: String

save_results <- function(x, cec, id, prob, dim, type, dest) {
  dirpath <- stringr::str_glue("{dest}/cec{cec}/{id}/{type}/")
  filepath <- stringr::str_glue("{dest}/cec{cec}/{id}/{type}/{type}-{prob}-D-{dim}.txt")
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE)
  }
  write.table(x, file = filepath, sep = ",", col.names = FALSE, row.names = FALSE)
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
