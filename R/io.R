#' Save benchmark results
#'
#' @description
#' Function saves result of benchmark to JSON file. If given dirpath doesn't exist function creates it.
#' @param x result vector or matrix
#' @param dest path to directory
#' @param filename
#' @param filename name of json file
#' @return IO :: ()
#' @export

save_results <- function(x, dest, filename) {
  dirpath <-
    dest %++% "/" %++% filename %++% "/"
  filepath <-
    dest %++% "/" %++% filename %++% "/" %++% filename %++% ".json"

  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE)
  }
  x %>%
    jsonlite::toJSON() %>%
    base::write(file = filepath)
}

#' Save metadata
#'
#' @description
#' Function save serialized benchmark's metadata. Created RDS file contains:
#' - benchmarked method written as a closure
#' - time of benchmark execution in minutes
#' - id of benchmark.
#'
#' @param dest path to directory
#' @param filename name of file with metadata
#' @param info list with stored objects
#' @return IO :: ()
#' @export

save_metadata <- function(dest, filename, info) {
  filepath <-
    dest %++% "/" %++% filename %++% "/" %++% filename %++% ".RDS"
  info %>%
    readr::write_rds(filepath)
}


#' Send SMS
#'
#' @description
#' Function sends SMS with information about status of benchmark.
#' It reads number and Twilio auth from .twilio-meta file.
#' @param filepath path to Twilio auth configuration :: String
#' @param type type of message i.e 'start' or 'end' of benchmark :: String
#' @param id benchmark id :: String

#' Save plot
#'
#' @description
#' Function saves ECDF plot to postcript.
#' @param .plot ggplot2 object
#' @param .name name of pllot
#' @param .x width of image
#' @param .y height of image
#' @export

save_eps <- function(.plot, .name, .x = 6, .y = 6) {
  postscript(file = paste0("../doc/eps/", .name, "-", ".eps"), width = .x, height = .y)
  print(.plot)
  dev.off()
}
