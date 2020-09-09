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
#' @export

save_results = function(x, dest, filename) {
  dirpath = 
    dest %++% '/' %++% filename %++% '/' 
  filepath = 
    dest %++% '/' %++% filename %++% '/' %++% filename %++% '.json'
  if (!dir.exists(dirpath))
    dir.create(dirpath, recursive = TRUE)
  print(x)
  x %>%
   jsonlite::toJSON() %>%
   base::write(file = filepath) 
}

#' TODO

save_metadata = function(dest, filename, info) {
  filepath = 
    dest %++% '/' %++% filename %++% '/' %++% filename %++% '.RDS'
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

send_sms = function(filepath, type, id) {
  if (type == "start")
    body = stringr::str_glue("Benchmark {id} start")
  else
    body = stringr::str_glue("Benchmark {id} end")
  config =
    yaml::read_yaml(filepath)
  Sys.setenv(TWILIO_SID = config$sid)
  Sys.setenv(TWILIO_TOKEN = config$token)
  twilio::tw_send_message(
    to = config$to_number,
    from = config$from_number,
    body = body 
    )
}
