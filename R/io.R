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

save_results = function(x, dest, filename) {
  dirpath = 
    dest %++% '/' %++% filename %++% '/' 
  filepath = 
    dest %++% '/' %++% filename %++% '/' %++% filename %++% '.json'

  if (!dir.exists(dirpath))
    dir.create(dirpath, recursive = TRUE)
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
