#' Benchmark frontend
#'
#' @description
#' Function takes configuration of benchmark list and runs benchmark.
#' @param config a list written by user or read from YAML configuration file.
#' @export

run_benchmark <- function(config) {
  parsed_config <-
    parse_config(config)
  tibble::tibble(
    method = parsed_config$methods_sym,
    id = parsed_config$ids
  ) %>%
    purrr::pmap(function(method, id) {
      c(bench_data, time) %<-%
        benchmark_parallel(
          .method = method,
          .probnum = parsed_config$probnum,
          .cec = parsed_config$cec,
          .dims = parsed_config$dims,
          .rep = parsed_config$repnum,
          .cpupc = parsed_config$cpu,
          .method_id = id
        )

      if (parsed_config$save == 1) {
        bench_data %T>%
          save_results(
            dest = parsed_config$dest,
            filename = id
          )
      }
      save_metadata(
        dest = parsed_config$dest,
        filename = id,
        info =
          list(
            method = method,
            time = time,
            id = id
          )
      )
    })
}
