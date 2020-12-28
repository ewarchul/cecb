#' Benchmark frontend
#'
#' @description
#' Function takes configuration of benchmark list and runs benchmark.
#' @param config a list written by user or read from YAML configuration file.
#' @export

run_benchmark <- function(config) {
  parsed_config <-
    parse_config(config)
  benchmark_data = 
    expand.grid(
      method = parsed_config$methods_sym,
      id = parsed_config$ids
    ) %>%
    run(parsed_config)
  error_table = 
    benchmark_data %>%
    purrr::flatten() %>%
    save_data(parsed_config)
  return(error_table)
}


run = function(grid, setup) {
grid %>%
  purrr::pmap(function(method, id) {
    bench_data =
      benchmark_parallel(
        method = method,
        probnum = setup$probnum,
        cec = setup$cec,
        dims = setup$dims,
        rep = setup$repnum,
        suite = setup$suite,
        cpupc = setup$cpu,
        benchmark_id = id
      )
    if (setup$save == 1) {
      bench_data %T>%
        save_results(
          dest = setup$dest,
          filename = id
        )
    }
    save_metadata(
      dest = setup$dest,
      filename = id,
      info =list(method = method, id = id)
    )
      list(data = bench_data)
  })
}

save_data = function(df, setup) {
  df %>%
    purrr::map(function(df) {
      generate_error_table(
        input = df,
        dims = setup$dims,
        probs = setup$probnum,
        reps = 1:setup$repnum,
        cec = setup$cec
      ) %T>%
      save_tables(
        dirpath = setup$dest
      )
    })
}
