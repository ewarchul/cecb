#' Run benchmark parallely
#'
#' @description
#' Function runs benchmark `.cec` on `.method` for given test functions and dimensionality.
#' Evalution on each function is repeated `.rep` times. User is able to
#' specify usage of CPU cores by `.cpupc` arg.
#' @param method optimization algorithm :: function
#' @param probnum indices of problem function :: [Int]
#' @param dims dimensionalities :: [Int]
#' @param rep amount of repetition :: Int
#' @param cec year of benchmark :: Int
#' @param suite benchmark suite :: String
#' @param cpupc CPU usage in pct :: Int
#' @param benchmark_id benchmark name :: String
#' @param dest filepath to place where benchmark restuls will be saved :: String
#' @importFrom foreach "%dopar%"
#' @export

benchmark_parallel <- function(method, probnum, dims,
                               rep, cec = 17, suite = "basic", cpupc = .75,
                               write_flag = TRUE, benchmark_id,
                               dest) {
  cli::cli_alert("(problem, dimension, repetition)\n")
  scores <- get_scores(cec, suite)
  eval_func <- get_eval_func(cec, suite)
  no_cores <- floor(cpupc * parallel::detectCores())
  doParallel::registerDoParallel(no_cores)
  eval_func <- get_eval_func(cec, suite)
  for (d in dims) {
    results <- foreach::foreach(
      n = probnum,
      .combine = c,
      .export = c("scores", "d", "cec")
    ) %dopar% {
      resultVector <- c()
      resets <- c()
      error_table_old <- matrix(0, nrow = 14, ncol = rep)
      error_table_new <- matrix(0, nrow = 16, ncol = rep)
      for (i in 1:rep) {
        time_start <- Sys.time()
        result <- tryCatch(
          {
            cli::cli_alert_info("Start {benchmark_id}: ({n}, {d}, {i})\n")
            method(
              runif(d, -100, 100),
              fn = function(x) {
                eval_func(n, x)
              },
              lower = -100,
              upper = 100
            )
          },
          error =
            function(cond) {
              print(paste("Dim:", d, " Problem:", n, " ", cond))
            }
        )
        resultVector <- c(resultVector, abs(result$value - scores[n]))
        resets <- c(resets, result$resets)
        recordedTimes_old <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
        recordedTimes_new <- get_recordedTimes(d)
        for (bb in 1:length(recordedTimes_old)) {
          error_table_old[bb, i] <- abs(result$diagnostic$bestVal[ceiling(recordedTimes_old[bb] * nrow(result$diagnostic$bestVal)), ] - scores[n])
        }
        for (bb in 1:length(recordedTimes_new)) {
          error_table_new[bb, i] <- abs(result$diagnostic$bestVal[ceiling(recordedTimes_new[bb] * nrow(result$diagnostic$bestVal)), ] - scores[n])
        }
        time_end <- round(as.numeric(Sys.time() - time_start, unit = "mins"), 2)
        cli::cli_alert_success("Done {benchmark_id}: ({n}, {d}, {i} [in {time_end} mins])\n")
      }
      if (write_flag) {
        save_results(resultVector, cec, benchmark_id, n, d, "N", dest)
        save_results(error_table_old, cec, benchmark_id, n, d, "M", dest)
        save_results(error_table_new, cec, benchmark_id, n, d, "m", dest)
      }
    }
  }
  doParallel::stopImplicitCluster()
}
