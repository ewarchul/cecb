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
#' @param cpupc CPU usage in pct :: Int
#' @importFrom foreach "%dopar%"

benchmark_parallel = function(method, probnum, dims,
                              rep, cec = 17, suite = "basic", cpupc = .75,
                              write_flag = TRUE, method_id,
                              dest) {
  cli::cli_alert("(problem, dimension, repetition)\n")
  scores = get_scores(cec, suite)
  eval_func = get_eval_func(cec, suite)
  no_cores = floor(cpupc * parallel::detectCores())
  doParallel::registerDoParallel(no_cores)
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
        time_start = Sys.time()
        result <- tryCatch(
          {
            cli::cli_alert_info("Start {method_id}: ({n}, {d}, {i})\n")
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
        recordedTimes_old = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
        recordedTimes_new = get_recordedTimes(d)
        for (bb in 1:length(recordedTimes_old)) {
          error_table_old[bb, i] <- abs(result$diagnostic$bestVal[ceiling(recordedTimes_old[bb] * nrow(result$diagnostic$bestVal)), ] - scores[n])
        }
        for (bb in 1:length(recordedTimes_new)) {
          error_table_new[bb, i] <- abs(result$diagnostic$bestVal[ceiling(recordedTimes_new[bb] * nrow(result$diagnostic$bestVal)), ] - scores[n])
        }
        time_end = round(as.numeric(Sys.time() - time_start, unit = "mins"), 2)
        cli::cli_alert_success("Done {method_id}: ({n}, {d}, {i} [in {time_end} mins])\n")
      } 
      if (write_flag) {
        save_results(resultVector, cec, method_id, n, d, "N", dest)
        save_results(error_table_old, cec, method_id, n, d, "M", dest)
        save_results(error_table_new, cec, method_id, n, d, "m", dest)
      }
    }
  }
  doParallel::stopImplicitCluster()
}

get_eval_func = function(cec, suite) {
  if (cec == 13) {
    function(n, x) { cecs::cec2013(n, x) + 1500 }
  }
  else if (cec == 14) {
    function(n, x) { cecs::cec2014(n, x) }
  }
  else if (cec == 17) {
    function(n, x) { cecs::cec2017(n, x) }
  }
  else if (cec == 21) {
    function(n, x) { cecs::cec2021(n, x, suite) }
  }
}

get_recordedTimes = function(dim) {
    dim^(((0:15) / 5) - 3)
}

get_scores = function(cec, suite) {
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
