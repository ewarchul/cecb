#' Run benchmark parallely
#'
#' @description
#' Function runs benchmark `.cec` on `.method` for given test functions and dimensionality.
#' Evalution on each function is repeated `.rep` times. User is able to
#' specify usage of CPU cores by `.cpupc` arg.
#' @param .method optimization algorithm :: function
#' @param .probnum indices of problem function :: [Int]
#' @param .dims dimensionalities :: [Int]
#' @param .rep amount of repetition :: Int
#' @param .cec year of benchmark :: Int
#' @param .cpupc CPU usage in pct :: Int
#' @importFrom foreach "%dopar%"

benchmark_parallel <- function(.method,
                               .probnum,
                               .dims,
                               .rep,
                               .cec = 17,
                               .cpupc = .75,
                               .write_flag = TRUE,
                               .method_id) {
  cli::cli_alert("(problem, dimension, repetition)\n")
  bench_start <- Sys.time()
  if (.cec == 17) {
    scores <- seq(100, 3000, by = 100)
  } else {
    scores <- c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500
  }
  no_cores <-
    floor(.cpupc * parallel::detectCores())
  doParallel::registerDoParallel(no_cores)

  benchmark_state <-
    collections::dict()
  problem_state <-
    collections::dict()

  for (d in .dims) {
    results <- foreach::foreach(
      n = .probnum,
      .combine = c,
      .export = c("scores", "d", ".cec", "problem_state")
    ) %dopar% {
      resultVector <- c()
      resets <- c()
      informMatrix <- matrix(0, nrow = 14, ncol = .rep)
      iteration_state <- collections::dict()
      prog_bar = 
        progress::progress_bar$new(
          format = "Method :alg | Dim. :dim | Prob. No. :prob [:bar] :current/:total (:percent)",
          total = .rep
        )
      prog_bar$tick(0)
      for (i in 1:.rep) {
        time_start <- Sys.time()
        prog_bar$tick(1, tokens = list(
          alg = .method_id,
          prob = n,
          dim = d
        ))
        result <- tryCatch(
          {
            .method(
              rep(0, d),
              fn = function(x) {
                if (.cec == 17) {
                  cec2017::cec2017(n, x)
                } else {
                  cec2013::cec2013(n, x) + 1500
                }
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
        recordedTimes <- c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
        for (bb in 1:length(recordedTimes)) {
          informMatrix[bb, i] <- abs(result$diagnostic$bestVal[recordedTimes[bb] * ceiling(nrow(result$diagnostic$bestVal)), ] - scores[n])
        }
        time_end <- round(as.numeric(Sys.time() - time_start, unit = "mins"), 2)
        iteration_state$set(i, informMatrix[, i])
      }
      problem_state$set(n, iteration_state$as_list())$as_list()
    }
    benchmark_state$set(d, results)
  }
  doParallel::stopImplicitCluster()
  list(
    data_comp = benchmark_state$as_list(),
    time = round(as.numeric(Sys.time() - bench_start, unit = "mins"), 2)
  )
}
