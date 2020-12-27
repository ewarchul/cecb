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

benchmark_parallel <- function(method,
                               probnum,
                               dims,
                               rep,
                               suite = NA,
                               cec = 17,
                               cpupc = .75,
                               write_flag = TRUE,
                               method_id) {
  bench_start <- Sys.time()
  c(scores, eval_func) %<~%
    get_benchmark_setup(cec)
  no_cores <- floor(cpupc * parallel::detectCores())
  benchmark_state <- collections::dict()
  problem_state <-collections::dict() 
  doParallel::registerDoParallel(no_cores)
  for (d in dims) {
    results <- foreach::foreach(
      n = probnum,
      .combine = c,
      .export = c("scores", "budget_steps", "d", "cec", "problem_state")
    ) %dopar% {
      budget_steps = get_budget_step(cec, suite, d)
      error_table <- matrix(0, nrow = 16, ncol = rep)
      iteration_state <- collections::dict() 
      prog_bar = 
        progress::progress_bar$new(
          format = "Run :: :alg  (D = :dim, F = :prob) [:bar] :current/:total (:percent)\n",
          total = rep
        )
      prog_bar$tick(0)
      for (i in 1:rep) {
        time_start <- Sys.time()
        prog_bar$tick(1,tokens = list(alg = method_id, prob = n, dim = d))
        result <- tryCatch(
          {
            method(
              runif(d, -100, 100),
              fn = function(x) {
                ifelse(cec == 21, eval_func(n, x, suite), eval_func(n, x))
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
        for (bb in 1:length(budget_steps)) {
          step = budget_steps[bb] * ceiling(nrow(result$diagnostic$bestVal)) 
          error_table[bb, i] <- abs(result$diagnostic$bestVal[step,] - scores[n])
        }
        time_end <- round(as.numeric(Sys.time() - time_start, unit = "mins"), 2)
        iteration_state$set(i, error_table[, i])
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
