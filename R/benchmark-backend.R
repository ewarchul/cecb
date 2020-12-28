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

benchmark_parallel <- function(
  method,
  probnum,
  dims,
  rep,
  cec,
  suite = "basic",
  cpupc = .75,
  write_flag = TRUE,
  benchmark_id
) {
  no_cores <- floor(cpupc * parallel::detectCores())
  benchmark_state <- collections::dict()
  problem_state <- collections::dict() 
  doParallel::registerDoParallel(no_cores)
  eval_func = get_eval_func(cec, suite)
  for (d in dims) {
    results <- foreach::foreach(
      n = probnum,
      .combine = c,
      .export = c("d", "problem_state")
    ) %dopar% {
      iteration_state <- collections::dict() 
      prog_bar = 
        progress::progress_bar$new(
          format = "Run :: :alg  (D = :dim, F = :prob) [:bar] :current/:total (:percent)\n",
          total = rep
        )
      prog_bar$tick(0)
      for (i in 1:rep) {
        prog_bar$tick(1, tokens = list(alg = benchmark_id, prob = n, dim = d))
        result <- tryCatch(
          {
            method(
              runif(d, -100, 100),
              fn = function(x) { eval_func(n, x) },
              lower = -100,
              upper = 100
            )
          },
          error =
            function(cond) {
              print(paste("Dim:", d, " Problem:", n, " ", cond))
            }
        )
        iteration_state$set(i, result$diagnostic$bestVal)
      }
      problem_state$set(n, iteration_state$as_list())$as_list()
    }
    benchmark_state$set(d, results)
  }
  doParallel::stopImplicitCluster()
  list(
    data_comp = benchmark_state$as_list(),
    benchmark_id = benchmark_id
  )
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
