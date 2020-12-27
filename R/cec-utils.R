get_benchmark_setup = function(cec, suite = NA, dim = NA) {
  if (cec == 13) {
    list(
     scores =  c(seq(-1400, -100, by = 100), seq(100, 1400, 100)) + 1500,
     eval_func = function(n, x) { cecs::cec2013(n, x) + 1500 }
    )
  } 
  else if (cec == 14) {
    list(
     scores = seq(100, 3000, by = 100), 
     eval_func = function(n, x) cecs::cec2014(n, x)
    )
  }
  else if (cec == 17) {
    list(
     scores = seq(100, 3000, by = 100), 
     eval_func = function(n, x) cecs::cec2017(n, x)
    )
  }
  else if (cec == 21) {
    list(
     scores = c(100, 1100, 700, 1900, 1700, 1600, 2100, 2200, 2400, 2500),
     eval_func = function(n, x, s) cecs::cec2021(n, x, suite)
    )
  }
}

get_budget_step = function(cec, suite = NA, dim = NA) {
 # cec2021_scores = function(dim) {
      dim^((0:15 / 5) - 3)
 #   }
 # if (cec %in% c(13, 14, 17))
 #    budget_steps = c(0.01, 0.02, 0.03, 0.05, seq(0.1, 1.0, by = 0.1)) 
 # else {
 #    if (is.null(suite))
 #      stop("Specify benchmark suite!")
 #    budget_steps = cec2021_scores(dim) 
 # }
}
