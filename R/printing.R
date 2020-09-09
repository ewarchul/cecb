#' Benchmark results basic stats
#'
#' @description
#' Function prints on STDIN basic statistics of benchmark result vector.
#' @param vec vector with results
#' @export

print_stats = function(vec) {
  cat(stringr::str_interp(
"Statistics:
  Median: ${median(vec)}
  Mean: ${mean(vec)}
  Max: ${max(vec)}
  Min: ${min(vec)}
  Std: ${sd(vec)}\n"
      )
  )
}


