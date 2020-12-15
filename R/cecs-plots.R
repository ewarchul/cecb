#' ECDF in function classes
#'
#' @export

cec_class_grid = function(filepaths, dim, cec = 17, rep = 51) {
  if (cec == 17)
    class = list(
      unimodal = 1:3,
      multimodal = 4:10,
      hybrid = 11:20,
      composition = 21:30
    )
  else 
    class = list(
      unimodal = 1:5,
      multimodal = 6:10,
      hybrid = 11:20,
      composition = 21:30
    )
  ecdf_values = 
    class %>% purrr::map_dfr(function(cls) {
      cecb::get_dfr(filepaths, list(dim = dim, fnc = cls, rep = rep)) %>%
      dplyr::mutate(Class = factor(stringr::str_glue("Functions: {dplyr::first(cls)} - {dplyr::last(cls)}")))
    })
  ecdf_values %>%
    cecb::ecdf_plot() + ggplot2::facet_wrap( ~ Class) + ggplot2::ggtitle(stringr::str_glue("CEC: {cec}"))
}

#' ECDF per problem
#'
#' @export

cec_problem_grid = function(filepaths, problems, dim, cec = 17, rep = 51) {
  ecdf_values = 
    problems %>% purrr::map_dfr(function(problem) {
      cecb::get_dfr(filepaths, list(dim = dim, fnc = problem, rep = rep)) %>%
      dplyr::mutate(Problem = factor(stringr::str_glue("Function: {problem}")))
    })
  ecdf_values %>%
    cecb::ecdf_plot() + ggplot2::facet_wrap( ~ Problem) + ggplot2::ggtitle(stringr::str_glue("CEC: {cec}"))
}
