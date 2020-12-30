#' ECDF plot
#' 
#' @description
#' Function plots ECDF curves.
#' @param .dfx data frame with benchmark results
#' @export

ecdf_plot = function(.dfx) {
  .dfx %>%
    ggplot2::ggplot(ggplot2::aes(x = Bstep)) +
    ggplot2::geom_point(ggplot2::aes(y = Value, shape = Method, color = Method), size = 0.5) +
    ggplot2::geom_line(ggplot2::aes(y = Value, linetype = Method, color = Method), size = 1.2) +
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::theme_bw() +
    ggplot2::xlab("log10 of (f-evals / dimension)") +
    ggplot2::ylab("Proportion of function + target pairs") +
    ggplot2::ylim(0, 1) +
    ggplot2::theme(
    axis.title = ggplot2::element_text(size = 15, face = "bold"),
    axis.text = ggplot2::element_text(size = 15, face = "bold"),
    legend.text = ggplot2::element_text(size = 15, face = "bold"),
    legend.title = ggplot2::element_text(size = 15, face = "bold"),
          )
}


#' ECDF in function classes
#'
#' @export

cec_class_grid = function(filepaths, dim, cec = 17, rep = 51) {
  class = get_class_div(cec)
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


get_class_div = function(cec) {
  if (cec %in% c(14, 17)) {
   list(
      unimodal = 1:3,
      multimodal = 4:10,
      hybrid = 11:20,
      composition = 21:30
    )
  }
  else if (cec == 13) {
    list(
      unimodal = 1:5,
      multimodal = 6:10,
      hybrid = 11:20,
      composition = 21:30
    )
  } else {
    list(
      unimodal = 1,
      multimodal = 2:4,
      hybrid = 5:7,
      composition = 8:10
    )
  }
}