#' ECDF plot
#' 
#' @description
#' Function plots ECDF curves.
#' @param .dfx data frame with benchmark results
#' @export

ecdf_plot = function(.dfx) {
  .dfx %>%
    ggplot2::ggplot(aes(x = Bstep)) +
    ggplot2::geom_point(aes(y = Value, shape = Method, color = Method), size = 0.5) +
    ggplot2::geom_line(aes(y = Value, linetype = Method, color = Method), size = 1.2) +
    ggplot2::scale_colour_brewer(palette="Dark2") +
    ggplot2::theme_bw() +
    xlab("log10 of (f-evals / dimension)") +
    ylab("Proportion of function + target pairs") +
    ylim(0, 1) +
    theme(
    axis.title = element_text(size = 15, face = "bold"),
    axis.text = element_text(size = 15, face = "bold"),
    legend.text = element_text(size = 15, face = "bold"),
    legend.title = element_text(size = 15, face = "bold"),
          )
}
