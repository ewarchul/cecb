#' ECDF plot
#'
#' @description
#' Function plots ECDF curves.
#' @param dfx data frame with benchmark results
#' @param aoc binary flag 
#' @param ymax height of the table with AOC values
#' @export

ecdf_plot <- function(dfx, aoc = FALSE, ymax = 0.3) {
  gplot = 
    dfx %>%
      ggplot2::ggplot(ggplot2::aes(x = Bstep)) +
      ggplot2::geom_point(ggplot2::aes(y = Value, shape = Method, color = Method), size = 0.5) +
      ggplot2::geom_line(ggplot2::aes(y = Value, linetype = Method, color = Method), size = 1.2) +
      ggplot2::scale_colour_brewer(palette = "Dark2") +
      ggplot2::theme_bw() +
      ggplot2::xlab("log10 of (f-evals / dimension)") +
      ggplot2::ylab("Proportion of function + target pairs") +
      ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 15, face = "bold"),
        axis.text = ggplot2::element_text(size = 15, face = "bold"),
        legend.text = ggplot2::element_text(size = 15, face = "bold"),
        legend.title = ggplot2::element_text(size = 15, face = "bold"),
      )
  if (aoc) {
    gplot + 
      ggplot2::annotation_custom(
        gridExtra::tableGrob(compute_aoc(dfx)),
          xmin = 3, ymin = 0,
          xmax = 4, ymax = ymax
      )
  } else {
    gplot
  } 
}
