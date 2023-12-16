# Declare variables for use in ggplot2
utils::globalVariables(c("sop", "day", "y"))

generate_barplot <- function(sop_dataframe){
  sop_dataframe |>
    ggplot2::ggplot() +
    ggplot2::aes(x = day, y = sop, fill = y) +
    ggplot2::geom_col(position="fill") +
    ggplot2::scale_x_continuous(breaks = 1:28) +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    ggplot2::scale_fill_brewer(palette = "YlOrRd") +
    ggplot2::labs(x = "Study Day",
         y = "SOP",
         fill = "State") +
    cowplot::theme_cowplot()
}

generate_lineplot <- function(sop_dataframe){
  sop_dataframe |>
    ggplot2::ggplot() +
    ggplot2::aes(x = day, y = sop, color=y) +
    ggplot2::geom_line() +
    ggplot2::geom_point(alpha=0.6, size=1.5) +
    ggplot2::scale_x_continuous(breaks = 1:28) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by=0.1)) +
    ggplot2::coord_cartesian(ylim = c(0,1)) +
    ggplot2::labs(x = "Study Day",
         y = "SOP",
         color = "State") +
    cowplot::theme_minimal_hgrid()
}

#' Plot the SOPs implied by the OTM parameters
#'
#' @param sop_dataframe A
#' @return Matrix of transition probabilities
#' @export
generate_sop_plot <- function(sop_dataframe){

  # Make sure `y` is a factor
  if (!is.factor(sop_dataframe$y)) {
    sop_dataframe$y <- as.factor(sop_dataframe$y)
  }

  bar <- generate_barplot(sop_dataframe)
  line <- generate_lineplot(sop_dataframe)

  combined <-
    patchwork::wrap_plots(
      bar,
      line,
      ncol = 1
    )

  return(combined)
}
