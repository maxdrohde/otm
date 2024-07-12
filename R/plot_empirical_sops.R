plot_empirical_sops <- function(df){
  df |>
    dplyr::mutate(y = as.factor(y)) |>
    dplyr::group_by(t, y, tx) |>
    dplyr::count() |>
    dplyr::group_by(t, tx) |>
    dplyr::mutate(n = n / sum(n)) |>
    ggplot2::ggplot() +
    ggplot2::aes(x = t, y = n, color=y, linetype=tx) +
    ggplot2::geom_line() +
    ggplot2::geom_point(alpha=0.6, size=0.25) +
    ggplot2::scale_x_continuous(breaks = unique(df$t)) +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by=0.1)) +
    ggplot2::coord_cartesian(ylim = c(0,1)) +
    ggplot2::labs(x = "Time",
         y = "Empirical State Occupancy Probability",
         color = "Ordinal State",
         linetype = "Treatment") +
    ggplot2::theme_light()
}
