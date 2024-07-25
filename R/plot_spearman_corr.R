plot_spearman_corr <- function(df, absorb = 999, tmax = 999){

  df <- otm:::carry_absorbing_forward(df,
                                      absorb = absorb,
                                      tmax = tmax)

  corr <-
    df |>
    dplyr::select(id, t, y) |>
    dplyr::arrange(t) |>
    dplyr::mutate(y = as.integer(y)) |>
    tidyr::pivot_wider(names_from = t, values_from = y) |>
    dplyr::ungroup() |>
    dplyr::select(-id) |>
    cor(method="spearman", use="pairwise.complete.obs")

  fig_corr <-
    corr |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    tidyr::pivot_longer(-rowname) |>
    dplyr::mutate(rowname = as.integer(rowname), name = as.integer(name)) |>
    dplyr::mutate(rowname = as.factor(rowname), name = as.factor(name)) |>
    dplyr::filter(rowname != name) |>
    ggplot2::ggplot() +
    ggplot2::aes(x=rowname, y=name, fill=value) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(mapping=ggplot2::aes(x=rowname, y=name, label=round(value, 2)),
              color="black",
              size = 2) +
    ggplot2::scale_fill_viridis_c(limits=c(0,1), breaks = seq(0,1, length.out = 11)) +
    ggplot2::expand_limits(value = c(0, 1)) +
    cowplot::theme_half_open(font_size=12,
                             font_family = "Source Sans Pro") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank()) +
    ggplot2::theme(legend.key.height = ggplot2::unit(0.8, "in")) +
    ggplot2::labs(x="Study Day",
         y="Study Day",
         fill="")

  return(fig_corr)
}
