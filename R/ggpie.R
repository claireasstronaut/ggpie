#' Draw pie charts
#'
#' Draw pie charts with ggplot2.
#'
#' @param df A data frame.
#' @param .x A categorical variable.
#' @param .y A numeric variable.
#' @param .offset A number, the distance between the center and label.
#' @param .number If the number appears in the label.
#' @param .legend If the legend is kept.
#' @param .round The round number in the label.
#' @param .percent If the percent appears in the label.
#' @param .start The starting angle.
#' @param .textcolor The color of the label.
#' @examples
#' df <- data.frame(type = c("a", "b", "c"), value = c(5, 3, 2))
#' ggpie(df, type, value)
#' @export

ggpie <- function(df, .x, .y, .offset = 1, .color = "white", .number = TRUE,
                  .legend = FALSE, .round = 1, .percent = TRUE, .start = 0,
                  .textcolor = "white") {
  .x <- as.character(substitute(.x))
  .y <- as.character(substitute(.y))
  df_plot <- df
  df_plot$offset <- .offset
  df_plot[[.x]] <- forcats::fct(df_plot[[.x]])
  df_plot$perc <- with(df_plot, round(get(.y) / sum(get(.y)) * 100, .round))
  df_plot$label <- df_plot[[.x]]
  if (.number) df_plot$label <- with(df_plot, paste0(label, ": ", prettyNum(get(.y), ",")))
  if (.percent) df_plot$label <- with(df_plot, paste0(label, "\n(", perc, " %)"))
  df_plot$y <- with(df_plot, (sum(get(.y)) - cumsum(get(.y)) + get(.y)/2))

  ggplot2::ggplot(df_plot, ggplot2::aes(1, .data[[.y]], fill = .data[[.x]])) +
    ggplot2::geom_col(color = .color) +
    ggplot2::geom_text(ggplot2::aes(label = label, y = y, x = offset), color = .textcolor) +
    ggplot2::coord_polar("y", start = .start) +
    cowplot::theme_cowplot() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank()
    ) +
    {if (!.legend) ggplot2::theme(legend.position = "none")} +
    ggplot2::labs(x = "", y = "")
}
