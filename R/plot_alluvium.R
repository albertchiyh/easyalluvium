#' Plot Alluvial Diagram
#'
#' This function creates an alluvial plot using specified grouping variables, allowing
#' for flexible color filling based on either a static color or a column in the dataset.
#'
#' @param data A data frame containing the data to be plotted.
#' @param grouping_vars A character vector of column names in \code{data} to be used for the plot axes.
#' @param fill_color A color name for the alluvium fill or the name of a column in \code{data} to define
#'   variable-based coloring. Default is "yellow".
#' @param stratum_color A color name for the stratum borders. Default is "black".
#' @param x_labels Optional character vector of labels for the x-axis categories. If NULL, \code{grouping_vars} are used.
#' @param title A title for the plot. Default is "Alluvial Diagram".
#' @param x_title A label for the x-axis. Default is "Categories".
#' @param y_title A label for the y-axis. Default is "Frequency".
#'
#' @return A ggplot object representing the alluvial diagram.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggalluvial
#'
#' @examples
#' \dontrun{
#'   dt <- read.csv("/path/to/essay_grades.csv")
#'   plot_alluvium(data = dt,
#'                 grouping_vars = c("essay1", "essay2", "essay3"),
#'                 fill_color = "yellow",
#'                 stratum_color = "black",
#'                 x_labels = c("Essay 1", "Essay 2", "Essay 3"),
#'                 title = "Alluvial Diagram of Student Performance on Essays",
#'                 x_title = "Essays",
#'                 y_title = "Number of Students")
#' }
#'
#' @export
plot_alluvium <- function(data, grouping_vars, fill_color = "yellow", stratum_color = "black",
                                           x_labels = NULL, title = "Alluvial Diagram", x_title = "Categories",
                                           y_title = "Frequency") {
  # Ensure the grouping variables are part of the data
  if (!all(grouping_vars %in% names(data))) {
    stop("One or more grouping variables are not in the data.")
  }

  # Group the data and summarize frequency
  data_freq <- data |>
    group_by(across(all_of(grouping_vars))) |>
    summarize(Freq = n(), .groups = "drop") |>
    ungroup()

  # Dynamically create axis mappings
  axis_mappings <- list()
  for (i in seq_along(grouping_vars)) {
    axis_mappings[[paste0("axis", i)]] <- sym(grouping_vars[i])
  }

  # Create the plot
  plot <- ggplot(data = data_freq, aes(y = Freq)) +
    geom_alluvium(aes(!!!axis_mappings), color = stratum_color, fill = fill_color) +
    geom_stratum(aes(!!!axis_mappings)) +
    geom_text(stat = "stratum", aes(!!!axis_mappings, label = after_stat(stratum))) +
    scale_x_discrete(limits = if (is.null(x_labels)) grouping_vars else x_labels, expand = c(.1, .1)) +
    labs(title = title, x = x_title, y = y_title) +
    theme_minimal()

  return(plot)
}
