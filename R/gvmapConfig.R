#'
#' Read config file of gvmap
#'
#' @examples
#' config_file <- "inst/extdata/config.yaml"
gvmapConfig <- function(config_file) {
  if (is.yaml.file(config_file)) {
    config_info <- read.config(config_file)
  } else {
    stop("must input a right config file of YAML format")
  }

  return(config_info)
}

#'
#' read legend data
#'
#' @examples
#' legend_data <- "inst/extdata/gvmap.test.txt"
readLegendFile <- function(legend_data) {
  if (typeof(legend_data) == "character") {
    data_info <- read.table(legend_data, header = T, sep = "\t", quote = '"')
    row.names(data_info) <- data_info[, 1]
  } else {
    data_info <- legend_data
  }

  return(data_info)
}

#'
#' read heatmap data
#'
#' heatmap_data <- "inst/extdata/count.txt"
readHeatmapFile <- function(heatmap_data) {
  if (typeof(heatmap_data) == "character") {
    data_info <- read.table(heatmap_data, header = T, sep = "\t", quote = '"')
    row.names(data_info) <- data_info[, 1]
    data_info <- data_info[, -1]
  } else {
    data_info <- heatmap_data
  }

  if(!is.matrix(data_info)) {
    data_info <- as.matrix(data_info)
  }
  if(!is.matrix(data_info)) stop("Heatmap data must be a matrix")

  return(data_info)
}

#'
#' checkPlotConfig
#'
#' heatmap 60%
#' legmap 20%
#' legmap explain + other 20%
#'
#'
checkPlotConfig <- function(plot_config) {

  group_span <- 10

  if (plot_config$heatmap_plot) {
    heatmap_height <- plot_config$plot_height * 0.6

    heatmap_row_rect <- heatmap_height * 0.8 / plot_config$heatmap_row_num
    heatmap_col_rect <- plot_config$plot_width * 0.7 / plot_config$heatmap_col_num

    heatmap_row_fz <- floor(heatmap_height * 0.8 / plot_config$heatmap_row_num)
    heatmap_col_fz <- floor(plot_config$plot_width * 0.3 / plot_config$heatmap_col_num)
    heatmap_col_fz <- min(20, heatmap_col_fz)

    sample_span <- heatmap_col_rect

    if (is.null(plot_config$sample_font_size)) {
      plot_config$sample_font_size <- heatmap_col_fz
    }

    plot_config <- c(plot_config,
                     heatmap_height = heatmap_height,
                     heatmap_row_fz = heatmap_row_fz,
                     heatmap_col_fz = heatmap_col_fz,
                     heatmap_row_rect = heatmap_row_rect,
                     heatmap_col_rect = heatmap_col_rect)
  }
  if (plot_config$legmap_plot) {
    legmap_row_rect <- min(30, plot_config$plot_height * 0.2 / sum(plot_config$legmap_row_num))
    legmap_col_rect <- plot_config$plot_width * 0.7 / plot_config$legmap_col_num

    legmap_row_fz <- min(15, legmap_row_rect * 0.8)
    legmap_col_fz <- floor(plot_config$plot_width * 0.3 / plot_config$legmap_col_num)
    legmap_col_fz <- min(20, legmap_col_fz)

    legmap_height <- plot_config$legmap_row_num * legmap_row_rect

    sample_span <- legmap_col_rect
    group_span <- legmap_row_rect

    if (is.null(plot_config$legend_font_size)) {
      plot_config$legend_font_size <- legmap_row_fz
    }

    plot_config <- c(plot_config,
                     list(legmap_height = legmap_height),
                     legmap_row_rect = legmap_row_rect,
                     legmap_col_rect = legmap_col_rect,
                     legmap_row_fz = legmap_row_fz,
                     legmap_col_fz = legmap_col_fz)

  }

  if (is.null(plot_config$group_span)) {
    plot_config$group_span <- group_span
  }
  if (is.null(plot_config$sample_span)) {
    plot_config$sample_span <- sample_span
  }

  dend_stroke <- 2

  plot_config <- c(plot_config,
                  dend_stroke = dend_stroke)

  return(plot_config)
}

























