#'
#' gvmapAutoConfig
#'
#' Generating gvmap config file automaticly
#'
#' @param heatmap_info list, heatmap data information
#' @param legend_info data frame, legend data information
#' @param color_info list, color information
#'
#' @export
#' @return a config file to run gvmap
#' @examples
#' heatmap_info <- list("heatmap_1", "heatmap_2", "heatmap_3")
#' legend_info <- data.frame(legend_group = c("legend_1", "legend_1", "legend_2"),
#'                           name = c("Age", "Gender", "NRAS"),
#'                           col_theme = c("binary_col", "binary_col", "tag_col"),
#'                           column_num = c(2, 3, 4))
#' config_file <- gvmapAutoConfig(heatmap_info = heatmap_info,
#'                                legend_info = legend_info)
#'
#'
gvmapAutoConfig <- function(heatmap_info,
                            legend_info,
                            color_info) {
  map_config <- list()
  color_config <- list()

  if (missing(heatmap_info)) {
    message("[INFO] heatmap_info is missing, the heatmap will not plot")
    heatmap_num <- 0
    map_config <- c(heatmap_num = 0)
  } else {
    if (is.list(heatmap_info)) {
      heatmap_num <- length(heatmap_info)
      # map_heatmap_info <- list(rep(NULL, heatmap_num))
      names(heatmap_info) <- paste0("heatmap_", 1:heatmap_num)
      map_config <- c(heatmap_num = heatmap_num,
                      heatmap_info)
    } else {
      stop("[ERROR] heatmap_info must be a list")
    }
  }

  if (missing(legend_info)) {
    message("[INFO] legend_info is missing, the legend will not plot")
    legend_num <- 0
    map_config <- c(map_config, legend_num = 0)
  } else {
    if (is.data.frame(legend_info)) {
      legend_num <- length(unique(legend_info[, 1]))
      map_config <- c(map_config,
                      legend_num = legend_num)
      for (i in 1:legend_num) {
        legend_sub <- legend_info[which(legend_info[, 1] == unique(legend_info[, 1])[i]), ]
        legend_sub_config <- lapply(1:length(legend_sub[, 1]), function(x) return(list(legend_sub[x, 2], legend_sub[x, 3], legend_sub[x, 4])))
        legend_sub_config <- list(legend_sub_config)
        names(legend_sub_config) <- paste0("legend_", i)
        map_config <- c(map_config, legend_sub_config)
      }
    } else {
      stop("[ERROR] heatmap_info must be a data frame")
    }
  }

  if (missing(color_info)) {
    message("[INFO] use default color theme instead")
  } else {
    color_config <- c(color_config = list(color_info))
  }

  config_file <- list(map_config = map_config)
  config_file <- gvmapConfig(config_file)

  return(config_file)
}
