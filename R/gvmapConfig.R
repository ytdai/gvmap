#'
#' Read config file of gvmap
#'
#' This function is used to check basic parameter in config file.
#' If a basic paramter in map_config is missing, the program will stop.
#' If a basic paramter in color_config is missing, it will use default color theme.
#' @param config_file the config file you input
#' @examples
#' config_file <- "inst/extdata/config.yaml"
#' gvmap_config <- gvmapConfig(config_file)
#'
gvmapConfig <- function(config_file) {
  default_color_theme <- list(
    bg_col = "#EEEEEE",
    tag_col = "#696969",
    binary_col = c("#196ABD", "#C20B01"),
    white_col = "#FFFFFF",
    black_col = "#000000",
    mutation_col = list(missense = "#3987CC",
                        frameshift = "#DB3D3D",
                        nonsense = "#FF7F0E",
                        protein_del = "#7F7F7F",
                        protein_ins = "#8C564B",
                        one_hit = "#819981",
                        tow_hit = "#499E49",
                        three_hit = "#237023"),
    pool_col = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A"),
    gradient_col_gr = c("green", "black", "red"),
    gradient_col_yp = c("yellow", "white", "purple"),
    gradient_col_br = c("blue", "white", "red"),
    gradient_col_gw = c("green", "white"),
    gv_line_col = "#FDBF6F",
    gv_bar_col = "#E6AB02",
    gv_dot_col = "#000000"
  )
  if (is.yaml.file(config_file)) {
    config_info <- read.config(config_file)
  } else {
    stop("must input a right config file of YAML format")
  }

  if (!"map_config" %in% names(config_info)) {
    stop("[ERROR] map_config info must be provided!")
  }

  if (!"heatmap_num" %in% names(config_info$map_config)) {
    stop("[ERROR] heatmap_num in map_config info must be provided!")
  }

  if (!"legend_num" %in% names(config_info$map_config)) {
    stop("[ERROR] legend_num in map_config info must be provided!")
  }

  if (config_info$map_config$heatmap_num > 0) {
    heatmap_name <- paste0("heatmap_", c(1:config_info$map_config$heatmap_num))
    if (!all(heatmap_name %in% names(config_info$map_config))) {
      stop("[ERROR] Unrecognized heatmap name in map_config")
    }
    for (i in 1:config_info$map_config$heatmap_num) {
      heatmap_name_i <- paste0("heatmap_", i)
      hid <- which(names(config_info$map_config) == heatmap_name_i)
      hparam <- list()
      if (!"raw_data" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, raw_data = FALSE)
      }
      if (!"kmer_col" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, kmer_col = 0)
      }
      if (!"kmer_row" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, kmer_row = 0)
      }
      if (!"split_row_name" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, list(split_row_name = NULL))
      }
      if (!"percentage" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, percentage = 0.6/config_info$map_config$heatmap_num)
      }
      if (!"kmer_col_color" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, kmer_col_color = "#000000")
      }
      if (!"kmer_row_color" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, kmer_row_color = "#000000")
      }
      if (!"dendrogram" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, dendrogram = c("both"))
      }
      if (!"Rowv" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, Rowv = TRUE)
      }
      if (!"Colv" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, Colv = TRUE)
      }
      if (!"na.rm" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, na.rm = TRUE)
      }
      if (!"color_theme" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, color_theme = "gradient_col_gr")
      }
      if (!"distfun" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, hclustfunc = dist)
      } else {
        if (config_info$map_config[[hid]]$distfun == "sih") {
          config_info$map_config[[hid]]$distfun <- function(x) as.dist((1-cor(t(x)))/2)
        } else {
          config_info$map_config[[hid]]$distfun <- dist
        }
      }
      if (!"hclustfun" %in% names(config_info$map_config[[hid]])) {
        hparam <- c(hparam, hclustfun = hclust)
      } else {
        if (config_info$map_config[[hid]]$hclustfun == "sih") {
          config_info$map_config[[hid]]$hclustfun <- function(x) hclust(x,method = 'ward.D2')
        } else {
          config_info$map_config[[hid]]$hclustfun <- hclust
        }
      }
      config_info$map_config[[hid]] <- c(config_info$map_config[[hid]], hparam)
    }
  }

  if (config_info$map_config$legend_num > 0) {
    legend_name <- paste0("legend_", c(1:config_info$map_config$legend_num))
    if (!all(legend_name %in% names(config_info$map_config))) {
      stop("[ERROR] Unrecognized legend name in map_config")
    }
  }

  if (!"map_order" %in% names(config_info$map_config)) {
    map_order <- grep('^heatmap_[0-9]|^legmap_[0-9]', names(config_info$map_config))
    map_order <- names(config_info$map_config)[map_order]
    config_info$map_config$map_order <- map_order
  }

  if (!"split_sample" %in% names(config_info$map_config)) {
    config_info$map_config$split_sample = NULL
  }

  if (!"color_config" %in% names(config_info)) {
    message("[WARNING] color_config info is not provided!")
    message("[INFO] Use default color theme!")

    config_info <- c(config_info, default_color_theme)
  } else {
    chk <- names(default_color_theme) %in% names(config_info$color_config)
    if (!all(chk)) {
      config_info$color_config <- c(config_info$color_config,
                                    default_color_theme[which(!chk)])
    }
  }

  return(config_info)
}


#'
#' read legend data
#'
#' @examples
#' legend_data_file <- "inst/extdata/gvmap.test.txt"
#' legend_data <- readLegendFile(legend_data_file)
#'
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
#' @examples
#' heatmap_data_file <- "inst/extdata/count.txt"
#' heatmap_data_file <- list(heatmap_1 = heatmap_data_file)
#' heatmap_data <- readHeatmapFile(heatmap_data_file)
#'
readHeatmapFile <- function(heatmap_data) {
  if (!is.list(heatmap_data)) {
    stop("[ERROR] The heatmap data must be a list, try heatmap_data <- list(heatmap_1 = heatmap_data)")
  }

  for (i in 1:length(names(heatmap_data))) {
    if (typeof(heatmap_data[[i]]) == "character") {
      data_info <- read.table(heatmap_data[[i]], header = T, sep = "\t", quote = '"')
      row.names(data_info) <- data_info[, 1]
      data_info <- data_info[, -1]
    } else {
      data_info <- heatmap_data[[i]]
    }
    if(!is.matrix(data_info)) {
      data_info <- as.matrix(data_info)
    }
    if(!is.matrix(data_info)) stop("Heatmap data in the list content must be a matrix")
    heatmap_data[[i]] <- data_info
  }

  return(heatmap_data)
}

























