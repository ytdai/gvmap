#
# heatmap plot parameter
#
getHeatmapParam <- function(heatmap_data, config_data, plot_config, i) {
  heatmap_name <- paste0("heatmap_", i)
  hid <- which(names(config_data$map_config) == heatmap_name)

  heatmap_subdata <-heatmap_data[[i]]
  heatmap_conf_data <- config_data$map_config[[hid]]

  if (i == 1) {
    plot_config$kmer_col <- heatmap_conf_data$kmer_col
  }

  nr <- dim(heatmap_subdata)[1]
  nc <- dim(heatmap_subdata)[2]

  h <- heatmap_conf_data$percentage * plot_config$plot_height
  w <- plot_config$plot_width
  row_dend_h <- 0.2 * w
  row_dend_w <- 0.8 * h
  col_dend_h <- 0.2 * h
  col_dend_w <- 0.7 * w

  if (plot_config$heatmap_row_span == 0) {
    rect_h <- row_dend_w / nr
  } else {
    rect_h <- (row_dend_w - (heatmap_conf_data$kmer_row - 1 + length(heatmap_conf_data$split_row_name)) * plot_config$heatmap_row_span ) / nr
  }

  row_fz <- min(10, floor(rect_h))
  row_fz <- max(2, row_fz)

  if (plot_config$sample_span == 0) {
    rect_w <- col_dend_w / plot_config$sample_num
  } else {
    rect_w <- (col_dend_w - (plot_config$kmer_col - 1 + length(config_data$map_config$split_sample)) * plot_config$sample_span) / nc
  }

  if (is.null(plot_config$sample_font_size)) {
    col_fz <- min(20, floor(rect_w))
    col_fz <- max(2, col_fz)
    plot_config$sample_font_size <- col_fz
  } else {
    col_fz <- plot_config$sample_font_size
  }

  hparam <- list(
    nr = nr,
    nc = nc,
    h = h,
    w = w,
    row_dend_h = row_dend_h,
    row_dend_w = row_dend_w,
    col_dend_h = col_dend_h,
    col_dend_w = col_dend_w,
    rect_h = rect_h,
    rect_w = rect_w,
    row_fz = row_fz,
    col_fz = col_fz
  )

  hparam <- list(a = hparam)
  names(hparam) <- heatmap_name

  plot_config$sample_text_width <- rect_w

  plot_config_com <- c(plot_config, hparam)

  return(plot_config_com)
}


#
# legend plot parameter
#
getLegendParam <- function(legend_data, config_data, plot_config) {
  legend_name <- paste0("legend_", 1:config_data$map_config$legend_num)
  lid <- match(legend_name, names(config_data$map_config))

  legend_num <- lapply(lid, function(x) length(config_data$map_config[[x]]))
  legend_num <- sum(unlist(legend_num))

  if (config_data$map_config$heatmap_num > 0) {
    heatmap_name <- paste0("heatmap_", 1:config_data$map_config$heatmap_num)
    hid <- match(heatmap_name, names(config_data$map_config))
    heatmap_pre <- lapply(hid, function(x) config_data$map_config[[x]]$percentage)
    heatmap_pre_sum <- sum(unlist(heatmap_pre))

    kmer_col <- config_data$map_config[[hid[1]]]$kmer_col
  } else {
    heatmap_pre_sum = 0
  }


  h <- (1 - heatmap_pre_sum) * 0.4 * plot_config$plot_height
  w <- 0.7 * plot_config$plot_width

  rect_h <- h / legend_num

  if (is.null(plot_config$legend_font_size)) {
    row_fz <- min(15, floor(rect_h))
    plot_config$legend_font_size <- row_fz
  } else {
    row_fz <- plot_config$legend_font_size
  }

  if (plot_config$sample_span == 0) {
    rect_w <- w / plot_config$sample_num
  } else {
    rect_w <- (w - (kmer_col - 1 + length(config_data$map_config$split_sample)) * plot_config$sample_span) / plot_config$sample_num
  }

  if (is.null(plot_config$sample_font_size)) {
    col_fz <- min(20, floor(rect_w))
    col_fz <- max(2, col_fz)
    plot_config$sample_font_size <- col_fz
  } else {
    col_fz <- plot_config$sample_font_size
  }


  lparam <- list(
    h = h,
    w = w,
    rect_h = rect_h,
    rect_w = rect_w,
    row_fz = row_fz,
    col_fz = col_fz
  )

  lparam <- list(legend = lparam)

  plot_config$sample_text_width <- rect_w

  plot_config_com <- c(plot_config, lparam)

  return(plot_config_com)


}


#
# checkPlotConfig
#
# heatmap 60%
# legmap 20%
# legmap explain + other 20%
#
#
checkPlotConfig <- function(plot_config, config_data) {
  group_order <- config_data$map_config$map_order
  group_baseline <- rep(0, (length(group_order) + 1))
  baseline <- 0

  if ("heatmap_1" %in% group_order) {
    if (config_data$map_config$heatmap_1$dendrogram == "row" | config_data$map_config$heatmap_1$dendrogram == "none" |
        !is.dendrogram(config_data$map_config$heatmap_1$Colv)) {
      group_baseline[1] <- 0
    } else {
      group_baseline[1] <- plot_config$heatmap_1$h * 0.2
    }
  }

  for (i in 1:length(group_order)) {
    if (grepl("^heatmap_", group_order[i])) {
      x <- plot_config[[which(names(plot_config) == group_order[i])]]
      group_baseline[i+1] <- group_baseline[i] + x$h*0.8 + plot_config$group_span
    } else if (grepl("^legend_", group_order[i])) {
      lid <- match(group_order[i], names(config_data$map_config))
      x <- plot_config[[which(names(plot_config) == "legend")]]
      group_baseline[i+1] <- group_baseline[i] + x$rect_h * length(config_data$map_config[[lid]]) + plot_config$group_span
    } else {
      stop("[ERROR] Invalid name in map_order, please check your data!")
    }
  }

  plot_config$group_baseline <- group_baseline

  return(plot_config)
}
