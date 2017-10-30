#'
#' dendrogram SVG elements generator
#'
dendSVG <- function(dend,
                    node_attr,
                    id = NULL,
                    plot_config,
                    tag = "col") {

  if (tag == "row") {
    zoom_x <- plot_config$heatmap_row_rect * (plot_config$heatmap_row_num - 1) / max(node_attr$node_x)
    zoom_y <- plot_config$plot_width * 0.2 / max(node_attr$node_y)
  } else {
    zoom_x <- plot_config$heatmap_col_rect * (plot_config$heatmap_col_num - 1) / max(node_attr$node_x)
    zoom_y <- plot_config$heatmap_height * 0.2 / max(node_attr$node_y)
  }

  node_attr$node_x <- node_attr$node_x * zoom_x
  node_attr$node_y <- node_attr$node_y * zoom_y

  dend_line <- lapply(2:length(node_attr$node_x), function(x) {
    pos1 <- c(node_attr$node_x[node_attr$node_father[x]], node_attr$node_y[node_attr$node_father[x]])
    pos2 <- c(node_attr$node_x[x], node_attr$node_y[x])
    line1 <- line.svg(x1 = pos1[1], y1 = (max(node_attr$node_y) - pos1[2]),
                      x2 = pos2[1], y2 = (max(node_attr$node_y) - pos1[2]),
                      stroke.width = plot_config$dend_stroke)
    line2 <- line.svg(x1 = pos2[1], y1 = (max(node_attr$node_y) - pos1[2]),
                      x2 = pos2[1], y2 = (max(node_attr$node_y) - pos2[2]),
                      stroke.width = plot_config$dend_stroke)
    return(c(line1, line2))
  })

  dend_line_ele <- paste(unlist(dend_line), collapse = "\n")
  dend_line_ele <- group.svg(id = id, group.content = dend_line_ele)

  # pack.svg(output.svg.name = "tests/test.svg", pack.content = dend_line_ele)
  return(dend_line_ele)
}

#'
#' calculate z data
#'
heatmapZdata <- function(heatmap_data) {
  nr <- dim(heatmap_data)[1]
  nc <- dim(heatmap_data)[2]

  row_mean_val <- rowMeans(heatmap_data)
  row_sd_val <- rowSds(heatmap_data)

  z_data <- lapply(1:nr, function(x) (heatmap_data[x, ] - row_mean_val[x])/row_sd_val[x])
  z_data <- t(matrix(unlist(z_data), nrow = nc, ncol = nr))

  colnames(z_data) <- colnames(heatmap_data)
  rownames(z_data) <- rownames(heatmap_data)

  return(z_data)
}

#'
#' heatmap SVG
#' heatmap plot content SVG element
#'
heatmapSVG <- function(z_data, config_data, plot_config, id) {
  # change value to color
  nr <- dim(z_data)[1]
  nc <- dim(z_data)[2]

  color_theme <- config_data$color_config[[match(config_data$map_config$heatmap_1$color_theme, names(config_data$color_config))]]
  color_list <- colorRampPalette(color_theme)(256)

  tag_val <- max(abs(min(z_data)), max(z_data))
  z_data <- round(z_data / tag_val * 128) + 128

  heatmap_svg_ele <- lapply(1:nr, function(m) {
    heatmap_svg_gene <- lapply(1:nc, function(n) {
      rect.svg(x = plot_config$heatmap_col_rect*(n-1),
               y = plot_config$heatmap_row_rect*(m-1),
               width = plot_config$heatmap_col_rect,
               height = plot_config$heatmap_row_rect,
               fill = color_list[z_data[m, n]],
               stroke = "none")
    })
    exp_svg <- unlist(heatmap_svg_gene)
    text_svg <- get.text.svg(x = plot_config$plot_width*0.71,
                             y = plot_config$heatmap_row_rect*m,
                             text.content = rownames(z_data)[m],
                             font.size = plot_config$heatmap_row_fz)
    return(paste(exp_svg, text_svg, sep = "\n"))
  })
  heatmap_svg_ele <- paste(unlist(heatmap_svg_ele), collapse = "\n")
  heatmap_svg_ele <- group.svg(id = id, group.content = heatmap_svg_ele)

  # pack.svg(output.svg.name = "tests/test.svg", pack.content = heatmap_svg_ele)
  return(heatmap_svg_ele)
}

#'
#' heatmap grandient plot
#'
heatmapGradSVG <- function(z_data, config_data, plot_config, id)  {
  # change value to color
  nr <- dim(z_data)[1]
  nc <- dim(z_data)[2]

  tag_val <- max(abs(min(z_data)), max(z_data))

  color_theme <- config_data$color_config[[match(config_data$map_config$heatmap_1$color_theme, names(config_data$color_config))]]
  color_list <- colorRampPalette(color_theme)(256)

  grad_h <- plot_config$heatmap_height * 0.1
  grad_w <- plot_config$plot_width * 0.12 / 256
  grad_mat <- lapply(1:256, function(x) rect.svg(x = (x-1)*grad_w, y = 0, width = grad_w, height = grad_h,
                                                 fill = color_list[x], stroke = "none"))
  grad_mat <- paste(unlist(grad_mat), collapse = "\n")
  grad_box <- rect.svg(x = 0, y = 0, width = plot_config$plot_width * 0.12, height = plot_config$heatmap_height * 0.1,
                       fill = "none")

  tag_val_m <- (tag_val) %% 1
  tag_val_n <- floor(tag_val)*2+1
  grad_axis <- lapply(1:tag_val_n, function(x) {
    num_info <- get.text.svg(x = (tag_val_m+x-1)*plot_config$plot_width * 0.12 / (tag_val*2),
                             y = plot_config$heatmap_height * 0.1 + plot_config$heatmap_row_fz + 3,
                             text.content = (x-1)-floor(tag_val), font.size = plot_config$heatmap_row_fz, text.anchor = "middle")
  })
  grad_axis <- paste(unlist(grad_axis), collapse = "\n")
  grad_z_text <- get.text.svg(x = plot_config$plot_width * 0.06, y = plot_config$heatmap_height * 0.1 + plot_config$heatmap_row_fz*2 + 10,
                              text.content = "Z-score", font.size = plot_config$heatmap_row_fz + 4, text.anchor = "middle")

  grad_svg <- paste(grad_mat, grad_box, grad_axis, grad_z_text, sep = "\n")
  grad_svg <- group.svg(group.content = grad_svg, id = id)
  return(grad_svg)

}

#'
#' Heatmap sample SVG element
#'
heatmapSampleSVG <- function(heatmap_data, plot_config, id) {
  sample_name <- colnames(heatmap_data)

  sample_svg <- lapply(1:length(sample_name), function(x) {
    get.text.svg(x = (x-0.5)*plot_config$heatmap_col_rect, y = 4, text.content = sample_name[x],
                 font.size = plot_config$heatmap_col_fz, rotate = 90)
  })
  sample_svg <- group.svg( group.content = paste(unlist(sample_svg), sep = "\n"), id = id)

  return(sample_svg)
}











