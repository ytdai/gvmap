#
# dendrogram SVG elements generator
#
dendSVG <- function(dend_attr,
                    id = NULL,
                    plot_config) {

  node_color <- dend_attr$node_color
  node_color[is.na(node_color)] <- "#000000"
  dend_attr$node_color <- node_color

  dend_line <- lapply(2:length(dend_attr$node_x), function(x) {
    pos1 <- c(dend_attr$node_x[dend_attr$node_father[x]], dend_attr$node_y[dend_attr$node_father[x]])
    pos2 <- c(dend_attr$node_x[x], dend_attr$node_y[x])
    line1 <- line.svg(x1 = pos1[1], y1 = max(dend_attr$node_y) - pos1[2],
                      x2 = pos2[1], y2 = max(dend_attr$node_y) - pos1[2],
                      stroke = dend_attr$node_color[x])
    line2 <- line.svg(x1 = pos2[1], y1 = max(dend_attr$node_y) - pos1[2],
                      x2 = pos2[1], y2 = max(dend_attr$node_y) - pos2[2],
                      stroke = dend_attr$node_color[x])
    return(c(line1, line2))
  })

  dend_line_ele <- paste(unlist(dend_line), collapse = "\n")
  dend_line_ele <- group.svg(id = id, group.content = dend_line_ele,
                             stroke.width = plot_config$dend_stroke_width,
                             font.family = plot_config$font_family)

  # pack.svg(output.svg.name = "tests/test.svg", pack.content = dend_line_ele)
  return(dend_line_ele)
}

#
# calculate row z data
#
heatmapRowZdata <- function(heatmap_data) {
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

#
# calculate col z data
#
heatmapColZdata <- function(heatmap_data) {
  nr <- dim(heatmap_data)[1]
  nc <- dim(heatmap_data)[2]

  col_mean_val <- colMeans(heatmap_data)
  col_sd_val <- colSds(heatmap_data)

  z_data <- lapply(1:nr, function(x) (heatmap_data[, x] - col_mean_val[x])/col_sd_val[x])
  z_data <- t(matrix(unlist(z_data), nrow = nc, ncol = nr))

  colnames(z_data) <- colnames(heatmap_data)
  rownames(z_data) <- rownames(heatmap_data)

  return(z_data)
}

#
# heatmap SVG
# heatmap plot content SVG element
#
heatmapSVG <- function(heatmap_sub_data, heatmap_sub_info, heatmap_sub_plot,
                       config_data, id) {
  # change value to color
  nr <- dim(heatmap_sub_data)[1]
  nc <- dim(heatmap_sub_data)[2]

  color_theme <- config_data$color_config[[match(heatmap_sub_info$color_theme, names(config_data$color_config))]]
  color_list <- colorRampPalette(color_theme)(256)

  tag_val <- max(abs(min(heatmap_sub_data)), max(heatmap_sub_data))
  heatmap_sub_data_col <- heatmap_sub_data / tag_val * 128 + 128

  # gap for name
  row_label <- which(!is.na(heatmap_sub_info$row_attr$node_label))
  row_gap <- heatmap_sub_info$row_attr$node_kmer_gap[row_label] + heatmap_sub_info$row_attr$node_name_gap[row_label]

  gap_info <- config_data$map_config$heatmap_kmer_gap
  col_label <- which(!is.na(gap_info$node_label))
  col_gap <- gap_info$node_kmer_gap[col_label] + gap_info$node_name_gap[col_label]

  if (length(col_gap) == 0) {
    col_gap <- rep(0, nc)
  }
  if (length(row_gap) == 0) {
    row_gap <- rep(0, nr)
  }

  heatmap_svg_ele <- lapply(1:nr, function(m) {
    heatmap_svg_gene <- lapply(1:nc, function(n) {
      rect.svg(x = heatmap_sub_plot$rect_w*(n-1) + col_gap[n],
               y = heatmap_sub_plot$rect_h*(m-1) + row_gap[m],
               width = heatmap_sub_plot$rect_w,
               height = heatmap_sub_plot$rect_h,
               fill = color_list[heatmap_sub_data_col[m, n]])
    })
    exp_svg <- paste(unlist(heatmap_svg_gene), collapse = "\n")
    text_svg <- get.text.svg(x = heatmap_sub_plot$w*0.71,
                             y = heatmap_sub_plot$rect_h*m + row_gap[m],
                             text.content = rownames(heatmap_sub_data)[m])
    return(paste(exp_svg, text_svg, sep = "\n"))
  })
  heatmap_svg_ele <- paste(unlist(heatmap_svg_ele), collapse = "\n")
  heatmap_svg_ele <- group.svg(id = id,
                               group.content = heatmap_svg_ele,
                               font.size = heatmap_sub_plot$row_fz)

  # pack.svg(output.svg.name = "tests/test.svg", pack.content = heatmap_svg_ele)
  return(heatmap_svg_ele)
}

#
# heatmap grandient plot
#
heatmapGradSVG <- function(heatmap_sub_data, heatmap_sub_info, heatmap_sub_plot,
                           config_data, id)   {
  # change value to color
  nr <- dim(heatmap_sub_data)[1]
  nc <- dim(heatmap_sub_data)[2]

  color_theme <- config_data$color_config[[match(heatmap_sub_info$color_theme, names(config_data$color_config))]]
  color_list <- colorRampPalette(color_theme)(256)

  tag_val <- max(abs(min(heatmap_sub_data)), max(heatmap_sub_data))

  grad_h <- min(20, heatmap_sub_plot$h * 0.03)
  grad_w <- heatmap_sub_plot$w * 0.10 / 256
  grad_mat <- lapply(1:256, function(x) rect.svg(x = (x-1)*grad_w, y = 0, width = grad_w, height = grad_h,
                                                 fill = color_list[x]))
  grad_mat <- paste(unlist(grad_mat), collapse = "\n")
  grad_box <- rect.svg(x = 0, y = 0, width = heatmap_sub_plot$w * 0.10, height = grad_h,
                       fill = "none", stroke = "#000000")

  grad_axis_num <- min(10, heatmap_sub_plot$row_fz * 3)
  grad_axis_num <- max(grad_axis_num, 4)
  grad_axis <- lim.axis.svg(x = c(-1*tag_val, tag_val), line.length = heatmap_sub_plot$w * 0.10,
                            axis.font.size = grad_axis_num,
                            span = grad_axis_num + 2,
                            id = "axis")
  grad_axis <- group.svg(id = "group_axis", group.content = grad_axis,
                         transform.sheet = paste0("translate(", 0, ",", grad_h ,")"))


  #grad_text <- get.text.svg( x = heatmap_sub_plot$w * 0.05,
  #                           y = grad_h + heatmap_sub_plot$row_fz * 3,
  #                           text.content = "value", text.anchor = "middle")
  grad_svg <- paste(grad_mat, grad_box, grad_axis, sep = "\n")
  grad_svg <- group.svg(group.content = grad_svg, id = id)
  return(grad_svg)

}

#
# Heatmap sample SVG element
#
heatmapSampleSVG <- function(heatmap_data, plot_config, id) {
  sample_name <- colnames(heatmap_data)

  sample_svg <- lapply(1:length(sample_name), function(x) {
    get.text.svg(x = (x-0.5)*plot_config$heatmap_col_rect, y = 4, text.content = sample_name[x],
                 font.size = plot_config$heatmap_col_fz, rotate = 90)
  })
  sample_svg <- group.svg( group.content = paste(unlist(sample_svg), sep = "\n"), id = id, font.family = plot_config$font_family)

  return(sample_svg)
}











