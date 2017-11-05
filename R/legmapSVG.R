#'
#' Conver legend sample information into svg elements
#'
sampleSVG <- function(plot_config, gap_info, id) {
  col_label <- which(!is.na(gap_info$node_label))
  col_gap <- gap_info$node_kmer_gap[col_label] + gap_info$node_name_gap[col_label]

  sample_name <- plot_config$sample_order

  sample_svg <- lapply(1:length(sample_name), function(x) {
    get.text.svg(x = (x-0.5)*plot_config$sample_text_width + col_gap[x],
                 y = 0, text.content = sample_name[x],
                 font.size = plot_config$sample_font_size,
                 rotate = 90)
  })
  sample_svg <- group.svg( group.content = paste(unlist(sample_svg), sep = "\n"), id = id)

  return(sample_svg)
}


#'
#'
#'
legendGroupSVG <- function(config_data, plot_config, legend_data, legend_sub_info, legend_sub_plot, gap_info, id) {
  group_name <- id

  group_info <- config_data$map_config[[which(names(config_data$map_config) == group_name)]]
  group_type <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[1]]))
  group_color_theme <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[2]]))
  group_col_num <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[3]]))

  col_label <- which(!is.na(gap_info$node_label))
  col_gap <- gap_info$node_kmer_gap[col_label] + gap_info$node_name_gap[col_label]


  # rect information
  baseline <- 0
  rect <- rep("", length(group_type))
  leg_text <- rep("", length(group_type))
  for (n in 1:length(group_type)) {
    type <- group_type[n]
    color_theme <- group_color_theme[n]
    col_num <- group_col_num[n]
    rect[n] <- getLegRowRectSVG(type, color_theme, col_num, legend_data, legend_sub_plot, col_gap, config_data, plot_config, n-1)
    leg_text[n] <- getRowLegendText(type, color_theme, col_num, legend_data, legend_sub_plot, config_data, plot_config, n-1)
  }

  if (plot_config$frame) {
    group_gap <- data.frame(table(col_gap))

    if (dim(group_gap)[1] == 1) {
      group_outline <- rect.svg(x = 0, y = 0,
                                width = legend_sub_plot$rect_w * length(row.names(legend_data)),
                                height = legend_sub_plot$rect_h * length(group_col_num),
                                fill = "none", stroke.width = plot_config$frame_stroke_width)
    } else {
      group_outline <- lapply(1:dim(group_gap)[1], function(x) {
        if (x == 1) {
          tt = 0
        } else {
          tt = sum(group_gap$Freq[1:(x-1)])
        }
        rect.svg(x = tt*legend_sub_plot$rect_w + as.numeric(as.character(group_gap$col_gap[x])),
                 y = 0,
                 width = legend_sub_plot$rect_w * group_gap$Freq[x],
                 height = legend_sub_plot$rect_h * length(group_col_num),
                 fill = "none", stroke.width = plot_config$frame_stroke_width)
      })
    }

    split_sample_name <- config_data$map_config$split_sample
    sample_name <- plot_config$sample_order

    if (!is.null(split_sample_name)) {
      split_sample_line <- lapply(1:length(split_sample_name), function(x) {
        sp_idx <- which(sample_name == split_sample_name[x])
        sp_line <- line.svg( x1 = legend_sub_plot$rect_w * sp_idx + col_gap[sp_idx],
                             y1 = 0,
                             x2 = legend_sub_plot$rect_w * sp_idx + col_gap[sp_idx],
                             y2 = legend_sub_plot$rect_h * length(group_col_num),
                             stroke.width = plot_config$frame_stroke_width)
      })
    } else {
      split_sample_line <- ""
    }
  } else {
    group_outline <- ""
    split_sample_line <- ""
  }

  group_rect_svg <- group.svg(id = paste0(group_name, "_mat"), group.content = paste(rect, collapse = "\n"))
  group_legend_svg <- group.svg(id = paste0(group_name, "_leg_text"), group.content = paste(leg_text, collapse = "\n"))
  group_outline_svg <- group.svg(id = paste0(group_name, "_outline"), group.content = paste(unlist(group_outline), unlist(split_sample_line), collapse = "\n"))

  group_svg <- paste(group_rect_svg, group_outline_svg, group_legend_svg, sep = "\n")

  return(group_svg)
}


#'
#' get every row rect element
#'
#'
getLegRowRectSVG <- function(type, color_theme, col_num, legend_data, legend_sub_plot, col_gap, config_data, plot_config, baseline) {
  colname_info <- type
  #message(colname_info)
  content <- as.matrix(legend_data[, col_num])
  rect <- rep("", length(col_num))

  colname_text <-  paste(get.text.svg( x=-legend_sub_plot$row_fz*0.5,
                                       y=(0.5+baseline)*legend_sub_plot$rect_h+legend_sub_plot$row_fz*0.5,
                                       text.anchor = "end",
                                       text.content = colname_info,
                                       font.size = legend_sub_plot$row_fz),
                         line.svg( x1=0,
                                   y1=(0.5+baseline)*legend_sub_plot$rect_h,
                                   x2=-legend_sub_plot$row_fz*0.3,
                                   y2=(0.5+baseline)*legend_sub_plot$rect_h,
                                   stroke.width = plot_config$stroke_width),
                         sep = "\n")

  if (grepl("^bg_", color_theme)) {
    # bg_col theme
    rect <- lapply(1:length(content), function(x) {
      rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                y = baseline*legend_sub_plot$rect_h,
                width = legend_sub_plot$rect_w,
                height = legend_sub_plot$rect_h,
                fill = config_data$color_config$bg_col,
                stroke = config_data$color_config$white_col,
                stroke.width = plot_config$stroke_width)
    })

  } else if (grepl("^tag_", color_theme)) {
    # tag_col theme
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
      } else {
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = color_this[1],
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
      }
    })

  } else if (color_theme == "mutation_col") {
    # mutation_col theme
    mut_colors <- config_data$color_config$mutation_col
    rect <- lapply(1:length(content), function(x) {
    #message(x)
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
     } else {
        mut_type <- data.frame(table(strsplit(tolower(as.character(content[x])), split="/")))
        if (length(mut_type$Var1) == 1) {
          mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_type$Var1))])
          if (length(mut_color) == 0) {
            mut_color <- config_data$color_config$white_col
          }
          rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                    y = baseline*legend_sub_plot$rect_h,
                    width = legend_sub_plot$rect_w,
                    height = legend_sub_plot$rect_h,
                    fill = mut_color,
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$stroke_width)

        } else {
          sub_rect <- rep("", length(mut_type$Var1) + 1)
          sum_freq <- sum(mut_type$Freq)
          for (m in (1:length(mut_type$Var1))) {
            mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_type$Var1[m]))])
            sub_rect[m] <- rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                                     y = (baseline+sum(mut_type$Freq[1:m-1])/sum_freq)*legend_sub_plot$rect_h,
                                     width = legend_sub_plot$rect_w,
                                     height = legend_sub_plot$rect_h*(sum(mut_type$Freq[m])/sum_freq),
                                     fill = mut_color,
                                     stroke = "none")
          }
          sub_rect[length(mut_type$Var1)+1] <- rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                                                         y = baseline*legend_sub_plot$rect_h,
                                                         width = legend_sub_plot$rect_w,
                                                         height = legend_sub_plot$rect_h,
                                                         fill = "none",
                                                         stroke = config_data$color_config$white_col,
                                                         stroke.width = plot_config$stroke_width)
          sub_rect_single <- paste(sub_rect, collapse = "\n")
          return(sub_rect_single)
        }
      }
    })

  } else if (grepl("^binary_", color_theme)) {
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- unique(as.vector(content))
    content_element <- sort(content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))])
    if (length(content_element) != 2) {
      stop("don't match binaly element color theme")
    } else {
      rect <- lapply(1:length(content), function(x) {
        #message(content[x,k])
        if (content[x] == 0 | is.na(content[x])) {
          rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                    y = baseline*legend_sub_plot$rect_h,
                    width = legend_sub_plot$rect_w,
                    height = legend_sub_plot$rect_h,
                    fill = config_data$color_config$bg_col,
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$stroke_width)
        } else if (content[x] == content_element[1]) {
          rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                    y = baseline*legend_sub_plot$rect_h,
                    width = legend_sub_plot$rect_w,
                    height = legend_sub_plot$rect_h,
                    fill = color_this[1],
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$stroke_width)
        } else if (content[x] == content_element[2]) {
          rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                    y = baseline*legend_sub_plot$rect_h,
                    width = legend_sub_plot$rect_w,
                    height = legend_sub_plot$rect_h,
                    fill = color_this[2],
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$stroke_width)
        } else {
          rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                    y = baseline*legend_sub_plot$rect_h,
                    width = legend_sub_plot$rect_w,
                    height = legend_sub_plot$rect_h,
                    fill = config_data$color_config$white_col,
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$stroke_width)
        }
      })
    }
  } else if (grepl("^pool_", color_theme)) {
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- sort(unique(as.vector(content)))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
      } else {
        tt <- which(content_element == content[x])
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = color_this[tt],
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
      }
    })
  } else {
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
      } else {
        color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]][1]
        rect.svg( x = (x-1)*legend_sub_plot$rect_w + col_gap[x],
                  y = baseline*legend_sub_plot$rect_h,
                  width = legend_sub_plot$rect_w,
                  height = legend_sub_plot$rect_h,
                  fill = color_this,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$stroke_width)
      }
    })
  }
  rect_total <- paste(paste(rect, collapse = "\n"),
                      paste(colname_text, collapse = "\n"),
                      sep = "\n")
  return(rect_total)
}

#'
#' legend appended data
#'
getRowLegendText <- function(type, color_theme, col_num, legend_data, legend_sub_plot, config_data, plot_config, baseline) {
  colname_info <- type
  #message(colname_info)
  content <- as.vector(legend_data[, col_num])

  legend_control <- min(12, legend_sub_plot$row_fz)

  title <- get.text.svg(x = 0, y = baseline * legend_sub_plot$rect_h + legend_control,
                        text.content = colname_info, font.size = legend_control + 2,
                        font.weight = "bold")
  title_app <- legend_sub_plot$w * 0.12
  rect_app <- legend_sub_plot$w * 0.1
  rect_gap <- 3

  if (grepl("^bg_", color_theme)) {
    basic_info <- paste(rect.svg(x = 0 + title_app, y = baseline * legend_sub_plot$rect_h,
                                 width = legend_control, height = legend_control,
                                 fill = config_data$color_config$bg_col, stroke.width = 0),
                        get.text.svg( x = 0 + title_app + legend_control + rect_gap,
                                      y = baseline * legend_sub_plot$rect_h + legend_control,
                                      text.content = content[1,1], font.size = legend_control),
                        sep = "\n")
  } else if (grepl("^tag_", color_theme)) {
    # tag_col theme
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- sort(unique(as.vector(content)))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    basic_info <- paste(rect.svg(x = 0 + title_app, y = baseline * legend_sub_plot$rect_h,
                                 width = legend_control, height = legend_control,
                                 fill = color_this[1], stroke.width = 0),
                        get.text.svg( x = 0 + title_app + legend_control + rect_gap,
                                      y = baseline * legend_sub_plot$rect_h + legend_control,
                                      text.content = content_element[1], font.size = legend_control),
                        sep = "\n")
  } else if (color_theme == "mutation_col") {
    # mutation_col theme
    mut_colors <- config_data$color_config$mutation_col

    content_element <- strsplit(tolower(as.character(content)), split="/")
    content_element <- sort(unique(as.vector(unlist(content_element))))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]

    basic_info <- lapply(1:length(content_element), function(x) {
      mut_name <- content_element[x]
      mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_name))])
      paste(rect.svg(x = 0 + title_app + (x-1)*rect_app,
                     y = baseline * legend_sub_plot$rect_h,
                     width = legend_control, height = legend_control,
                     fill = mut_color, stroke.width = 0),
            get.text.svg( x = 0 + title_app + (x-1)*rect_app + legend_control + rect_gap,
                          y = baseline * legend_sub_plot$rect_h + legend_control,
                          text.content = mut_name, font.size = legend_control),
            sep = "\n")
    })
    basic_info <- paste(unlist(basic_info), collapse = "\n")
  } else if (grepl("^binary_", color_theme)) {
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- unique(as.vector(content))
    content_element <- sort(content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))])

    basic_info <- lapply(1:2, function(x) {
      paste(rect.svg(x = 0 + title_app + (x-1)*rect_app,
                     y = baseline * legend_sub_plot$rect_h,
                     width = legend_control, height = legend_control,
                     fill = color_this[x], stroke.width = 0),
            get.text.svg( x = 0 + title_app + (x-1)*rect_app + legend_control + rect_gap,
                          y = baseline * legend_sub_plot$rect_h + legend_control,
                          text.content = content_element[x], font.size = legend_control),
            sep = "\n")
    })
    basic_info <- paste(unlist(basic_info), collapse = "\n")
  } else if (grepl("^pool_", color_theme)) {
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- sort(unique(as.vector(content)))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]

    if (length(content_element) <= 8) {
      basic_info <- lapply(1:length(content_element), function(x) {
        paste(rect.svg(x = 0 + title_app + (x-1)*rect_app,
                       y = baseline * legend_sub_plot$rect_h,
                       width = legend_control, height = legend_control,
                       fill = color_this[x], stroke.width = 0),
              get.text.svg( x = 0 + title_app + (x-1)*rect_app + legend_control + rect_gap,
                            y = baseline * legend_sub_plot$rect_h + legend_control,
                            text.content = content_element[x], font.size = legend_control),
              sep = "\n")
      })
    } else {
      basic_info_1 <- lapply(1:8, function(x) {
        paste(rect.svg(x = 0 + title_app + (x-1)*rect_app,
                       y = baseline * legend_sub_plot$rect_h,
                       width = legend_control, height = legend_control,
                       fill = color_this[x], stroke.width = 0),
              get.text.svg( x = 0 + title_app + (x-1)*rect_app + legend_control + rect_gap,
                            y = baseline * legend_sub_plot$rect_h + legend_control,
                            text.content = content_element[x], font.size = legend_control),
              sep = "\n")
      })
      basic_info_2 <- lapply(8:length(content_element), function(x) {
        paste(rect.svg(x = 0 + title_app + (x-1)*rect_app,
                       y = baseline * legend_sub_plot$rect_h + legend_control + 2,
                       width = legend_control, height = legend_control,
                       fill = color_this[x], stroke.width = 0),
              get.text.svg( x = 0 + title_app + (x-1)*rect_app + legend_control + rect_gap,
                            y = baseline * legend_sub_plot$rect_h + legend_control + legend_control + 2,
                            text.content = content_element[x], font.size = legend_control),
              sep = "\n")
      })
      basic_info <- c(unlist(basic_info_1), unlist(basic_info_2))
    }
    basic_info <- paste(unlist(basic_info), collapse = "\n")
  } else {
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- sort(unique(as.vector(content)))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    basic_info <- paste(rect.svg(x = 0 + title_app, y = baseline * legend_sub_plot$rect_h,
                                 width = legend_control, height = legend_control,
                                 fill = color_this[1], stroke.width = 0),
                        get.text.svg( x = 0 + title_app + legend_control + rect_gap,
                                      y = baseline * legend_sub_plot$rect_h + legend_control,
                                      text.content = content_element[1], font.size = legend_control),
                        sep = "\n")
  }

  basic_info <- paste(title, basic_info, sep = "\n")
  return(basic_info)
}









