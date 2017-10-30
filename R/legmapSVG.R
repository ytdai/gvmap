#'
#'
#'
legendSampleSVG <- function(legend_data, plot_config, id) {
  sample_name <- rownames(legend_data)

  sample_svg <- lapply(1:length(sample_name), function(x) {
    get.text.svg(x = (x-0.5)*plot_config$legmap_col_rect, y = 4, text.content = sample_name[x],
                 font.size = plot_config$legmap_col_fz, rotate = 90)
  })
  sample_svg <- group.svg( group.content = paste(unlist(sample_svg), sep = "\n"), id = id)

  return(sample_svg)
}


#'
#'
#'
legendGroupSVG <- function(legend_data, config_data, plot_config, i) {
  group_name <- paste("legmap_", i, sep = "")
  group_name_info <- paste("legmap_", i, "_info", sep = "")

  group_info <- config_data$map_config[[which(names(config_data$map_config) == group_name)]]
  group_type <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[1]]))
  group_color_theme <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[2]]))
  group_col_num <- unlist(lapply(1:length(group_info), function(x) group_info[[x]][[3]]))

  # rect information
  baseline <- 0
  rect <- rep("", length(group_type))
  leg_text <- rep("", length(group_type))
  for (n in 1:length(group_type)) {
    type <- group_type[n]
    color_theme <- group_color_theme[n]
    col_num <- group_col_num[n]
    rect[n] <- getLegRowRectSVG(type, color_theme, col_num, legend_data, config_data, plot_config, n-1)
    #leg_text[n] <- getRowLegendText(type, color_theme, col_num, data_info, color_config, rect_config, n-1)
  }

  if (plot_config$frame) {
    group_outline_g <- paste("legmap_", i, "_outline", sep = "")
    gourp_outline <- rect.svg(x = 0, y = 0,
                              width = plot_config$legmap_col_rect * length(row.names(legend_data)),
                              height = plot_config$legmap_row_rect * length(group_col_num),
                              fill = "none", stroke.width = plot_config$dend_stroke)
    if (!is.null(config_data$map_config$split_sample)) {
      sample_list <- row.names(legend_data)
      sample_line <- lapply(1:length(config_data$map_config$split_sample), function(x) {
        sample_index <- which(sample_list == config_data$map_config$split_sample[x])
        return(line.svg( x1 = plot_config$legmap_col_rect*sample_index, y1 = 0,
                         x2 = plot_config$legmap_col_rect*sample_index, y2 = plot_config$legmap_row_rect*length(group_col_num),
                         stroke.width = plot_config$dend_stroke))
      })
    } else {
      sample_line <- ""
    }
    group_rect_svg <- group.svg(id = group_name_info, group.content = paste(rect, collapse = "\n"))
    group_outline_svg <- group.svg(id = group_outline_g, group.content = c(gourp_outline, paste(sample_line, collapse = "\n")))

    group_svg <- paste(group_rect_svg, group_outline_svg,
                       sep = "\n")
  } else {
    group_rect_svg <- group.svg(id = group_name_info, group.content = paste(rect, collapse = "\n"))

    group_svg <- paste(group_rect_svg,
                       sep = "\n")
  }

  return(group_svg)
}


#'
#' get every row rect element
#'
#'
getLegRowRectSVG <- function(type, color_theme, col_num, legend_data, config_data, plot_config, baseline) {
  colname_info <- type
  #message(colname_info)
  content <- as.matrix(legend_data[, col_num])
  rect <- rep("", length(col_num))

  colname_text <-  paste(get.text.svg( x=-8, y=(0.5+baseline)*plot_config$legmap_row_rect+plot_config$legend_font_size*0.5, text.anchor = "end",
                                       text.content = colname_info, font.size = plot_config$legend_font_size),
                         line.svg( x1=0, y1=(0.5+baseline)*plot_config$legmap_row_rect,
                                   x2=-6, y2=(0.5+baseline)*plot_config$legmap_row_rect,
                                   stroke.width = plot_config$dend_stroke),
                         sep = "\n")

  if (grepl("^bg_", color_theme)) {
    # bg_col theme
    rect <- lapply(1:length(content), function(x) {
      rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                y = baseline*plot_config$legmap_row_rect,
                width = plot_config$legmap_col_rect,
                height = plot_config$legmap_row_rect,
                fill = config_data$color_config$bg_col,
                stroke = config_data$color_config$white_col,
                stroke.width = plot_config$dend_stroke)
    })

  } else if (grepl("^tag_", color_theme)) {
    # tag_col theme
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
      } else {
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = color_this[1],
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
      }
    })

  } else if (color_theme == "mutation_col") {
    # mutation_col theme
    mut_colors <- config_data$color_config$mutation_col
    rect <- lapply(1:length(content), function(x) {
    #message(x)
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
     } else {
        mut_type <- data.frame(table(strsplit(tolower(as.character(content[x])), split="/")))
        if (length(mut_type$Var1) == 1) {
          mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_type$Var1))])
          if (length(mut_color) == 0) {
            mut_color <- config_data$color_config$white_col
          }
          rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                    y = baseline*plot_config$legmap_row_rect,
                    width = plot_config$legmap_col_rect,
                    height = plot_config$legmap_row_rect,
                    fill = mut_color,
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$dend_stroke)

        } else {
          sub_rect <- rep("", length(mut_type$Var1) + 1)
          sum_freq <- sum(mut_type$Freq)
          for (m in (1:length(mut_type$Var1))) {
            mut_color <- as.character(mut_colors[which(names(mut_colors) == as.character(mut_type$Var1[m]))])
            sub_rect[m] <- rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                                     y = (baseline+sum(mut_type$Freq[1:m-1])/sum_freq)*plot_config$legmap_row_rect,
                                     width = plot_config$legmap_col_rect,
                                     height = plot_config$legmap_row_rect*(sum(mut_type$Freq[m])/sum_freq),
                                     fill = mut_color,
                                     stroke = "none")
          }
          sub_rect[length(mut_type$Var1)+1] <- rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                                                         y = baseline*plot_config$legmap_row_rect,
                                                         width = plot_config$legmap_col_rect,
                                                         height = plot_config$legmap_row_rect,
                                                         fill = "none",
                                                         stroke = config_data$color_config$white_col,
                                                         stroke.width = plot_config$dend_stroke)
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
          rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                    y = baseline*plot_config$legmap_row_rect,
                    width = plot_config$legmap_col_rect,
                    height = plot_config$legmap_row_rect,
                    fill = config_data$color_config$bg_col,
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$dend_stroke)
        } else if (content[x] == content_element[1]) {
          rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                    y = baseline*plot_config$legmap_row_rect,
                    width = plot_config$legmap_col_rect,
                    height = plot_config$legmap_row_rect,
                    fill = color_this[1],
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$dend_stroke)
        } else if (content[x] == content_element[2]) {
          rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                    y = baseline*plot_config$legmap_row_rect,
                    width = plot_config$legmap_col_rect,
                    height = plot_config$legmap_row_rect,
                    fill = color_this[2],
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$dend_stroke)
        } else {
          rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                    y = baseline*plot_config$legmap_row_rect,
                    width = plot_config$legmap_col_rect,
                    height = plot_config$legmap_row_rect,
                    fill = config_data$color_config$white_col,
                    stroke = config_data$color_config$white_col,
                    stroke.width = plot_config$dend_stroke)
        }
      })
    }
  } else if (grepl("^pool_", color_theme)) {
    color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]]
    content_element <- sort(unique(as.vector(content)))
    content_element <- content_element[which(content_element != 0 & content_element != "0" & !is.na(content_element))]
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
      } else {
        tt <- which(content_element == content[x])
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = color_this[tt],
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
      }
    })
  } else {
    rect <- lapply(1:length(content), function(x) {
      if (content[x] == 0 | is.na(content[x])) {
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = config_data$color_config$bg_col,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
      } else {
        color_this <- config_data$color_config[[match(color_theme, names(config_data$color_config))]][1]
        rect.svg( x = (x-1)*plot_config$legmap_col_rect,
                  y = baseline*plot_config$legmap_row_rect,
                  width = plot_config$legmap_col_rect,
                  height = plot_config$legmap_row_rect,
                  fill = color_this,
                  stroke = config_data$color_config$white_col,
                  stroke.width = plot_config$dend_stroke)
      }
    })
  }
  rect_total <- paste(paste(rect, collapse = "\n"),
                      paste(colname_text, collapse = "\n"),
                      sep = "\n")
  return(rect_total)
}













