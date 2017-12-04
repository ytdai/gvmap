#'
#' gvmap package
#'
#' @param legend_data data frame. The basic legend data of the plot. It will contain all the
#' sample information. Attention: the row name of the legend data must be the sample name, or
#' this funciton will NOT run.
#' @param heatmap_data list, the heatmap data of the sample. The list name of heatmap data must
#' like this form: heatmap_1, heatmap_2 ... And each heatmap subset must be a martrix, with sample
#' name being the col name and gene set being the row name.
#' @param config_file character, the path of your config file. see \link[gvmap]{gvmap-config}
#' @param output_svg_name character, the output svg file name
#' @param convert_pdf bool, whether to convert svg file into pdf. Attention: If your svg file is
#' over 10M, it's better not convert it into pdf. This bug is still being repaired.
#' @param output_group_info bool, output group information of each heatmap and legend
#' @param convert_jpg bool, whether to convert svg file into png.
#' @param plot_width number, the canvas width of your figure. Default: 1200
#' @param plot_height number, the canvas height of your figure. Default: 1600
#' @param stroke_width number, the stroke width of your legend figure. Default: 1
#' @param dend_stroke_width number, the dendrogram stroke width. Default: 2
#' @param group_span number, the distance between two groups, such as heatmap_1 and legend_1. Default: 10
#' @param sample_span number, the distance between sample. Default: 0
#' @param heatmap_row_span number, the distance between two specific colnames on heatmap. Default: 0
#' @param frame bool, if TRUE, the legend data has black outline. Otherwise there is no outline. Default: TRUE
#' @param frame_stroke_width number, the stroke width of the legend frame
#' @param sample_font_size number, the sample font size
#' @param legend_font_size number, the legend font size
#' @param font_family character, the font family of the plot. Default: "Arial"
#'
#' @seealso
#' \link{heatmap}, \link[gplots]{heatmap.2}
#'
#' @export
#'
#' @examples
#' browseVignettes("gvmap")
#'
gvmap <- function(legend_data,
                  heatmap_data,
                  config_file,

                  # output svg name
                  output_svg_name,
                  convert_pdf = FALSE,
                  convert_jpg = FALSE,

                  # output group info
                  output_group_info = FALSE,

                  # plot for canvas data
                  plot_width = 1200,
                  plot_height = 1600,
                  stroke_width = 1,
                  dend_stroke_width = 2,
                  group_span = 10,
                  sample_span = 0,
                  heatmap_row_span = 0,
                  frame = TRUE,
                  frame_stroke_width = 2,
                  sample_font_size = NULL,
                  legend_font_size = NULL,
                  font_family = "Arial") {

  ## =======================
  ## Input data
  ## =======================

  # read config file and generate config parmeters
  if (missing(config_file)) {
    stop("[ERROR] Config file is required!")
  }
  config_data <- gvmapConfig(config_file)
  message("[INFO] Read config data completed")

  # read legend data
  if (missing(legend_data)) {
    message("[WARNING] Legend data is missing")
    if (config_data$map_config$legend_num > 0) {
      message("[WARNING] The legend_num (legend number) in config file is over zero, please check your data!")
    }
    legend_data_plot <- FALSE
  } else {
    if (config_data$map_config$legend_num == 0) {
      message("[INFO] The legend_num (legend number) in config file is zero, the legend will not plot.")
      legend_data_plot <- FALSE
    } else {
      legend_data_plot <- TRUE
    }
    legend_data <- readLegendFile(legend_data)
  }
  message("[INFO] Read legend data completed")

  # read heatmap data
  if (missing(heatmap_data)) {
    message("[WARNING] heatmap data is missing.")
    if (config_data$map_config$heatmap_num > 0) {
      message("[WARNING] The heatmap_num (heatmap number) in config file is over zero, please check your data!")
    }
    heatmap_data_plot <- FALSE
  } else {
    if (config_data$map_config$heatmap_num == 0) {
      message("[INFO] The heatmap_num (heatmap number) in config file is zero, the heatmap will not plot.")
      heatmap_data_plot <- FALSE
    } else {
      heatmap_data_plot <- TRUE
    }
    heatmap_data <- readHeatmapFile(heatmap_data)
  }
  message("[INFO] Read heatmap data completed")

  if (!heatmap_data_plot & !legend_data_plot) {
    stop("[ERROR] None heatmap or legend, please check your data!")
  }

  if (missing(output_svg_name)) {
    stop("[ERROR] The output svg name is required!")
  }

  ## =======================
  ## Generating plot parameter
  ## =======================

  plot_config <- list(
    plot_width = plot_width,
    plot_height = plot_height*0.9,
    plot_out_height = plot_height,
    stroke_width = stroke_width,
    dend_stroke_width = dend_stroke_width,
    group_span = group_span,
    sample_span = sample_span,
    heatmap_row_span = heatmap_row_span,
    frame = frame,
    frame_stroke_width = frame_stroke_width,
    sample_font_size = sample_font_size,
    legend_font_size = legend_font_size,
    font_family = font_family
  )

  if (heatmap_data_plot) {
    sample_num <- dim(heatmap_data[[1]])[2]
    sample <- colnames(heatmap_data[[1]])
  } else {
    sample_num <- dim(legend_data)[1]
    sample <- rownames(legend_data)
  }
  plot_config <- c(plot_config, sample_num = sample_num,
                   list(sample = sample),
                   list(sample_order = sample))

  # heatamap plot parameter
  if (heatmap_data_plot) {
    for (i in 1:config_data$map_config$heatmap_num) {
      plot_config <- getHeatmapParam(heatmap_data, config_data, plot_config, i)
    }
  }

  if (legend_data_plot) {
    plot_config <- getLegendParam(legend_data, config_data, plot_config)
  }
  message("[INFO] Generating plot paramters completed")

  ## =======================
  ## Data processsing
  ## =======================

  # heatmap_1 is the leading controller of all plots
  if (heatmap_data_plot) {
    Rowv <- config_data$map_config$heatmap_1$Rowv
    if (isTRUE(Rowv)) {
      Rowv <- rowMeans(heatmap_data[[1]], na.rm = config_data$map_config$heatmap_1$na.rm)
    }
    if (is.numeric(Rowv)) {
      Rowv <- reorderfun(as.dendrogram(
        config_data$map_config$heatmap_1$hclustfun( config_data$map_config$heatmap_1$distfun(heatmap_data[[1]]) )
      ), Rowv)
    }
    if (is.dendrogram(Rowv)) {
      Rowv <- rev(Rowv)
      rowInd <- order.dendrogram(Rowv)
      if(plot_config$heatmap_1$nr != length(rowInd))
        stop("[ERROR] Row dendrogram in heatmap_1 is the wrong size")
    } else {
      if (!is.null(Rowv) && !is.na(Rowv) && !identical(Rowv, FALSE)) {
        warning("[WARNING] Invalid value for Rowv in heatmap_1, ignoring")
      }
      Rowv <- NULL
      rowInd <- 1:plot_config$heatmap_1$nr
    }

    Colv <- config_data$map_config$heatmap_1$Colv
    if (isTRUE(Colv)) {
      Colv <- colMeans(heatmap_data[[1]], na.rm = config_data$map_config$heatmap_1$na.rm)
    }
    if (is.numeric(Colv)) {
      Colv <- reorderfun(as.dendrogram(config_data$map_config$heatmap_1$hclustfun(
        config_data$map_config$heatmap_1$distfun(t(heatmap_data[[1]])))
      ), Colv)
    }
    if (is.dendrogram(Colv)) {
      sampleInd <- order.dendrogram(Colv)
      if (plot_config$heatmap_1$nc != length(sampleInd))
        stop("[ERROR] Col dendrogram is the wrong size in heatmap_1")
    } else {
      if (!is.null(Colv) && !is.na(Colv) && !identical(Colv, FALSE)) {
        warning("[WARNING] Invalid value for Colv in heatmap_1, ignoring")
      }
      Colv <- FALSE
      sampleInd <- 1:plot_config$heatmap_1$nc
    }

    if(is.dendrogram(Rowv) & (config_data$map_config$heatmap_1$kmer_row > 1)) {
      color_theme <- config_data$map_config$heatmap_1$kmer_row_color
      if (!is.null(color_theme)) {
        col_idx <- which(names(config_data$color_config) == color_theme)
        color_theme <- config_data$color_config[[col_idx]]
        #color_theme <- color_theme[1:config_data$map_config$heatmap_1$kmer_row]
      } else {
        color_theme <- config_data$color_config$black_col
      }


      Rowv <- dendextend::color_branches(Rowv,
                                         k = config_data$map_config$heatmap_1$kmer_row,
                                         col = color_theme,
                                         groupLabels=TRUE)
    }
    if(is.dendrogram(Colv) & config_data$map_config$heatmap_1$kmer_col > 1) {
      color_theme <- config_data$map_config$heatmap_1$kmer_col_color
      if (!is.null(color_theme)) {
        col_idx <- which(names(config_data$color_config) == color_theme)
        color_theme <- config_data$color_config[[col_idx]]
        #color_theme <- color_theme[1:config_data$map_config$heatmap_1$kmer_col]
      } else {
        color_theme <- config_data$color_config$black_col
      }


      Colv <- dendextend::color_branches(Colv,
                                         k = config_data$map_config$heatmap_1$kmer_col,
                                         col = color_theme,
                                         groupLabels=TRUE)
    }

    config_data$map_config$heatmap_1$Rowv = Rowv
    config_data$map_config$heatmap_1$rowInd = rowInd
    config_data$map_config$heatmap_1$Colv = Colv
    config_data$map_config$sampleInd = sampleInd

    # reorder heatmap_1
    heatmap_sub_data <- heatmap_data[[1]]
    if (!config_data$map_config$heatmap_1$raw_data) {
      heatmap_sub_data <- heatmapRowZdata(heatmap_sub_data)
    }
    # reorder heatmap mat
    heatmap_sub_data <- heatmap_sub_data[rowInd, sampleInd]
    heatmap_data[[1]] <- heatmap_sub_data

    plot_config$sample_order <- plot_config$sample[sampleInd]

    # reorder other heatmap data
    if (config_data$map_config$heatmap_num > 1) {
      for (i in 2:config_data$map_config$heatmap_num) {
        heatmap_sub_name <- paste0("heatmap_", i)
        heatmap_sub_info <- config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]
        heatmap_sub_data <- heatmap_data[[which(names(heatmap_data) == heatmap_sub_name)]]
        heatmap_sub_plot <- plot_config[[which(names(plot_config) == heatmap_sub_name)]]

        col_idx <- match(plot_config$sample_order, colnames(heatmap_sub_data))

        if (anyNA(col_idx)) {
          message("[WARNING] The sample value of ", heatmap_sub_name," data doesd't match heatmap_1")
        }
        heatmap_sub_data <- heatmap_sub_data[, col_idx]
        colnames(heatmap_sub_data) <- plot_config$sample_order

        Rowv <- heatmap_sub_info$Rowv
        if (isTRUE(Rowv)) {
          Rowv <- rowMeans(heatmap_sub_data, na.rm = heatmap_sub_info$na.rm)
        }
        if (is.numeric(Rowv)) {
          Rowv <- reorderfun(as.dendrogram(
            heatmap_sub_info$hclustfun( heatmap_sub_info$distfun(heatmap_sub_data) )
          ), Rowv)
        }
        if (is.dendrogram(Rowv)) {
          Rowv <- rev(Rowv)
          rowInd <- order.dendrogram(Rowv)
          if(heatmap_sub_plot$nr != length(rowInd))
            stop("[ERROR] Row dendrogram in ", heatmap_sub_name , " is the wrong size")
        } else {
          if (!is.null(Rowv) && !is.na(Rowv) && !identical(Rowv, FALSE)) {
            warning("[WARNING] Invalid value for Rowv in  ", heatmap_sub_name , ", ignoring")
          }
          Rowv <- FALSE
          rowInd <- 1:heatmap_sub_plot$nr
        }
        if (!heatmap_sub_info$raw_data) {
          heatmap_sub_data <- heatmapRowZdata(heatmap_sub_data)
        }

        if(is.dendrogram(Rowv) & (heatmap_sub_info$kmer_row > 1)) {
          color_theme <- heatmap_sub_info$kmer_row_color
          if (is.null(color_theme)) {
            col_idx <- which(names(config_data$color_config) == color_theme)
            color_theme <- config_data$color_config[[col_idx]]
            # color_theme <- color_theme[1:heatmap_sub_info$kmer_row]
          } else {
            color_theme <- config_data$color_config$black_col
          }


          Rowv <- dendextend::color_branches(Rowv,
                                             k = heatmap_sub_info$kmer_row,
                                             col = color_theme,
                                             groupLabels=TRUE)
        }

        # reorder heatmap mat
        heatmap_sub_data <- heatmap_sub_data[rowInd, ]
        heatmap_data[[which(names(heatmap_data) == heatmap_sub_name)]] <- heatmap_sub_data
        config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]$Rowv <- Rowv
        config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]$rowInd <- rowInd
        config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]$Colv <- FALSE
      }
    }
  }
  message("[INFO] Preprocessing heatmap data")

  if (legend_data_plot) {
    leg_sample <- rownames(legend_data)
    leg_idx <- match(plot_config$sample_order, leg_sample)
    if (anyNA(leg_idx)) {
      message("[WARNING] The rownames value of legend data doesd't match heatmap")
    }
    legend_data <- legend_data[leg_idx, ]
    rownames(legend_data) <- plot_config$sample_order
  }
  message("[INFO] Preprocessing legend data")

  # calculate SVG location
  plot_config <- checkPlotConfig(plot_config, config_data)

  group_info <- list()

  # transform position of dend
  if (heatmap_data_plot) {
    for (i in 1:config_data$map_config$heatmap_num) {
      heatmap_sub_name <- paste0("heatmap_", i)
      heatmap_sub_info <- config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]
      heatmap_sub_data <- heatmap_data[[which(names(heatmap_data) == heatmap_sub_name)]]
      heatmap_sub_plot <- plot_config[[which(names(plot_config) == heatmap_sub_name)]]

      col_attr <- dendAttribute(dend = heatmap_sub_info$Colv)
      row_attr <- dendAttribute(dend = heatmap_sub_info$Rowv)

      col_attr <- dendGap(dend_attr = col_attr, gap = plot_config$sample_span,
                          w = (heatmap_sub_plot$col_dend_w - heatmap_sub_plot$rect_w), h = heatmap_sub_plot$col_dend_h,
                          name = config_data$map_config$split_sample)
      row_attr <- dendGap(dend_attr = row_attr, gap = plot_config$heatmap_row_span,
                          w = (heatmap_sub_plot$row_dend_w - heatmap_sub_plot$rect_h), h = heatmap_sub_plot$row_dend_h,
                          name = heatmap_sub_info$split_row_name)

      config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]] <- c(
        config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]],
        list(col_attr = col_attr, row_attr = row_attr)
      )

      if (i == 1) {
        config_data$map_config$heatmap_kmer_gap = col_attr
      }

      # add group kmer tag
      group_sub_row <- data.frame(row_name = row_attr$node_label,
                                  gap = row_attr$node_group,
                                  tag = "g0")
      iii <- 0
      gtag <- rep("g0", length(group_sub_row[, 1]))
      for (ii in 1:length(group_sub_row[, 1])) {
        if (isTRUE(group_sub_row$gap[ii])) {
          iii <- iii + 1
          gtag[ii] <- paste0("g", iii)
        } else {
          gtag[ii] <- paste0("g", iii)
        }
      }
      group_sub_row$tag <- gtag
      group_sub_row <- group_sub_row[!is.na(group_sub_row$row_name), ]
      group_sub_row <- list(group_sub_row)
      names(group_sub_row) <- heatmap_sub_name
      group_info <- c(group_info, group_sub_row)

    }
  } else {
    config_data$map_config$heatmap_kmer_gap = NULL
  }

  ## =======================
  ## Generating SVG element
  ## =======================
  message("[INFO] Start to generating SVG element")
  def_content <- ""
  if (heatmap_data_plot) {
    for (i in 1:config_data$map_config$heatmap_num) {
      heatmap_sub_name <- paste0("heatmap_", i)
      heatmap_sub_info <- config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]
      heatmap_sub_data <- heatmap_data[[which(names(heatmap_data) == heatmap_sub_name)]]
      heatmap_sub_plot <- plot_config[[which(names(plot_config) == heatmap_sub_name)]]

      if (is.dendrogram(heatmap_sub_info$Colv)) {
        col_dend_svg <- dendSVG(dend_attr = heatmap_sub_info$col_attr, id = paste0(heatmap_sub_name, "_dend_col"),
                                plot_config = plot_config)
      } else {
        col_dend_svg <- group.svg(id = paste0(heatmap_sub_name, "_dend_col"), group.content = "")
      }
      if (is.dendrogram(heatmap_sub_info$Rowv)) {
        row_dend_svg <- dendSVG(dend_attr = heatmap_sub_info$row_attr, id = paste0(heatmap_sub_name, "_dend_row"),
                                plot_config = plot_config)
      } else {
        row_dend_svg <- group.svg(id = paste0(heatmap_sub_name, "_dend_row"), group.content = "")
      }
      exp_mat_svg <- heatmapSVG(heatmap_sub_data = heatmap_sub_data,
                                heatmap_sub_info = heatmap_sub_info,
                                heatmap_sub_plot = heatmap_sub_plot,
                                config_data = config_data,
                                id = paste0(heatmap_sub_name, "_exp_mat"))

      grad_svg <- heatmapGradSVG(heatmap_sub_data = heatmap_sub_data,
                                 heatmap_sub_info = heatmap_sub_info,
                                 heatmap_sub_plot = heatmap_sub_plot,
                                 config_data = config_data,
                                 id = paste0(heatmap_sub_name, "_grad"))

      def_content = paste(def_content, col_dend_svg, row_dend_svg, exp_mat_svg, grad_svg, sep = '\n')
    }
  }

  if (legend_data_plot) {
    for (i in 1:config_data$map_config$legend_num) {
      legend_sub_name <- paste0("legend_", i)
      legend_sub_info <- config_data$map_config[[which(names(config_data$map_config) == legend_sub_name)]]
      legend_sub_plot <- plot_config[[which(names(plot_config) == "legend")]]

      legend_group_svg <- legendGroupSVG(config_data = config_data,
                                         plot_config = plot_config,
                                         legend_data = legend_data,
                                         legend_sub_info = legend_sub_info,
                                         legend_sub_plot = legend_sub_plot,
                                         gap_info = config_data$map_config$heatmap_kmer_gap,
                                         id = legend_sub_name)
      legend_group_svg <- paste(legend_group_svg, collapse = "\n")
      def_content <- paste(def_content, legend_group_svg, sep = "\n")
    }
  }

  # add sample information
  if (!is.null(config_data$map_config$heatmap_kmer_gap)) {
    group_sub_col <- data.frame(col_name = config_data$map_config$heatmap_kmer_gap$node_label,
                                gap = config_data$map_config$heatmap_kmer_gap$node_group,
                                tag = "g0")
    iii <- 0
    gtag <- rep("g0", length(group_sub_col[, 1]))
    for (ii in 1:length(group_sub_col[, 1])) {
      if (group_sub_col$gap[ii]) {
        iii <- iii + 1
        gtag[ii] <- paste0("g", iii)
      } else {
        gtag[ii] <- paste0("g", iii)
      }
    }
    group_sub_col$tag <- gtag
    group_sub_col <- group_sub_col[!is.na(group_sub_col$col_name), ]
    group_sub_col <- list(group_sub_col)
    names(group_sub_col) <- "sample_list"
    group_info <- c(group_info, group_sub_col)
  }

  # add sample svg
  sample_svg <- sampleSVG(plot_config = plot_config,
                          gap_info = config_data$map_config$heatmap_kmer_gap,
                          id = "sample_list")
  def_content <- paste(def_content, sample_svg, sep = "\n")

  def_content <- defs.svg(defs.content = def_content)

  ## =======================
  ## Generating SVG Location
  ## =======================
  message("[INFO] Start to generating SVG element location")
  use_content <- ""
  kk <- 0
  for (i in 1:length(config_data$map_config$map_order)) {
    order_name <- config_data$map_config$map_order[i]
    if (grepl("^heatmap_", order_name)) {
      heatmap_sub_info <- config_data$map_config[[which(names(config_data$map_config) == order_name)]]
      heatmap_sub_plot <- plot_config[[which(names(plot_config) == heatmap_sub_name)]]
      if (order_name == "heatmap_1") {
        if (heatmap_sub_info$dendrogram == "both" | heatmap_sub_info$dendrogram == "col") {
          col_dend_use <- use.svg(id = paste0(order_name, "_dend_col"),
                                  x = heatmap_sub_plot$w*0.2 + 0.5*heatmap_sub_plot$rect_w,
                                  y = 0)
          use_content <- paste(use_content, col_dend_use, sep = "\n")
        }
      }

      if (heatmap_sub_info$dendrogram == "both" | heatmap_sub_info$dendrogram == "row") {
        row_dend_use <- use.svg(id = paste0(order_name, "_dend_row"),
                                x = 0 , y = 0, scale = c(-1, 1),
                                translate = c(plot_config$group_baseline[i] + heatmap_sub_plot$rect_h*0.5, 0),
                                rotate = c(90, 0, 0))
        use_content <- paste(use_content, row_dend_use, sep = "\n")
      }
      exp_mat_use <- use.svg(id = paste0(order_name, "_exp_mat"),
                             x = heatmap_sub_plot$w * 0.2,
                             y = plot_config$group_baseline[i])
      grad_use <- use.svg(id = paste0(order_name, "_grad"),
                             x = 0,
                             y = plot_config$group_baseline[i] - heatmap_sub_plot$rect_h*5)
      use_content <- paste(use_content, exp_mat_use, grad_use, sep = "\n")
    }
    if (grepl("^legend_", order_name)) {
      legend_sub_plot <- plot_config[[which(names(plot_config) == "legend")]]
      legend_sub_info <- config_data$map_config[[which(names(config_data$map_config) == order_name)]]
      legend_mat_use <- use.svg(id = paste0(order_name, "_mat"),
                                x = plot_config$plot_width * 0.2,
                                y = plot_config$group_baseline[i])
      legend_out_use <- use.svg(id = paste0(order_name, "_outline"),
                                x = plot_config$plot_width * 0.2,
                                y = plot_config$group_baseline[i])
      legend_app_use <- use.svg(id = paste0(order_name, "_leg_text"),
                                x = plot_config$plot_width * 0.02,
                                y = plot_config$group_baseline[length(plot_config$group_baseline)] + kk + plot_config$sample_font_size*10)
      legend_control <- max(8, legend_sub_plot$row_fz)
      legend_control <- min(14, legend_control)
      kk <- kk + (legend_control+4) * length(legend_sub_info)
      use_content <- paste(use_content, legend_mat_use, legend_out_use, legend_app_use, sep = "\n")
    }
  }

  # use sample svg
  sample_use <- use.svg(id = "sample_list",
                        x = plot_config$plot_width * 0.2,
                        y = plot_config$group_baseline[length(plot_config$group_baseline)])
  use_content <- paste(use_content, sample_use, sep = "\n")

  use_content <- group.svg(group.content = use_content, transform.sheet = "translate(20,20)", id = "use",
                           font.family = plot_config$font_family)

  ## =======================
  ## Output the result
  ## =======================
  message("[INFO] Output SVG")
  pack_content <- paste(def_content, use_content, sep = "\n")
  pack.svg(pack.content = pack_content, output.svg.name = output_svg_name,
           width = plot_config$plot_width, height = plot_config$plot_out_height)
  output_svg_name <- normalizePath(output_svg_name)

  if (output_group_info) {
    output_group_info_name <- gsub(".svg$", ".group.info.xlsx", output_svg_name)
    write.xlsx(group_info, output_group_info_name, colWidths = c(NA, "auto", "auto"))
  }

  # convert
  if (convert_pdf) {
    if (file.size(output_svg_name) > 10000000) {
      cmd = sprintf("inkscape --without-gui --export-pdf=\"%s\" %s", gsub(".svg$", ".pdf", output_svg_name), output_svg_name)
      system(cmd)
    } else {
      rsvg_pdf(svg = output_svg_name, file = gsub(".svg$", ".pdf", output_svg_name))
    }
  }
  if (convert_jpg) {
    cmd <- sprintf("convert -density 300 %s %s", output_svg_name, gsub(".svg$", ".jpg", output_svg_name))
    system(cmd)
  }

}


