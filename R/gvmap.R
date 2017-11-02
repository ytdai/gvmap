#'
#' gvmap package
#'
#' @param legend_data a data frame. The basic legend data of the plot
#' @param heatmap_data a numberic matrix
#' @param config_file a configuration file
#'
#' @param Rowv determines if and how the row dendrogram should be reordered.	By default,
#' it is TRUE, which implies dendrogram is computed and reordered based on row means.
#' If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a
#' dendrogram, then it is used "as-is", ie without any reordering. If a vector of
#' integers, then dendrogram is computed and reordered based on the order of the vector.
#' @param Colv determines if and how the column dendrogram should be reordered.	Has the options
#' as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means
#' that columns should be treated identically to the rows.
#' @param distfun function used to compute the distance (dissimilarity) between both rows
#' and columns. Defaults to dist.
#' @param hclustfun function used to compute the hierarchical clustering when Rowv or
#' Colv are not dendrograms. Defaults to hclust.
#' @param dendrogram character string indicating whether to draw 'none', 'row', 'column' or
#' 'both' dendrograms. Defaults to 'both'. However, if Rowv (or Colv) is FALSE or NULL
#' and dendrogram is 'both', then a warning is issued and Rowv (or Colv) arguments are honoured.
#' @param reorderfun function(d, w) of dendrogram and weights for reordering the row and
#' column dendrograms. The default uses stats{reorder.dendrogram}
#' @param symm logical indicating if x should be treated symmetrically; can only be true
#' when x is a square matrix.
#' @param revC logical indicating if the column order should be reversed for plotting.
#' Default (when missing) - is FALSE, unless symm is TRUE.
#' This is useful for cor matrix.
#'
#' @seealso
#' \link{heatmap}, \link[gplots]{heatmap.2}
#'
#' @examples
#' legend_data <- "inst/extdata/gvmap.test.txt"
#' config_file <- "inst/extdata/config.yaml"
#'
#' heatmap_data_file_1 <- "inst/extdata/count.txt"
#' heatmap_data_file_2 <- "inst/extdata/count.1.txt"
#' heatmap_data <- list(heatmap_1 = heatmap_data_file_1,
#'                      heatmap_2 = heatmap_data_file_2)
#'
#' heatmap_data <- readHeatmapFile(heatmap_data)
#'
#' output_svg_name <- "tests/out.svg"
#'
gvmap <- function(legend_data,
                  heatmap_data,
                  config_file,

                  # output svg name
                  output_svg_name,
                  convert_pdf = TRUE,
                  convert_png = FALSE,

                  # plot for canvas data
                  plot_width = 1200,
                  plot_height = 1600,
                  stroke_width = 0.5,
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

  if (!heatmap_data_plot & !legend_data_plot) {
    stop("[ERROR] None heatmap or legend, please check your data!")
  }

  ## =======================
  ## Generating plot parameter
  ## =======================

  plot_config <- list(
    plot_width = plot_width,
    plot_height = plot_height,
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
      col_idx <- which(names(config_data$color_config) == color_theme)
      color_theme <- config_data$color_config[[col_idx]]
      color_theme <- color_theme[1:config_data$map_config$heatmap_1$kmer_row]

      Rowv <- dendextend::color_branches(Rowv,
                                         k = config_data$map_config$heatmap_1$kmer_row,
                                         col = color_theme)
    }
    if(is.dendrogram(Colv) & !is.null(config_data$map_config$heatmap_1$kmer_col > 1)) {
      color_theme <- config_data$map_config$heatmap_1$kmer_col_color
      col_idx <- which(names(config_data$color_config) == color_theme)
      color_theme <- config_data$color_config[[col_idx]]
      color_theme <- color_theme[1:config_data$map_config$heatmap_1$kmer_col]

      Colv <- dendextend::color_branches(Colv,
                                         k = config_data$map_config$heatmap_1$kmer_col,
                                         col = color_theme)
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
        # reorder heatmap mat
        heatmap_sub_data <- heatmap_sub_data[rowInd, ]
        heatmap_data[[which(names(heatmap_data) == heatmap_sub_name)]] <- heatmap_sub_data
        config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]$Rowv <- Rowv
        config_data$map_config[[which(names(config_data$map_config) == heatmap_sub_name)]]$rowInd <- rowInd
      }
    }
  }

  if (legend_data_plot) {
    leg_sample <- rownames(legend_data)
    leg_idx <- match(plot_config$sample_order, leg_sample)
    if (anyNA(leg_idx)) {
      message("[WARNING] The rownames value of legend data doesd't match heatmap")
    }
    legend_data <- legend_data[leg_idx, ]
    rownames(legend_data) <- plot_config$sample_order
  }

  # calculate SVG location
  plot_config <- checkPlotConfig(plot_config, config_data)

  ## =======================
  ## Generating SVG element
  ## =======================
  def_content <- ""

  ## =======================
  ## Generating SVG Location
  ## =======================

  ## =======================
  ## Output the result
  ## =======================

  # ------------------------------------
  # svg element generate
  # ------------------------------------
  def_content <- ""
  loc_content <- ""

  if (heatmap_data_plot) {
    col_attr <- dendAttribute(dend = Colv)
    row_attr <- dendAttribute(dend = Rowv)

    col_dend_svg <- dendSVG(dend = Colv, node_attr = col_attr, id = "heatmap_dend_col",
                            plot_config = plot_config, tag = "col")
    row_dend_svg <- dendSVG(dend = Rowv, node_attr = row_attr, id = "heatmap_dend_row",
                            plot_config = plot_config, tag = "row")

    z_data <- heatmapZdata(heatmap_data)
    heatmap_svg <- heatmapSVG(z_data = z_data,
                              config_data = config_data,
                              plot_config = plot_config,
                              id = "heatmap_exp_mat")
    heatmap_grad_svg <- heatmapGradSVG(z_data = z_data,
                                       config_data = config_data,
                                       plot_config = plot_config,
                                       id = "heatmap_grad")

    heatmap_sample_svg <- heatmapSampleSVG(heatmap_data = heatmap_data,
                                           plot_config = plot_config,
                                           id = "heatmap_sample_svg")

    heatmap_svg_defs <- paste(col_dend_svg, row_dend_svg,
                              heatmap_svg, heatmap_sample_svg,
                              heatmap_grad_svg, sep = "\n")

    def_content <- paste(def_content, heatmap_svg_defs, sep = "\n")

  }

  if (legend_data_plot) {
    legend_sample_svg <- legendSampleSVG(legend_data = legend_data,
                                         plot_config = plot_config,
                                         id = "legend_sample_svg")

    legend_group_svg <- lapply(1:config_data$map_config$legmap_num, function(i) {
      legendGroupSVG(legend_data, config_data, plot_config, i)
    })
    legend_group_svg <- paste(legend_group_svg, collapse = "\n")

    def_content <- paste(def_content, legend_group_svg, sep = "\n")
  }

  def_content <- defs.svg(defs.content = def_content)


  if (TRUE) {
    col_dend_use <- use.svg(id = "heatmap_dend_col", x = plot_config$plot_width*0.2 + 0.5*plot_config$heatmap_col_rect , y = 0)
    row_dend_use <- use.svg(id = "heatmap_dend_row", x = 0 , y = 0, scale = c(-1, 1), translate = c(plot_config$heatmap_height*0.2 + plot_config$heatmap_row_rect*0.5, 0), rotate = c(90, 0, 0))
    heatmap_use <- use.svg(id = "heatmap_exp_mat", x = plot_config$plot_width*0.2 , y = plot_config$heatmap_height*0.2)
    heatmap_sampe_use <- use.svg(id = "heatmap_sample_svg", x = plot_config$plot_width*0.2 , y = plot_config$heatmap_height)
    heatmap_grad_use <- use.svg(id = "heatmap_grad", x = plot_config$plot_width*0.03 , y = plot_config$heatmap_height*0.05)

    heatmap_svg_use <- paste(col_dend_use, row_dend_use, heatmap_use, heatmap_sampe_use, heatmap_grad_use, sep = "\n")
    loc_content <- paste(loc_content, heatmap_svg_use, sep = "\n")
  }

  gvmap_content <- paste(def_content, loc_content, sep = "\n")

  pack.svg(pack.content = gvmap_content, output.svg.name = "tests/out.svg",
           width = plot_config$plot_width, height = plot_config$plot_height)
  rsvg_pdf(svg = "tests/out.svg", file = "tests/out.pdf")
  rsvg_png(svg = "tests/out.svg", file = "tests/out.png")
}


