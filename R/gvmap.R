#'
#' gvmap package
#'
#' @param legend_data a data frame. The legend data of the plot
#'
#' @examples
#' legend_data <- "inst/extdata/gvmap.test.txt"
#' config_file <- "inst/extdata/config.yaml"
#' heatmap_data <- "inst/extdata/count.txt"
#' output_svg_name <- "tests/out.svg"
#'
gvmap <- function(legend_data,
                  heatmap_data,
                  config_file,

                  # output svg name
                  output_svg_name,
                  convert_pdf = TRUE,

                  # plot for heatmap data
                  Rowv = TRUE,
                  Colv = if (symm) "Rowv" else TRUE,
                  distfun = dist,
                  hclustfun = hclust,
                  dendrogram = c("both", "row", "column", "none"),
                  reorderfun = function(d, w) reorder(d, w),

                  symm = FALSE,
                  revC,

                  ## data scaling
                  na.rm = TRUE,

                  # plot for canvas data
                  plot_width = 1200,
                  plot_height = 1600,
                  stroke_width = 0.5,
                  group_span = NULL,
                  sample_span = NULL,
                  frame = TRUE,
                  sample_font_size = NULL,
                  legend_font_size = NULL,
                  font_family = "Arial") {

  # read config file and generate config parmeters
  if (missing(config_file)) {
    stop("[ERROR] Config file is required")
  }
  config_data <- gvmapConfig(config_file)

  # read legend data
  if (missing(legend_data)) {
    message("[WARNING] Legend data is missing")
    if (config_data$map_config$legmap_num > 0) {
      message("[WARNING] The legmap number in config file is over zero, please check your data")
    }
    legend_data_plot <- FALSE
  } else {
    if (config_data$map_config$legmap_num == 0) {
      message("[INFO] The legmap number in config file is zero, the legmap will not plot")
      legend_data_plot <- FALSE
    } else {
      legend_data_plot <- TRUE
    }
    legend_data <- readLegendFile(legend_data)
  }

  # read heatmap data
  if (missing(heatmap_data)) {
    message("[WARNING] heatmap data is missing")
    if (config_data$map_config$heatmap_num > 0) {
      message("[WARNING] The heatmap number in config file is over zero, please check your data")
    }
    heatmap_data_plot <- FALSE
  } else {
    if (config_data$map_config$heatmap_num == 0) {
      message("[INFO] The heatmap number in config file is zero, the heatmap will not plot")
      heatmap_data_plot <- FALSE
    } else {
      heatmap_data_plot <- TRUE
    }
    heatmap_data <- readHeatmapFile(heatmap_data)
  }

  plot_config <- list(
    plot_width = plot_width,
    plot_height = plot_height,
    stroke_width = stroke_width,
    group_span = group_span,
    sample_span = sample_span,
    frame = frame,
    sample_font_size = sample_font_size,
    legend_font_size = legend_font_size,
    font_family = font_family
  )

  if (heatmap_data_plot) {
    nr <- dim(heatmap_data)[1]
    nc <- dim(heatmap_data)[2]

    # plot parameter
    plot_config <- c(
      plot_config,
      list(
        heatmap_plot = TRUE,
        heatmap_num = config_data$map_config$heatmap_num,
        heatmap_row_num = nr,
        heatmap_col_num = nc
      ))

    # pre processing of heatmap data

    ## Dendrograms for Row/Column of heatmap
    ##=======================
    dendrogram <- match.arg(dendrogram)

    # Use dendrogram argument to set defaults for Rowv/Colv
    if (missing(Rowv)) {
      Rowv <- dendrogram %in% c("both", "row")
    }
    if (missing(Colv)) {
      Colv <- dendrogram %in% c("both", "column")
    }

    if (isTRUE(Rowv)) {
      Rowv <- rowMeans(heatmap_data, na.rm = na.rm)
    }
    if (is.numeric(Rowv)) {
      Rowv <- reorderfun(as.dendrogram(hclustfun(distfun(heatmap_data))), Rowv)
    }
    if (is.dendrogram(Rowv)) {
      Rowv <- rev(Rowv)
      rowInd <- order.dendrogram(Rowv)
      if(nr != length(rowInd))
        stop("[ERROR] Row dendrogram is the wrong size")
    } else {
      if (!is.null(Rowv) && !is.na(Rowv) && !identical(Rowv, FALSE))
        warning("[WARNING] Invalid value for Rowv, ignoring")
      Rowv <- NULL
      rowInd <- 1:nr
    }

    if (identical(Colv, "Rowv")) {
      Colv <- Rowv
    }
    if (isTRUE(Colv)) {
      Colv <- colMeans(heatmap_data, na.rm = na.rm)
    }
    if (is.numeric(Colv)) {
      Colv <- reorderfun(as.dendrogram(hclustfun(distfun(t(heatmap_data)))), Colv)
    }
    if (is.dendrogram(Colv)) {
      colInd <- order.dendrogram(Colv)
      if (nc != length(colInd))
        stop("[ERROR] Col dendrogram is the wrong size")
    } else {
      if (!is.null(Colv) && !is.na(Colv) && !identical(Colv, FALSE))
        warning("[WARNING] Invalid value for Colv, ignoring")
      Colv <- NULL
      colInd <- 1:nc
    }


    # TODO:  We may wish to change the defaults a bit in the future
    ## revC
    ##=======================
    if(missing(revC)) {
      if (symm) {
        revC <- TRUE
      } else if(is.dendrogram(Colv) & is.dendrogram(Rowv) & identical(Rowv, rev(Colv))) {
        revC <- TRUE
      } else {
        revC <- FALSE
      }
    }
    if(revC) {
      Colv <- rev(Colv)
      colInd <- rev(colInd)
    }

    ## reorder x (and others)
    ##=======================
    heatmap_data <- heatmap_data[rowInd, colInd]
  }


  if (legend_data_plot) {

    # pre processing of legmap data
    # reorder legend data
    if (heatmap_data_plot) {
      leg.ind <- match(colnames(heatmap_data), row.names(legend_data))
      if (anyNA(leg.ind)) {
        message("[WARNING] The rownames value of legend data doesd't match heatmap")
      }
      legend_data <- legend_data[leg.ind, ]
      row.names(legend_data) <- colnames(heatmap_data)
    }

    # add lengend data config
    plot_config <- c(
      plot_config,
      list(
        legmap_plot = TRUE,
        legmap_num = config_data$map_config$legmap_num,
        legmap_col_num = length(row.names(legend_data)),
        legmap_row_num = unlist(lapply(1:config_data$map_config$legmap_num, function(x) {
          length(config_data$map_config[[grep("legmap_[0-9]", names(config_data$map_config))[x]]])
        })))
    )
  }
  plot_config <- checkPlotConfig(plot_config)

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


