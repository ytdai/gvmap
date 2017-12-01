#'
#' gvmap modify attribute to config file
#'
#' @param attr_name character, attribute name
#' @param value attribute value
#' @param target character, target of attribute
#' @param config_file list, config info generating by gvmapAutoConfig
#'
#' @return a config file to run gvmap
#'
#' @export
#' @examples
#' heatmap_info <- list("heatmap_1", "heatmap_2", "heatmap_3")
#' legend_info <- data.frame(legend_group = c("legend_1", "legend_1", "legend_2"),
#'                           name = c("Age", "Gender", "NRAS"),
#'                           col_theme = c("binary_col", "binary_col", "tag_col"),
#'                           column_num = c(2, 3, 4))
#' config_file <- gvmapAutoConfig(heatmap_info = heatmap_info,
#'                                legend_info = legend_info)
#'
#' # modify heatmap_num
#' config_file <- gvmapModAttr(attr_name = "heatmap_num",
#'                             value = 2,
#'                             config_file = config_file)
#' config_file$map_config$heatmap_num
#'
#' # modify legend_num
#' config_file <- gvmapModAttr(attr_name = "legend_num",
#'                             value = 3,
#'                             config_file = config_file)
#' config_file$map_config$legend_num
#'
#' # modify raw_data of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "raw_data",
#'                             value = FALSE, target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify kmer_col of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "kmer_col",
#'                             value = 3,
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify kmer_row of heatmap_2
#' config_file <- gvmapModAttr(attr_name = "kmer_row",
#'                             value = 4,
#'                             target = "heatmap_2",
#'                             config_file = config_file)
#'
#' # modify split_row_name of heatmap_2
#' config_file <- gvmapModAttr(attr_name = "split_row_name",
#'                             value = c("M1001", "M4001"),
#'                             target = "heatmap_2",
#'                             config_file = config_file)
#'
#' # modify kmer_col_color of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "kmer_col_color",
#'                             value = "pool_col",
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify kmer_row_color of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "kmer_row_color",
#'                             value = "tag_col",
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify dendrogram of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "dendrogram",
#'                             value = "col",
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify Rowv of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "Rowv",
#'                             value = FALSE,
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify Colv of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "Colv",
#'                             value = FALSE,
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify color_theme of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "color_theme",
#'                             value = "gradient_col_gr_oo",
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify distfun of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "distfun",
#'                             value = dist,
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify distfun of heatmap_1
#' config_file <- gvmapModAttr(attr_name = "hclustfun",
#'                             value = function(x) hclust(x,method = 'ward.D2'),
#'                             target = "heatmap_1",
#'                             config_file = config_file)
#'
#' # modify legend_1
#' # NOTE: legend only have one attr_name
#' leg_value <- data.frame(name = c("age", "gender"),
#'                         color = "binary_col",
#'                         column = c(2,3))
#' config_file <- gvmapModAttr(attr_name = "legend",
#'                             value = leg_value,
#'                             target = "legend_1",
#'                             config_file = config_file)
#'
#' # modify color
#' config_file <- gvmapModAttr(attr_name = "tag_col",
#'                             value = "#000000",
#'                             config_file = config_file)
#'
#' config_file <- gvmapModAttr(attr_name = "tag_col_1",
#'                             value = "#FF00000",
#'                             config_file = config_file)
#'
#'
gvmapModAttr <- function( attr_name, value, target, config_file ) {
  if (missing(attr_name) | missing(value) | missing(config_file)) {
    stop("[ERROR] attr_name, value, targer and config_file must be input")
  }

  if (!is.list(config_file)) {
    stop("[ERROR] config_file must be a list")
  }

  if (!is.character(attr_name) & length(attr_name) > 1) {
    stop("[ERROR] attr_name must be a character")
  }

  if (grepl("heatmap_num", attr_name)) {
    if (is.numeric(value)) {
      config_file$map_config$heatmap_num <- value
    } else {
      stop("[ERROR] the value of heatmap_num must be a number")
    }
  }

  if (grepl("legend_num", attr_name)) {
    if (is.numeric(value)) {
      config_file$map_config$legend_num <- value
    } else {
      stop("[ERROR] the value of legend_num must be a number")
    }
  }

  if (grepl("map_order", attr_name)) {
    if (is.vector(value)) {
      config_file$map_config$map_order <- value
    } else {
      stop("[ERROR] the value of map_order must be a vector")
    }
  }

  if (grepl("split_sample", attr_name)) {
    if (is.vector(value)) {
      config_file$map_config$split_sample <- value
    } else {
      stop("[ERROR] the value of split_sample must be a vector")
    }
  }

  if (grepl("raw_data", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.logical(value)) {
        stop("[ERROR] value of raw_data must be logical")
      } else {
        config_file$map_config[[idx]]$raw_data <- value
      }
    }
  }

  if (grepl("kmer_col$", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.numeric(value)) {
        stop("[ERROR] value of raw_data must be a number")
      } else {
        config_file$map_config[[idx]]$kmer_col <- value
      }
    }
  }

  if (grepl("kmer_row$", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.numeric(value)) {
        stop("[ERROR] value of raw_data must be a number")
      } else {
        config_file$map_config[[idx]]$kmer_row <- value
      }
    }
  }

  if (grepl("split_row_name", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.vector(value)) {
        stop("[ERROR] value of raw_data must be a vector")
      } else {
        config_file$map_config[[idx]]$split_row_name <- value
      }
    }
  }

  if (grepl("percen", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.numeric(value)) {
        stop("[ERROR] value of raw_data must be a number")
      } else {
        config_file$map_config[[idx]]$percentage <- value
      }
    }
  }

  if (grepl("kmer_col_color", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.character(value)) {
        stop("[ERROR] value of kmer_col_color must be a character")
      } else {
        if (grepl("bg_|tag_|binary_|white_|black_|pool_|gradient_|kmer_", value)) {
          config_file$map_config[[idx]]$kmer_col_color <- value
        } else {
          stop("[ERROR] invalid value of kmer_col_color")
        }
      }
    }
  }

  if (grepl("kmer_row_color", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.character(value)) {
        stop("[ERROR] value of kmer_row_color must be a character")
      } else {
        if (grepl("bg_|tag_|binary_|white_|black_|pool_|gradient_|kmer_", value)) {
          config_file$map_config[[idx]]$kmer_row_color <- value
        } else {
          stop("[ERROR] invalid value of kmer_row_color")
        }
      }
    }
  }

  if (grepl("dendrogram", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.character(value)) {
        stop("[ERROR] value of dendrogram must be a character")
      } else {
        if (value %in% c("none", "col", "row", "both")) {
          config_file$map_config[[idx]]$dendrogram <- value
        } else {
          stop("[ERROR] invalid value of dendrogram, must be \"none\", \"col\", \"row\" or \"both\"")
        }
      }
    }
  }

  if (grepl("Rowv", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.logical(value)) {
        stop("[ERROR] value of Rowv must be logical")
      } else {
        config_file$map_config[[idx]]$Rowv <- value
      }
    }
  }

  if (grepl("Colv", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.logical(value)) {
        stop("[ERROR] value of Colv must be logical")
      } else {
        config_file$map_config[[idx]]$Colv <- value
      }
    }
  }

  if (grepl("color_theme", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.character(value)) {
        stop("[ERROR] value of kmer_row_color must be a character")
      } else {
        if (grepl("bg_|tag_|binary_|white_|black_|pool_|gradient_|kmer_", value)) {
          config_file$map_config[[idx]]$color_theme <- value
        } else {
          stop("[ERROR] invalid value of color_theme")
        }
      }
    }
  }

  if (grepl("distfun", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.function(value) & !is.character(value)) {
        stop("[ERROR] value of distfun must be a function or a character")
      } else {
        config_file$map_config[[idx]]$distfun <- value
      }
    }
  }

  if (grepl("hclustfun", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. heatmap_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.function(value) & !is.character(value)) {
        stop("[ERROR] value of hclustfun must be a function or a character")
      } else {
        config_file$map_config[[idx]]$hclustfun <- value
      }
    }
  }

  if (grepl("legend$", attr_name)) {
    if (missing(target)) {
      stop("[ERROR] target is required, eg. legend_1")
    } else {
      idx <- which(names(config_file$map_config) == target)
      if (!is.data.frame(value)) {
        stop("[ERROR] value of legend must be a data frame")
      } else {
        legend_sub_config <- lapply(1:length(value[, 1]), function(x) return(list(value[x, 1], value[x, 2], value[x, 3])))
        config_file$map_config[[idx]] <- legend_sub_config
      }
    }
  }

  if (grepl("bg_|tag_|binary_|white_|black_|pool_|gradient_|kmer_|gv_line_|gv_bar_|gv_dot_|mutation_", attr_name))  {
    if (attr_name %in% names(config_file$color_config)) {
      idx <- which(names(config_file$color_config) == attr_name)
      config_file$color_config[[idx]] <- value
    } else {
      value <- list(value)
      names(value) <- attr_name
      config_file$color_config <- c(config_file$color_config, value)
    }
  }

  return(config_file)
}



