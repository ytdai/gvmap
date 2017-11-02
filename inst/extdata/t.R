leg.ind <- match(colnames(heatmap_data), row.names(legend_data))
      if (anyNA(leg.ind)) {
        message("[WARNING] The rownames value of legend data doesd't match heatmap")
      }
      legend_data <- legend_data[leg.ind, ]
      row.names(legend_data) <- colnames(heatmap_data)