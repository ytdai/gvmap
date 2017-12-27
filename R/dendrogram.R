#
# dendAttribute
# This function is to transfer the dendrogram object info into coordinate
# @param dend a dendrogram object
# @return x position, y position, and the father node
#
dendAttribute <- function(dend) {
  # dend %>% unclass %>% str

  if (is.dendrogram(dend)) {
    node_h <- dend %>% get_nodes_attr("height")
    node_mb <- dend %>% get_nodes_attr("members")
    # how much "left" is this node from its left-most child's location
    node_mp <- dend %>% get_nodes_attr("midpoint")
    node_label <- dend %>% get_nodes_attr("label")

    node_edgePar <- dend %>% get_nodes_attr("edgePar")

    node_color <- lapply(1:length(node_edgePar), function(x) {
       if ("col" %in% names(node_edgePar[[x]])) {
         return(node_edgePar[[x]]$col)
       } else {
         return(NA)
       }
    })
    node_color <- unlist(node_color)

    node_group <- lapply(1:length(node_edgePar), function(x) {
      if ("p.border" %in% names(node_edgePar[[x]])) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    node_group <- unlist(node_group)

    k <- rep(0, length(node_mb))
    for (x in 2:length(node_mb)) {
      if (node_mb[x] >= node_mb[x-1]) {
        k[x] = k[x-1] + 1
      } else {
        k[x] = k[x-1]
      }
    }

    node_mp[which(is.na(node_mp))] = 0
    node_x <- node_mp + k
    node_y <- node_h
    # plot(node_x, node_y)

    node_father <- rep(0, length(node_mb))
    for (x in 2:length(node_mb)) {
      if (node_mb[x] < node_mb[x-1]) {
        node_father[x] <- x-1
      } else {
        m <- rev(node_mb[1:x])
        sum = 0
        for (j in 1:length(m)) {
          if (m[j] == 1 | j == 1) {
            sum = sum + m[j]
          }
          if (m[j] == sum & j != 1) {
            node_father[x] = length(m) - j + 1
            break
          }
        }
      }
    }

  } else {
    node_x <- NA
    node_y <- NA
    node_father <- NA
    node_color <- NA
    node_group <- NA
    node_label <- NA
  }
  dend_attr <- list(node_x = node_x,
                    node_y = node_y,
                    node_label = node_label,
                    node_father = node_father,
                    node_color = node_color,
                    node_group = node_group)

  return(dend_attr)
}


is.dendrogram <- function (x) { inherits(x, "dendrogram")  }


reorderfun <- function(d, w) reorder(d, w)

#
# judge and add gap in heatmap dend
#
dendGap <- function(dend_attr, gap, w, h, name) {
  if (length(dend_attr$node_x) > 1) {
    aa <- rep(0, length(dend_attr$node_x))
    bb <- rep(0, length(dend_attr$node_x))
    k = 0
    for (i in 2:length(dend_attr$node_x)) {
      if (isTRUE(dend_attr$node_group[i])) {
        aa[i] = aa[i-1] + gap * k
        k = 1
      } else {
        aa[i] = aa[i-1]
      }
      if (dend_attr$node_label[i] %in% name) {
        bb[i] = bb[i-1] + gap
      } else {
        bb[i] = bb[i-1]
      }
    }

    zoom_w <- (w - max(aa) - max(bb)) / max(dend_attr$node_x)
    zoom_h <- (h) / max(dend_attr$node_y)

    dend_attr$node_x <- dend_attr$node_x * zoom_w + aa + bb
    dend_attr$node_y <- dend_attr$node_y * zoom_h

    dend_attr$node_kmer_gap <- aa
    dend_attr$node_name_gap <- bb

    return(dend_attr)

  }
  return(dend_attr)
}


