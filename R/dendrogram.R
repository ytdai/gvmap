#'
#' dendAttribute
#' This function is to transfer the dendrogram object info into coordinate
#' @param dend a dendrogram object
#' @return x position, y position, and the father node
#'
dendAttribute <- function(dend) {
  node_h <- dend %>% get_nodes_attr("height")
  node_mb <- dend %>% get_nodes_attr("members")
  # how much "left" is this node from its left-most child's location
  node_mp <- dend %>% get_nodes_attr("midpoint")

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

  dend_attr <- list(node_x = node_x,
                    node_y = node_y,
                    node_father = node_father)

  return(dend_attr)
}

#'
#' is.dendrogram
#'
is.dendrogram <- function (x) { inherits(x, "dendrogram")  }

