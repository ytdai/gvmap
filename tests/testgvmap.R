


exp_mat <- read.table("inst/extdata/count.txt", header = T)
row.names(exp_mat) <- exp_mat[, 1]
exp_mat <- exp_mat[, -1]
head(exp_mat)

x <- exp_mat

# heatmap.2(as.matrix(exp_mat))


heatmap.2(heatmap_data,
          col = colorRampPalette(config_data$color_config$gradient_col)(400),
          scale = "row", dendrogram = 'both',
          key = TRUE, symkey = FALSE, density.info = "none",
          trace = "none", cexRow = 0.5)

#
library(gplots)
library(configr)
library(dendextend)
library(matrixStats)
library(easySVG)
library(rsvg)



if(FALSE) {

  x <- hclust(dist(1:3))
  library(dendextend)
  d <- color_branches(as.dendrogram(x), k = 2)
  plot(d)

  #source this whole file first!
  str(hclustToTree(x)[[1]])
  str(dendToTree(as.dendrogram(x)))
  str(dendToTree(d))

  str(dendToTree(row_dend2))

  a <- dendToTree(d)
  dendToTree(dend)

  x <- hclust(dist(rnorm(40)))
  d <- color_branches(as.dendrogram(x), k = 2)
  plot(d)

  d %>% labels
  d %>% nleaves
  d %>% nnodes

  node_h <- d %>% get_nodes_attr("height")
  node_mb <- d %>% get_nodes_attr("members")
  node_mp <- d %>% get_nodes_attr("midpoint") # how much "left" is this node from its left-most child's location

  node_attr <- dendAttribute(d)

  leaf_col <- get_leaves_branches_col(d)

  d %>% hang.dendrogram %>% get_nodes_attr("height")

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

  plot(node_x, node_y)

  node_father <- rep(0, length(node_mb))
  for (x in 2:length(node_mb)) {
    if (node_mb[x] < node_mb[x-1]) {
      node_father[x] <- x-1
    } else if (node_mb[x] == node_mb[x-1]) {
      node_father[x] <- x-2
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
}



#############################
getwd()

exp_data <- read.csv("inst/extdata/symbol.raw_data.csv")

library(matrixStats)

a <- rowVars(as.matrix(exp_data[, 2:11]))

exp_data <- exp_data[ order(a), ]
exp_gene <- head(exp_data, 100)

write.table(exp_gene, file = "inst/extdata/count.txt", sep = "\t", col.names = T, row.names = F, quote = F)
#############################

