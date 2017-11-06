
file_r <- dir(path = "R/")
file_p <- paste0("R/", file_r)
for (k in file_p) {
  source(file = k)
}


legend_data <- "inst/extdata/gvmap.test.txt"
config_file <- "inst/extdata/config.yaml"

heatmap_data_file_1 <- "inst/extdata/count.txt"
heatmap_data_file_2 <- "inst/extdata/count.1.txt"
heatmap_data_file_3 <- "inst/extdata/count.1.txt"
heatmap_data <- list(heatmap_1 = heatmap_data_file_1,
                     heatmap_2 = heatmap_data_file_2,
                     heatmap_3 = heatmap_data_file_3)

output_svg_name <- "tests/out1.svg"

gvmap(legend_data = legend_data,
      heatmap_data = heatmap_data,
      config_file = config_file,
      output_svg_name = output_svg_name,
      group_span = 10,
      sample_span = 5,
      frame_stroke_width = 2,
      heatmap_row_span = 2,
      dend_stroke_width = 2,
      stroke_width = 1,
      plot_width = 1000,
      plot_height = 1300)




 ############################################
exp_mat <- read.csv("inst/extdata/symbol.raw_data.csv", header = T)

exp_mat <- read.table("inst/extdata/count.txt", header = T)
row.names(exp_mat) <- exp_mat[, 1]
exp_mat <- exp_mat[, -1]
head(exp_mat)

x <- exp_mat

heatmap.2(as.matrix(symbol_data),
          col = color.out,
          hclust=function(x) hclust(x,method = 'ward.D2'),
          distfun=function(x) as.dist((1-cor(t(x)))/2),
          scale = "row", dendrogram = 'both',
          key = TRUE, symkey = FALSE, density.info = "none",
          trace = "none", cexRow = 0.5,
          ColSideColors = as.character(color_slide$color_cell))


heatmap.2(heatmap_data,
          col = colorRampPalette(config_data$color_config$gradient_col)(400),
          scale = "row", dendrogram = 'both',
          key = TRUE, symkey = FALSE, density.info = "none",
          trace = "none", cexRow = 0.5)

heatmap.2(heatmap_sub_data, scale = "col",
          col = colorRampPalette(c("green", "black", "red"))(400),
          dendrogram = 'both', density.info = "none",
          trace = "none", cexRow = 0.5)


plot_width = 1200
plot_height = 1600
stroke_width = 0.5
dend_stroke_width = 2
group_span = 30
sample_span = 20
heatmap_row_span = 10
frame = TRUE
frame_stroke_width = 2
sample_font_size = NULL
legend_font_size = NULL
font_family = "Arial"

#
library(gplots)
library(configr)
library(dendextend)
library(matrixStats)
library(easySVG)
library(rsvg)



dend <- dendextend::color_branches(dend,
                           k = 3,
                           col = c("#000000"),
                           groupLabels = T)



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
exp_gene <- head(exp_data, 50)

write.table(exp_gene, file = "inst/extdata/count.1.txt", sep = "\t", col.names = T, row.names = F, quote = F)
#############################

