
library(configr)
library(dendextend)
library(matrixStats)
library(easySVG)
library(rsvg)
library(stringr)

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

output_svg_name <- "tests/g3.svg"
config_file <- "inst/extdata/config.g3.yaml"

gvmap(legend_data = legend_data,
      heatmap_data = heatmap_data,
      config_file = config_file,
      output_svg_name = output_svg_name,
      plot_width = 1200,
      plot_height = 1600,
      dend_stroke_width = 1,
      heatmap_row_span = 10)




# test ETV6-RUNX1
g3_exp_data <- read.csv("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/count/exp_g3.9.csv")

sample_order_g3 <- read.csv("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/develop/ball_data/heatmap/g3/fig_subtype_sample0.95.csv")

rownames(g3_exp_data) <- g3_exp_data[, 2]
g3_exp_data <- g3_exp_data[, -1]
g3_exp_data <- g3_exp_data[, -1]

g3_exp_data <- g3_exp_data[, match(sample_order_g3$x, colnames(g3_exp_data))]

filygenes <- function(exp)
{
  geninfo <- read.table("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/count/geneinfo/geneloc.txt")
  fil <- geninfo[,1] == "chrY"
  ygenes <- geninfo[fil,4]
  fil <- row.names(exp) %in% ygenes
  exp <- exp[!fil,]
  return(exp)
}

filxgenes <- function(exp)
{
  geninfo <- read.table("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/count/geneinfo/geneloc.txt")
  fil <- geninfo[,1] == "chrX"
  ygenes <- geninfo[fil,4]
  fil <- row.names(exp) %in% ygenes
  exp <- exp[!fil,]
  return(exp)
}

blackgenes <- read.table("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/count/heatmap_blackgenes.txt")
dat.tmp <- g3_exp_data[!row.names(g3_exp_data) %in% blackgenes[,1], ]

fil <- str_detect(row.names(dat.tmp), fixed("."))
dat.tmp <- dat.tmp[!fil,]
fil <- str_detect(row.names(dat.tmp), "^MIR|^RN7|^RNA|^SNOR|^LINC|^TRAJ|^RNY")
dat.tmp <- dat.tmp[!fil,]
dat.tmp <- filygenes(dat.tmp)
dat.tmp <- filxgenes(dat.tmp)

g3_exp <- dat.tmp

sig_gene <- rowVars(as.matrix(g3_exp))

sig_gene_data <- data.frame(index = 1:length(sig_gene),
                            var = sig_gene)
sig_gene_data <- sig_gene_data[ order(sig_gene_data$var, decreasing = T), ]
sel <- sig_gene_data[1:floor(length(sig_gene_data[ , 1]) * 0.05), ]

heatmap_1 <- g3_exp[sel$index, ]

sel <- sig_gene_data[floor(length(sig_gene_data[ , 1]) * 0.05):floor(length(sig_gene_data[ , 1]) * 0.08), ]

heatmap_2 <- g3_exp[sel$index, ]

g3_info <- read.table("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/develop/ball_data/g3.info.txt", header = T, sep = "\t")

data_file <- "/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/develop/ball_data/ball.sampleinfo.txt"

data_file <- read.table(data_file, header = T, sep = "\t", na.strings = "#N/A", comment.char = "!")
sample_order_merge <- merge(sample_order_g3, data_file, sort = F, all.x = T, by.x = "x", by.y = "nid")
sample_order_merge <- merge(sample_order_merge[, 1:3], g3_info, sort = F, all.x = T, by.x = "x", by.y = "sample")

a <- rep("0", length(sample_order_merge$x))
for (i in 1:length(sample_order_merge$x)) {
  if (sample_order_merge$other.fusion[i] != "0") {
    a[i] = as.character(sample_order_merge$other.fusion[i])
  } else if (sample_order_merge$ETV6.RUNX1.like[i] != "0") {
    a[i] = as.character(sample_order_merge$ETV6.RUNX1.like[i])
  }
}

sample_order_merge$ETV6.RUNX1.like <- a

mutation_file <- read.table("/Users/daiyuting/Documents/projects/ball/analysis/metarnaseq/develop/ball_data/sj.dat.txt", header = T, sep = "\t")
mutation_merge <- merge(sample_order_g3, mutation_file, all.x = T, sort = F, by.x = "x", by.y = "sample")

gene_list <- unique(mutation_merge$gene)
mut_mat <- data.frame(matrix(0, nrow = length(sample_order_g3$x), ncol = length(gene_list)))
colnames(mut_mat) <- gene_list
row.names(mut_mat) <- sample_order_g3$x

for (i in 1:length(mut_mat)) {
  for (j in 1:length(mut_mat[, 1])) {
    #message(i, "  ", j)
    tag <- intersect(which(mutation_merge$gene == gene_list[i]),
                     which(mutation_merge$x == sample_order_g3$x[j]))
    if (length(tag) > 0) {
      mut_mat[j, i] <- paste(mutation_merge$class[tag], collapse = "/")
    } else {
      mut_mat[j, i] <- 0
    }
  }
}

mun_tag <- rep(0, length(mut_mat))
for (i in 1:length(mut_mat)) {
  mun_tag[i] <- length(which(mut_mat[, i] == "0"))
}
plot_mut_mat <- mut_mat[, (mun_tag < 159)]

data_plot_info <- cbind(sample_order_merge, plot_mut_mat)


heatmap_1_f <- heatmap_1[, 90:150]
heatmap_2_f <- heatmap_2[, 90:150]
data_plot_info_f <- data_plot_info[90:150, ]

heatmap_data <- list(heatmap_1 = heatmap_1_f,
                     heatmap_2 = heatmap_2_f)

legend_data <- data_plot_info_f


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

