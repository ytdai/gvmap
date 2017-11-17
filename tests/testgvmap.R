
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


legend_data <- "inst/extdata/legend.txt"
config_file <- "inst/extdata/config.mtp.yaml"

heatmap_data_file_1 <- "inst/extdata/heatmap.txt"
heatmap_data_1 <- read.table(heatmap_data_file_1, sep = "\t", header = T)
rownames(heatmap_data_1) <- heatmap_data_1$X
heatmap_data_1 <- heatmap_data_1[, -1]

heatmap_data_2 <- heatmap_data_1[20:50, ]
heatmap_data_3 <- heatmap_data_1[80:100, ]

heatmap_data <- list(heatmap_1 = heatmap_data_1,
                     heatmap_2 = heatmap_data_2,
                     heatmap_3 = heatmap_data_3)

gvmap(legend_data = legend_data,
      heatmap_data = heatmap_data,
      config_file = config_file,
      output_svg_name = "tests/o1.svg",
      sample_span = 20,
      heatmap_row_span = 10,
      output_group_info = T)

rsvg_pdf(svg = "tests/g3.min.svg", file = "tests/g3.min.pdf")

# ==================================
h1 <- read.table(file = "tests/g3.heatmap1.txt", sep = "\t")
h2 <- read.table(file = "tests/g3.heatmap2.txt", sep = "\t")
sampleinfo <- read.table( file = "tests/g3.sampleinfo.txt", sep = "\t")
mut <- read.table( file = "tests/g3.mutation.txt", sep = "\t")

mun_tag <- rep(0, length(mut))
for (i in 1:length(mut)) {
  mun_tag[i] <- length(which(mut[, i] == "0"))
}
plot_mut_mat <- mut[, (mun_tag < 159)]

data_plot_info <- cbind(sampleinfo, plot_mut_mat)
row.names(data_plot_info) <- data_plot_info$x


h11 <- h1[, 1:100]
h2 <- h2[1:120, ]

heatmap_data <- list(heatmap_1 = h1)

legend_data <- data_plot_info

output_svg_name <- "tests/g3.svg"
config_file <- "tests/config.g3.yaml"

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


gvmap(legend_data = legend_data,
      heatmap_data = heatmap_data,
      config_file = config_file,
      output_svg_name = output_svg_name,
      dend_stroke_width = 1,
      convert_pdf = TRUE)

# test on server

sample_info <- read.table("ball.sampleinfo.txt", sep = "\t", header = T, comment.char = "!")
mutation_info <- read.table("mutation_matrix.txt", sep = "\t", header = T, comment.char = "!")
exp_data <- read.csv("datExpAfterSVA1285.csv", header = T)

dim(sample_info)
dim(mutation_info)
dim(exp_data)

row.names(exp_data) <- exp_data[, 1]
exp_data <- exp_data[, -1]


filygenes <- function(exp)
{
  geninfo <- read.table("geneinfo/geneloc.txt")
  fil <- geninfo[,1] == "chrY"
  ygenes <- geninfo[fil,4]
  fil <- row.names(exp) %in% ygenes
  exp <- exp[!fil,]
  return(exp)
}

filxgenes <- function(exp)
{
  geninfo <- read.table("geneinfo/geneloc.txt")
  fil <- geninfo[,1] == "chrX"
  ygenes <- geninfo[fil,4]
  fil <- row.names(exp) %in% ygenes
  exp <- exp[!fil,]
  return(exp)
}

blackgenes <- read.table("heatmap_blackgenes.txt")
dat.tmp <- exp_data[!row.names(exp_data) %in% blackgenes[,1], ]

fil <- str_detect(row.names(dat.tmp), fixed("."))
dat.tmp <- dat.tmp[!fil,]
fil <- str_detect(row.names(dat.tmp), "^MIR|^RN7|^RNA|^SNOR|^LINC|^TRAJ|^RNY")
dat.tmp <- dat.tmp[!fil,]
dat.tmp <- filygenes(dat.tmp)
dat.tmp <- filxgenes(dat.tmp)

exp_data <- dat.tmp

exp_data$gene_var <- gene_var

# ==

library(configr)
library(dendextend)
library(matrixStats)
library(easySVG)
library(rsvg, lib.loc = "/home/dyt/R/x86_64-pc-linux-gnu-library/3.4")
library(stringr)
library(gvmap, lib.loc = "/home/dyt/R/x86_64-pc-linux-gnu-library/3.4")

sample_info <- read.table("ball.sampleinfo.txt", sep = "\t", header = T, comment.char = "!")
mutation_info <- read.table("mutation_matrix.txt", sep = "\t", header = T, comment.char = "!")
exp_data <- read.table("filter_exp_mat.txt", header = T, sep = "\t")

exp_data <- as.matrix(exp_data)

sel_sample <- data.frame(sample = colnames(exp_data))


gene_var <- rowVars(exp_data)

gene_var_data <- data.frame(index = c(1:length(gene_var)), var = gene_var)

exp_data_sort <- exp_data[gene_var_data$index[order(gene_var_data$var, decreasing = T)], ]
sig_exp_data <- head(exp_data_sort, floor(length(gene_var)*0.1))

heatmap_data <- list(heatmap_1 = sig_exp_data)

legend_data_1 <- merge(sel_sample, sample_info, by.x = "sample", by.y = "nid", sort = F, all.x = T)
row.names(legend_data_1) <- legend_data_1$sample

mun_tag <- rep(0, length(mutation_info))
for (i in 1:length(mutation_info)) {
  mun_tag[i] <- length(which(mutation_info[, i] == "0"))
}
table(mun_tag)

mut_tag_order <- data.frame(idx = 1:length(mun_tag),
                            num = mun_tag)

mut_tag_order <- mut_tag_order[order(mut_tag_order$num), ]
mutation_info_order <- mutation_info[, mut_tag_order$idx]

plot_mut_mat <- mutation_info[, mut_tag_order$idx[(mut_tag_order$num < 1311)]]

#plot_mut_mat <- mutation_info[, mut_tag_order$idx[(mut_tag_order$num < 1321) & (mut_tag_order$num >= 1311)]]

#plot_mut_mat <- mutation_info[, mut_tag_order$idx[(mut_tag_order$num < 1323) & (mut_tag_order$num >= 1321)]]

mut_merge <- merge(sel_sample, plot_mut_mat, by.x = "sample", by.y = 0, sort = F, all.x = T)


legend_data <- cbind(legend_data_1, mut_merge)

config_file <- "config.allsample.yaml"
config_file <- "config.allsample.mut.yaml"

gvmap(legend_data = legend_data,
      heatmap_data = heatmap_data,
      config_file = config_file,
      output_svg_name = "output_all_sample_kmer.1323.svg",
      stroke_width = 0.1,
      dend_stroke_width = 1,
      frame_stroke_width = 1,
      sample_span = 2,
      sample_font_size = 0.6,
      output_group_info = T,
      convert_jpg = T,
      plot_height = 2000)

# single group

oo = 4

for (oo in 1:9) {
  if (oo == 9) {
    oo = "9.10"
  }

g1_name <- read.csv(paste0("groupnames/g", oo, ".csv"))

sel_exp_data <- exp_data[, match(g1_name$x, colnames(exp_data))]
gene_var <- rowVars(sel_exp_data)

gene_var_data <- data.frame(index = c(1:length(gene_var)), var = gene_var)

sel_exp_data_sort <- sel_exp_data[gene_var_data$index[order(gene_var_data$var, decreasing = T)], ]
sel_sig_exp_data <- head(sel_exp_data_sort, floor(length(gene_var)*0.05))

heatmap_data <- list(heatmap_1 = sel_sig_exp_data)

legend_data_1 <- merge(g1_name, sample_info, by.x = "x", by.y = "nid", sort = F, all.x = T)
row.names(legend_data_1) <- legend_data_1$sample

#plot_mut_mat <- mutation_info[, mut_tag_order$idx[(mut_tag_order$num < 1321) & (mut_tag_order$num >= 1311)]]

#plot_mut_mat <- mutation_info[, mut_tag_order$idx[(mut_tag_order$num < 1323) & (mut_tag_order$num >= 1321)]]

mut_merge <- merge(g1_name, mutation_info, by.x = "x", by.y = 0, sort = F, all.x = T)
row.names(mut_merge) <- mut_merge$x

mun_tag <- rep(0, length(mut_merge))
for (i in 1:length(mut_merge)) {
  mun_tag[i] <- length(which(mut_merge[, i] == "0"))
}
table(mun_tag)

mut_tag_order <- data.frame(idx = 1:length(mun_tag),
                            num = mun_tag)

mut_tag_order <- mut_tag_order[order(mut_tag_order$num), ]
mut_merge_order <- mut_merge[, mut_tag_order$idx]

plot_mut_mat <- mut_merge[, mut_tag_order$idx[(mut_tag_order$num < 358)]]

head(plot_mut_mat)

legend_data <- cbind(legend_data_1, mut_merge_order)

config_file <- paste0("groupnames/config.g", oo, ".yaml")

gvmap(legend_data = legend_data,
      heatmap_data = heatmap_data,
      config_file = config_file,
      output_svg_name = paste0("groupnames/figure.0.05.g", oo, ".svg"),
      stroke_width = 0.5,
      dend_stroke_width = 1,
      frame_stroke_width = 1,
      sample_span = 2,
      convert_jpg = T)

}





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
sel <- sig_gene_data[1:floor(length(sig_gene_data[ , 1]) * 0.1), ]

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

write.table(heatmap_1, file = "tests/g3.heatmap1.txt", sep = "\t", col.names = T, row.names = T, quote = F)
write.table(heatmap_2, file = "tests/g3.heatmap2.txt", sep = "\t", col.names = T, row.names = T, quote = F)
write.table(sample_order_merge, file = "tests/g3.sampleinfo.txt", sep = "\t", col.names = T, row.names = T, quote = F)
write.table(mut_mat, file = "tests/g3.mutation.txt", sep = "\t", col.names = T, row.names = T, quote = F)


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

heatmap.2(as.matrix(h0), scale = "col",
          col = colorRampPalette(c("green", "black", "red"))(400),
          dendrogram = 'both', density.info = "none",
          trace = "none", cexRow = 0.5)

heatmap.2(as.matrix(h00), scale = "col",
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




# ====================
sample_info <- read.table("ball.sampleinfo.txt", sep = "\t", header = T, comment.char = "!")
samples <- sample_info$nid
mutation_info <- read.table("mutation.txt", sep = "\t" , header = T)

gene_list <- unique(mutation_info$gene)
mut_mat <- data.frame(matrix(0, nrow = length(samples), ncol = length(gene_list)))

colnames(mut_mat) <- gene_list
rownames(mut_mat) <- samples

for (i in 1:length(mut_mat)) {
  gene_name <- gene_list[i]
  mut_gene <- mutation_info[which(mutation_info$gene == gene_list[i]), ]
  mut_sam <- unique(mut_gene$sample)
  for (j in 1:length(mut_sam)) {
    mm <- mut_gene[which(mut_gene$sample == as.character(mut_sam[j])), ]
    mm_info <- paste(mm$class, collapse = "/")
    sam_pos <- which(samples == as.character(mut_sam[j]))
    mut_mat[sam_pos, i] <- mm_info
  }
  if (i %% 100 == 0) {
    message(i)
  }
}

write.table(mut_mat, "mutation_matrix.txt", sep = "\t", col.names = T, row.names = T, quote = F)

mun_tag <- rep(0, length(mut_mat))
for (i in 1:length(mut_mat)) {
  mun_tag[i] <- length(which(mut_mat[, i] == "0"))
}




