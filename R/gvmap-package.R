#'
#' gvmap config package
#' show gvmap config information
#'
#' @description
#' [* PARAMETERS IN MAP_CONFIG *]
#'
#' ======= HEATMAP ========
#'
#' [param] raw_data TRUE or FALSE, determines whether to change the raw data to row z-data to
#' illustrate the heatmap.
#'
#' [param] kmer_col number, split the col data by kmer. ATTENTION: only heatmap_1 has this parameter
#'
#' [param] kmer_row number, split the row data by kmer.
#'
#' [param] split_row_name vector, the specific row name to split, eg. ["name1", "name2", "name3"]
#'
#' [param] percentage number, between 0 and 1, the total heatmap percentage of the whole canvas.
#'
#' [param] kmer_col_color character. Col data color theme of dendrogram.
#'
#' [param] kmer_row_color character. Row data color them of dendrogram.
#'
#' [param] dendrogram "both", "col", "row", "none". SEE \link[gplots]{heatmap.2}
#'
#' [param] color_theme character heatmap gradient colot theme.
#'
#' [param] Rowv TRUE or FALSE, determines if and how the row dendrogram should be reordered.	By default,
#' it is TRUE, which implies dendrogram is computed and reordered based on row means.
#' If NULL or FALSE, then no dendrogram is computed and no reordering is done. If a
#' dendrogram, then it is used "as-is", ie without any reordering. If a vector of integers,
#'  then dendrogram is computed and reordered based on the order of the vector.
#' SEE \link[gplots]{heatmap.2}
#'
#' [param] Colv TRUE or FALSE, determines if and how the column dendrogram should be reordered.	Has the options
#' as the Rowv argument above and additionally when x is a square matrix, Colv = "Rowv" means
#' that columns should be treated identically to the rows. SEE \link[gplots]{heatmap.2}
#'
#' [param] distfun character, function used to compute the distance (dissimilarity) between both rows
#' and columns. Defaults to dist.  Also, there are other several parameters in distfun, such as "euclidean",
#'  "maximum", "manhattan", "canberra", "binary" or "minkowski". You can also have a
#' user-defined function by inputing a function parameter through \link[gvmap]{gvmapModAttr}.
#' SEE \link[gplots]{heatmap.2}
#'
#' [param] hclustfun character, function used to compute the hierarchical clustering when Rowv or
#' Colv are not dendrograms. Defaults to hclust. Also, there are other several parameters in hclustfun,
#' such as ward.D, ward.D2, single, complete, average, mcquitty, median, centroid. You can also have a
#' user-defined function by inputing a function parameter through \link[gvmap]{gvmapModAttr}.
#'  SEE \link[gplots]{heatmap.2}
#'
#' ======= LEGEND ========
#'
#' the legend data config format is simple
#'
#' eg:
#'
#' ["Gender","binary_gender_col", 3]
#'
#' ["ETV6-RUNX1", "tag_col", 4]
#'
#' ["KRAS", "mutation_col", 7]
#'
#' The first element in the vector is legend data. It dose not the same as the colnames in legend data.
#' The second element in the vector is the color theme, you can choose the color from color config list.
#' The third element in the vector is the column number.
#'
#' ======= OTHER ========
#'
#' [param] heatmap_num the total heatmap number
#'
#' [param] legend_num the total legend number
#'
#' [param] map_order the order of all plot element
#'
#' [param] split_sample split sample name
#'
#' [* PARAMETERS IN COLOR_CONFIG *]
#'
#' [param] bg_col background color theme, other user-define color theme must start with 'bg_'.
#'
#' [param] tag_col target color theme, other user-define color theme must start with 'tag_'.
#'
#' @author
#' Dai Yuting \url{forlynna@sjtu.edu.cn}
#' @seealso
#' Useful links:
#'
#' \url{https://github.com/ytdai/gvmap}
#'
#' Report bugs at \url{https://github.com/ytdai/gvmap/issues}
#' @docType package
#' @name gvmap
#' @import rsvg configr dendextend matrixStats stringr easySVG stats openxlsx
#' @importFrom grDevices colorRampPalette
#' @importFrom utils read.table
#' @importFrom stats as.dendrogram as.dist cor dist hclust order.dendrogram reorder
#'
NULL


