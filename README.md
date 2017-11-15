

gvmap package
==============

An Improved Function to Plot Heatmap of Genomic Data

# DESCRIPTION

Gvmap is an R package that can integrate multiple heatmap and legend figures. 

# INSTALLATION

Gvmap depends on several R packages. They are 
[configr](https://cran.r-project.org/web/packages/configr/index.html), 
[dendextend](https://cran.r-project.org/web/packages/dendextend/index.html), 
[matrixStats](https://cran.r-project.org/web/packages/matrixStats/index.html), 
[easySVG](https://github.com/ytdai/easySVG), 
[rsvg](https://cran.r-project.org/web/packages/rsvg/index.html), and 
[stringr](https://cran.r-project.org/web/packages/stringr/index.html). 

You can install gvmap through github or source code.


## Github

``` r
# Install the cutting edge development version from GitHub:
# install.packages("devtools")

# install easySVG
devtools::install_github("ytdai/easySVG")

# install other packages
install.packages("configr")
install.packages("dendextend")
install.packages("matrixStats")
install.packages("rsvg")
install.packages("stringr")

devtools::install_github("ytdai/gvmap")
```

## Zip/Tarball

1. Download the appropriate zip file or tar.gz file from Github
2. Unzip the file and change directories into the easySVG directory
3. Run `R CMD INSTALL pkg`


# Plot

Run by default paramter

![](https://github.com/ytdai/gvmap/tree/master/vignettes/o1.svg)

Only one heatmap

![](https://github.com/ytdai/gvmap/tree/master/vignettes/o2.svg)

Only legend

![](https://github.com/ytdai/gvmap/tree/master/vignettes/o3.svg)

Add sample span and heatmap span

![](https://github.com/ytdai/gvmap/tree/master/vignettes/o4.svg)














