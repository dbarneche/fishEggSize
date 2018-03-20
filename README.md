# Global environmental drivers of marine fish egg size

This repository contains code and data needed to reproduce the article:

**Barneche DR, Burgess SC, Marshall DJ** (in press) Global environmental drivers of marine fish egg size. *Global Ecology and Biogeography*. doi: 10.1111/geb.12748

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1204618.svg)](https://doi.org/10.5281/zenodo.1204618)

## Instructions

All analyses were done in `R`. To compile the paper, including figures and tables we use the [remake](https://github.com/richfitz/remake) package for R. You can install remake using the `devtools` package:

```r
devtools::install_github('richfitz/remake', dependencies = TRUE)
```
(run `install.packages('devtools')` to install devtools if needed.)

The `remake` package also depends on `storr`, install it like this:
```r
devtools::install_github('richfitz/storr', dependencies = TRUE)
```

Next you need to open an R session with working directory set to the root of the project.

We use a number of packages, missing packages can be easily installed by remake:

```r
remake::install_missing_packages()
```

And then install the package `fontcm`, via `extrafont`. This installs the font `CM Roman` we use in our figures (for more information on see [these instructions](https://cran.r-project.org/web/packages/fontcm/README.html):

```r
extrafont::font_install('fontcm')
```

Then, to generate all figures, analyses, and manuscript (using the package `knitr`), simply do:

```r
remake::make()
```

All output will be automatically placed in a directory called `output` (it is going to be automatically created for you).

Also notice that the whole project compilation, including environmental data download from the NOAA database and the Bayesian analyses, may take up to a week to run on a regular computer.

If you find remake confusing and prefer to run plain R, you can use remake to build a script `build.R` that produces a all outputs, e.g.

```r
remake::make_script(filename = 'build.R')
```

### The paper can be reproduced using the following software and associated packages:
```
R version 3.4.3 (2017-11-30)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: OS X El Capitan 10.11.6

Matrix products: default
BLAS: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.4/Resources/lib/libRlapack.dylib

locale:
[1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

attached base packages:
[1] tools     parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] rmarkdown_1.8         knitr_1.19            envPred_0.0.0.1000    raster_2.6-7          sp_1.2-7              zoo_1.8-1             plyr_1.8.4           
 [8] ncdf4_1.16            noaaErddap_0.0.0.9000 fontcm_1.1            extrafont_0.17        LoLinR_0.0.0.9000     Hmisc_4.1-1           Formula_1.2-2        
[15] survival_2.41-3       lattice_0.20-35       brms_2.1.0            ggplot2_2.2.1         Rcpp_0.12.15          rfishbase_2.1.2.4     ape_5.0              
[22] rotl_3.0.3           

loaded via a namespace (and not attached):
 [1] nlme_3.1-131         matrixStats_0.53.0   xts_0.10-1           RColorBrewer_1.1-2   progress_1.1.2       threejs_0.3.1        httr_1.3.1          
 [8] rprojroot_1.3-2      rstan_2.17.3         backports_1.1.2      R6_2.2.2             DT_0.4               rpart_4.1-11         lazyeval_0.2.1      
[15] colorspace_1.3-2     nnet_7.3-12          gridExtra_2.3        prettyunits_1.0.2    Brobdingnag_1.2-4    compiler_3.4.3       extrafontdb_1.0     
[22] htmlTable_1.11.2     shinyjs_1.0          colourpicker_1.0     scales_0.5.0         dygraphs_1.1.1.4     checkmate_1.8.5      lmtest_0.9-35       
[29] mvtnorm_1.0-7        stringr_1.2.0        digest_0.6.15        StanHeaders_2.17.2   foreign_0.8-69       rentrez_1.1.0        base64enc_0.1-3     
[36] pkgconfig_2.0.1      htmltools_0.3.6      htmlwidgets_1.0      rlang_0.1.6          rstudioapi_0.7       shiny_1.0.5          bindr_0.1           
[43] jsonlite_1.5         crosstalk_1.0.0      gtools_3.5.0         acepack_1.4.1        dplyr_0.7.4          inline_0.3.14        magrittr_1.5        
[50] loo_1.1.0            bayesplot_1.4.0      Matrix_1.2-12        munsell_0.4.3        abind_1.4-5          stringi_1.1.6        yaml_2.1.16         
[57] storr_1.1.3          grid_3.4.3           crayon_1.3.4         miniUI_0.1.1         rncl_0.8.2           splines_3.4.3        pillar_1.1.0        
[64] igraph_1.1.2         markdown_0.8         shinystan_2.4.0      reshape2_1.4.3       stats4_3.4.3         rstantools_1.4.0     XML_3.98-1.9        
[71] glue_1.2.0           evaluate_0.10.1      latticeExtra_0.6-28  data.table_1.10.4-3  httpuv_1.3.5         Rttf2pt1_1.3.5       gtable_0.2.0        
[78] purrr_0.2.4          tidyr_0.8.0          assertthat_0.2.0     mime_0.5             xtable_1.8-2         coda_0.19-1          rsconnect_0.8.5     
[85] tibble_1.4.2         shinythemes_1.1.1    bindrcpp_0.2         cluster_2.0.6        remake_0.3.0         bridgesampling_0.4-0
```

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the green button `clone or download` and then click on `Download ZIP`  

## Bug reporting
* Please [report any issues or bugs](https://github.com/dbarneche/fishEggSize/issues).
