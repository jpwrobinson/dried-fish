# Dried fish provide widespread access to critical nutrients across sub-Saharan Africa
> To download, click the green Code button, then either `git clone` or download the ZIP.

This repository contains data and code needed to reproduce **Robinson et al.** Dried fish provide widespread access to critical nutrients across sub-Saharan Africa. [YEAR] *[JOURNAL]* [URL]



All analyses require `R`, and the entire project can be compiled using the `targets` package. In [R Studio](https://posit.co/download/rstudio-desktop/), open the [R project file](dried-fish.Rproj) and then [_targets.R](_targets.R) and run `tar_make()`. This will load and clean nutrient data and household surveys, extract water and urban proximity covariates, and load statistical model objects, and reproduce all figures as pdfs.

Note that statistical models must be re-run outside the targets workflow, using [mod_consumption.R](scripts/mod_consumption.R). You will need [brms](https://paulbuerkner.com/brms/) and [RStan](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started) installed on your machine before doing this.

The proximity calculations can take up to 4 hours on a desktop machine.


### Software and packages

```
R version 4.3.3 (2024-02-29)
Platform: x86_64-apple-darwin20 (64-bit)
Running under: macOS 15.5

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.3-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/London
tzcode source: internal

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] emmeans_1.10.1      FAOSTAT_2.4.0       WDI_2.7.8           conflicted_1.2.0    rethinking_2.13.2   brms_2.20.4        
 [7] Rcpp_1.0.12         raster_3.6-26       sp_2.1-2            tmap_3.3-4          rnaturalearth_1.0.1 sf_1.0-15          
[13] slider_0.3.1        scales_1.3.0        ggExtra_0.10.1      ggradar_0.2         modelr_0.1.11       bayesplot_1.11.0   
[19] tidybayes_3.0.6     cowplot_1.1.2       countrycode_1.5.0   janitor_2.2.0       readxl_1.4.3        lubridate_1.9.3    
[25] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4         purrr_1.0.2         readr_2.1.5         tidyr_1.3.0        
[31] tibble_3.2.1        ggplot2_3.5.0       tidyverse_2.0.0     targets_1.5.1      

loaded via a namespace (and not attached):
  [1] svUnit_1.0.6         shinythemes_1.2.0    later_1.3.2          cellranger_1.1.0     xts_0.13.2           XML_3.99-0.16.1     
  [7] lifecycle_1.0.4      StanHeaders_2.32.5   processx_3.8.3       lattice_0.22-5       MASS_7.3-60.0.1      crosstalk_1.2.1     
 [13] ggdist_3.3.2         backports_1.4.1      magrittr_2.0.3       yaml_2.3.8           httpuv_1.6.13        pkgbuild_1.4.3      
 [19] DBI_1.2.0            RColorBrewer_1.1-3   abind_1.4-5          tensorA_0.36.2.1     inline_0.3.19        terra_1.7-71        
 [25] units_0.8-5          bridgesampling_1.1-2 codetools_0.2-19     DT_0.31              tidyselect_1.2.1     shape_1.4.6         
 [31] matrixStats_1.2.0    stats4_4.3.3         base64enc_0.1-3      jsonlite_1.8.8       e1071_1.7-14         ellipsis_0.3.2      
 [37] tools_4.3.3          glue_1.8.0           gridExtra_2.3        xfun_0.41            distributional_0.4.0 loo_2.6.0           
 [43] withr_3.0.2          fastmap_1.1.1        shinyjs_2.1.0        callr_3.7.5          digest_0.6.34        secretbase_0.3.0    
 [49] timechange_0.2.0     R6_2.6.1             mime_0.12            estimability_1.5     colorspace_2.1-1     gtools_3.9.5        
 [55] dichromat_2.0-0.1    markdown_1.12        threejs_0.3.3        generics_0.1.4       data.table_1.15.4    class_7.3-22        
 [61] httr_1.4.7           htmlwidgets_1.6.4    tmaptools_3.1-1      RJSONIO_1.3-1.9      pkgconfig_2.0.3      dygraphs_1.1.1.6    
 [67] gtable_0.3.4         htmltools_0.5.7      base64url_1.4        png_0.1-8            posterior_1.5.0      snakecase_0.11.1    
 [73] knitr_1.45           rstudioapi_0.15.0    tzdb_0.4.0           reshape2_1.4.4       coda_0.19-4.1        checkmate_2.3.1     
 [79] nlme_3.1-164         curl_5.2.0           proxy_0.4-27         zoo_1.8-12           cachem_1.0.8         KernSmooth_2.23-22  
 [85] miniUI_0.1.1.1       warp_0.2.1           leafsync_0.1.0       pillar_1.10.2        grid_4.3.3           vctrs_0.6.5         
 [91] shinystan_2.6.0      promises_1.2.1       arrayhelpers_1.1-0   xtable_1.8-4         mvtnorm_1.2-4        cli_3.6.5           
 [97] compiler_4.3.3       rlang_1.1.6          rstantools_2.3.1.1   labeling_0.4.3       classInt_0.4-10      ps_1.7.6            
[103] plyr_1.8.9           stringi_1.8.3        rstan_2.32.6         viridisLite_0.4.2    QuickJSR_1.0.9       stars_0.6-4         
[109] munsell_0.5.1        leaflet_2.2.1        colourpicker_1.3.0   Brobdingnag_1.2-9    V8_4.4.1             Matrix_1.6-5        
[115] hms_1.1.3            leafem_0.2.3         shiny_1.8.0          igraph_2.0.1.1       broom_1.0.5          memoise_2.0.1       
[121] RcppParallel_5.1.7   lwgeom_0.2-14
```



