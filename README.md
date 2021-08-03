# binary_kmeans
A respository housing the code to run the Binary K-means step detection algorithm, designed to detect changepoints/steps within piecewise constant signals in linear time complexity.

# Installation and Usage Guide
- Clone the GitHub repository onto your local filesystem through RStudio's version checkout feature, by navigating to `File > New Project... > Version Control > Git` and pasting in the URL of this repository (https://github.com/r-karimi/binary_kmeans.git).
- Open the project file `binary_kmeans.Rproj` in RStudio, if this is not done automatically.
- Ensure your working directory is the base directory of this repository. 
- Open the script `single_example.R`. Ensure that the necessary packages are installed, and follow `single_example.R`, using the `source()` function to import all functions in `preprocessing.R`, `main.R`, and  `helpers.R`. 
- Import your raw data using `read.table()`, and apply `pipe(data, compression)` on your imported dataset with a chosen compression factor (0.1 usually works, but you can try varying from 0.2 to 0.05 for optimization, depending on whether overfitting or underfitting occurs).
- Call `nrow(clusters) - 1` on the resulting cluster object to print the predicted number of steps.
- Call `cluster_plot(clusters)` on the resulting cluster object to visualize the clustering.

All in all, the installation should take ~ 10 minutes, with the majority of the time alloted to downloading necessary packages. The analysis in the `single_example.R` script should run in less than a minute, with each call to `pipe()` running on the order of seconds.

# Tested Software Environments
This software was developed and tested in the following R environment, and all packages below must be present before running the code in this repository:

```
R version 3.6.0 (2019-04-26)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Mojave 10.14.6

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets 
[6] methods   base     

other attached packages:
[1] ggplot2_3.3.0 purrr_0.3.3   dplyr_0.8.5   tidyr_1.0.2  

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.4.6     withr_2.1.2      packrat_0.5.0   
 [4] crayon_1.3.4     assertthat_0.2.1 grid_3.6.0      
 [7] R6_2.4.1         gtable_0.3.0     lifecycle_0.2.0 
[10] magrittr_1.5     scales_1.1.0     pillar_1.4.3    
[13] rlang_0.4.5      rstudioapi_0.11  vctrs_0.2.4     
[16] tools_3.6.0      glue_1.4.0       munsell_0.5.0   
[19] compiler_3.6.0   colorspace_1.4-1 pkgconfig_2.0.3 
[22] tidyselect_1.0.0 tibble_2.1.3
```
