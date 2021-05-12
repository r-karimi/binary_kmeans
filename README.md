# binary_kmeans
A respository housing the code to run the Binary K-means step detection algorithm, designed to detect changepoints within piecewise constant signals in linear time complexity.

# Installation Guide

To use this software, clone the GitHub repository onto your local filesystem. Then, ensure your working directory is the base directory of this repository. Open the script `example.R` in an R IDE (RStudio recommended). Ensure that the necessary packages are installed, and follow `example.R` to use the `source()` package to import all functions in `preprocessing.R`, `main.R`, and  `helpers.R`. Import your raw data, and apply `pipe(data, compression)` on your imported dataset with a chosen compression factor (usually 0.1 works okay, varying from 0.2 to 0.05, heuristically).