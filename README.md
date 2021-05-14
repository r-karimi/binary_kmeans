# binary_kmeans
A respository housing the code to run the Binary K-means step detection algorithm, designed to detect changepoints/steps within piecewise constant signals in linear time complexity.

# Installation and Usage Guide
- Clone the GitHub repository onto your local filesystem through RStudio's version checkout feature, by navigating to `File > New Project... > Version Control > Git` and pasting in the URL of this repository (https://github.com/r-karimi/binary_kmeans.git).
- Open the project file `binary_kmeans.Rproj` in RStudio, if this is not done automatically.
- Ensure your working directory is the base directory of this repository. 
- Open the script `single_example.R`. Ensure that the necessary packages are installed, and follow `single_example.R` to use the `source()` package to import all functions in `preprocessing.R`, `main.R`, and  `helpers.R`. 
- Import your raw data using `read.table()`, and apply `pipe(data, compression)` on your imported dataset with a chosen compression factor (0.1 usually works, but you can try varying from 0.2 to 0.05 for optimization, depending on whether overfitting or underfitting occurs).
- Call `nrow(clusters) - 1` on the resulting cluster object to print the predicted number of steps.
- Call `cluster_plot(clusters)` on the resulting cluster object to visualize the clustering.