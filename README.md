# binary_kmeans
A respository housing the code to run the Binary K-means step detection algorithm, designed to detect changepoints within piecewise constant signals in linear time complexity.

# Installation and Usage Guide
- Clone the GitHub repository onto your local filesystem. 
- Ensure your working directory is the base directory of this repository. 
- Open the script `single_example.R` in an R IDE (RStudio recommended). Ensure that the necessary packages are installed, and follow `single_example.R` to use the `source()` package to import all functions in `preprocessing.R`, `main.R`, and  `helpers.R`. 
- Import your raw data using `read.table()`, and apply `pipe(data, compression)` on your imported dataset with a chosen compression factor (usually 0.1 works okay, varying from 0.2 to 0.05, heuristically).
- Call `length(clusters) - 1` on the resulting cluster object to print the predicted number of steps.
- Call `cluster_plot(clusters)` on the resulting cluster object to visualize the clustering.
- Follow the steps in `batch_example.R` to iterate over a folder of raw traces and visualize population statistics from the clustering outputs.