# This script contains worked examples running the Binary K-means
# clustering algorithm on simulated and experimental time-intensity
# traces.

# Load necessary packages.
library(tidyr)
library(dplyr)
library(purrr)
library(ggplot2)

# Load functions from repository.
source("functions/helpers.R")
source("functions/preprocessing.R")
source("functions/main.R")

# -------------- #
# Simulated Data #
# -------------- #

# Create a simulated time-intensity trace of 1000 data points.
simulated_trace = simulate_trace(n = 1000, 
                                 sig_noise_prop = 1, 
                                 step_prob = 0.01)

# Visualize the raw trace.
plot(simulated_trace$data, type = "l")

# Apply the pipe() function with compression of 10%.
clustered_simulated_trace = pipe(simulated_trace$data, compression = 0.075)

# Visualize the resultant clustering.
cluster_plot(clustered_simulated_trace, "Simulated Trace Clustering")

# Compare the predicted and actual number of steps.
paste("Predicted steps:", nrow(clustered_simulated_trace) - 1)
paste("Actual steps:", simulated_trace$steps)

# ----------------- #
# Experimental Data #
# ----------------- #

# Read in an example trace (just time and intensity columns).
experimental_trace = read.table("sample_data_clean/tr12.dat") %>%
  select(1, 2)

# Visualize the raw trace.
plot(experimental_trace, type = "l")

# Apply the pipe() function with compression of 15%.
clustered_experimental_trace = pipe(experimental_trace, compression = 0.15)

# Visualize the resultant clustering.
cluster_plot(clustered_experimental_trace, "Experimental Trace Clustering")

# Compare the predicted and actual number of steps.
paste("Predicted steps:", nrow(clustered_experimental_trace) - 1)
paste("Actual steps:", 5)
