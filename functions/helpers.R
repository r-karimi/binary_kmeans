# This file contains all helper functions used by the main functions
# of the Binary K-means repository.

## shapiro_helper
# Arguments:
  # df: A data frame of time-intensity tuples.
# Returns:
  # A number reflecting the outcome of the Shapiro test
# Description:
  # The shapiro_helper function takes a data frame (the time-intensity
  # tuples of a cluster) and returns a number reflecting the outcome of the
  # Shapiro-Wilk test for normality on the intensity values. If the cluster
  # contains less than three and greater than 4999 time-intensity tuples,
  # the number zero is returned. If the size is greater than 4999, the number
  # 0 is returned. If the size is less than 3, the number 1 is returned.

shapiro_helper = function(df){
  if(nrow(df) >= 3 && nrow(df) < 5000){
    return(shapiro.test(df$intensity)$p.value) 
    # Apply the Shapiro-Wilk test and return the p-value.
  } 
  if (nrow(df) >= 5000) {
    return(0) # Return 0 (mark for reclustering) if n >= 5000 points.
  } 
  else {
    return(1) # Don't recluster if n < 3 points.
  }
}

## kmeans_helper
# Arguments:
  # df: A data frame of time-intensity tuples.
# Returns:
  # A kmeans-type object with k = 2.
# Description:
  # A brief helper function to take a data frame, isolate the time and the
  # intensity values, and perform a binary segmentation with the k-means
  # clustering algorithm.

kmeans_helper = function(df){
  df[,c(1,2)] %>% # Isolate time and intensity 
    kmeans(centers = 2) %>% # Perform binary segmentation
    return()
}

## min_time_helper
# Arguments:
  # df: A data frame of time-intensity tuples.
# Returns:
  # A number, the smallest value presented in the first (time) column of df.
# Description:
  # A brief helper function to take a data frame and return the smallest
  # time presented in the time column of df.

min_time_helper = function(df){
  return(min(df[,1]))
}

## median_helper
# Arguments:
  # df: A data frame of time-intensity tuples.
# Returns:
  # The median value of the intensity column of df.
# Description:
  # A brief helper function to return the median value of the intensity
  # column of df.

median_helper = function(df){
  median(df$intensity) %>% 
    return()
}

## sd_helper
# Arguments:
  # df: A data frame of time-intensity tuples.
# Returns:
  # The standard deviation of the intensity column of df, if there are
  # more than two time-intensity tuples in df.
# Description:
  # A brief helper function to return the standard deviation of 
  # the intensities of a set of time-intensity tuples.

sd_helper = function(df){
  if(length(df$intensity) > 2){
    sd(df$intensity) %>% return()
  } else {
    return(0)
  }
}

## merge_helper
# Arguments:  
  # df1: the first data frame of time-intensity tuples from the first cluster.
  # df2: the second data frame of time-intensity tuples from the second cluster.
  # cluster_number: the cluster number to be assigned to the merged cluster.
# Returns:
  # A merged cluster containing the time-intensity tuples from the clusters
  # fed into the merge, as well as some cluster statistics for the merged
  # cluster. 
# Description:
  # A helper function to merge two clusters that were previously seperated
  # by a round of binary segmentation.

merge_helper = function(df1, df2, cluster_number){
  min_time = min(df1[,1]) # Find the minimum time associated with the new cluster.
  combined_data = bind_rows(df1, df2) %>% 
    nest(data = everything()) # Combine the time-intensity tuples from df1 and df2 and merge.
  merged_clusters = cbind(.cluster = cluster_number, 
                          med = median(combined_data[[1]][[1]]$intensity), # Find the median intensity of the new cluster
                          shap = 1, # Do not recluster the combined cluster
                          reclust = F, # Do not mark for reclustering
                          min_time = min_time,
                          data = combined_data) 
  
  return(merged_clusters)
}

## breakpoint_helper
# Arguments:  
  # df: A data frame of time-intensity tuples.
  # max_cluster: The number assigned to the cluster before intra-cluster resolution of level sets.
# Returns:
  # A nested data frame of new clusters and the corresponding cluster statistics.
# Description:
  # A helper function to assist in intra-cluster resolution of transient
  # steps, by examining the first and last 20% of tuples in a given
  # cluster. The function computes the lagged differences (an estimate of the
  # derivative) between data points, and looks for regions where the
  # lagged difference exceeds 3*sd of the middle 60% of the cluster.

breakpoint_helper = function(df, max_cluster){
  L = length(df$intensity) # Define a helper variable L for future ease of notation.
  
  first_fifth = diff(df$intensity[1:(0.2*round(L))])
  last_fifth = diff(df$intensity[(0.8*round(L)):L])
  middle = diff(df$intensity[(0.2*round(L)):(0.8*round(L))])
  
  # Find indices of breakpoints in the signal.
  first_breaks =  which(abs(first_fifth - mean(middle)) >=  3*sd(middle))
  last_breaks = which(abs(last_fifth - mean(middle)) >=  3*sd(middle))
  
  # Make a list of indices of breakpoint locations.
  breakpoints = c(first_breaks, last_breaks + round(0.8*L))
  
  # Loop through the breakpoints and update the cluster number at every breakpoint.
  for(i in breakpoints){
    df$.cluster[i:L] = min(df$.cluster[i:L]) + 1
  }
  
  df$.cluster = df$.cluster + max_cluster
  
  # Put the data frame back into the cluster format with necessary statistics.
  df %>% 
    select(time, intensity, .cluster) %>%
    group_by(.cluster) %>% 
    nest() %>% 
    mutate(med = map(data, median_helper), shap = map(data, shapiro_helper), reclust = F, min_time = map(data, min_time_helper)) %>% 
    return()
}

## cluster_plot
# Arguments:  
  # clusters: A list of predicted clusters from a raw time-intensity table,
  # as generated with `pipe()`.
  # name: The title of the resultant plot.
# Returns:
  # A ggplot2 graph object that can be called to display a plot.
# Description:
  # cluster_plot is a helper function designed to visualize clustering outcomes
  # from the Binary K-means algorithm, as applied with the pipe() function. 

cluster_plot = function(clusters, name){
  # Pull out time, intensity, and cluster numbers from clusters object.
  plotR = clusters %>% unnest(cols = c(data))
  
  # Construct the plot.
  processed_plot = ggplot(data = plotR, aes(x = time, y = intensity, colour = factor(.cluster))) +
    geom_point(size = 0.75, alpha = 1) +
    geom_line() +
    labs(x = "Time (s)", 
         y = "Intensity (a.u.)", 
         colour = "Cluster",
         title = name) +
    theme_classic()
  
  # Return the plot.
  return(processed_plot)
}

## simulate_trace
# Arguments:  
  # n: A number, the number of time-intensity tuples contained in 
  # the resultant simulated signal.
  # sig_noise_prop: A decimal, the proportionality factor between the square root
  # of the signal intensity of a given cluster. Set to 1 for Poissonian-like noise, 
  # increase and decrease to afford simulated signals of various S/N.
  # step_prob: A decimal, the decimal probability that a given timepoint 
  # contains a changepoint.
# Returns:
  # A list containing the time-intensity tuples of simulated traces, and the number
  # of steps in the simulated signal.
# Description:
  # simulate_trace is a function to assist in assessing Binary K-means at various S/N
  # by generating simulated traces containing stochastic step events, masked by
  # Gaussian noise. The step heights have an underlying uniform value masked
  # by Gaussian noise.

simulate_trace = function(n, sig_noise_prop, step_prob = 0.05){
  # Generate a pseudorandom vector of length n.
  randoms = runif(n, min = 0, max = 1)
  
  # Check which locations will contain steps, and
  # demarkate them with the number 1.
  randoms[which(randoms >= (1 - step_prob))] = 1
  randoms[which(randoms < (1 - step_prob))] = 0
  
  # Sum over the randoms vector to figure out
  # how many steps are contained by the simulated trace.
  n_steps = sum(randoms)
  
  # Construct the simulated trace backward by initializing a 
  # placeholder vector.
  signalI = c()
  
  # Assign a starting signal amplitude of 0.
  signalAmp = 0
  
  # Make the entire signal, adding 100 to the signal amplitude
  # when a step location is encountered.
  for (i in 1:n) {
    if (randoms[i] == 0) {
      signalI[[i]] = signalAmp
    } 
    if (randoms[i] == 1) {
      signalAmp = signalAmp + 100
      signalI[[i]] = signalAmp
    }
  }
  
  # Reverse the signal for a piecewise constant
  # decreasing signal.
  signalI = rev(signalI)
  
  # Add a buffer of 0-valued signal at the end.
  signalI = c(signalI, rep(0, 0.5*n))
  
  # Add Gaussian noise to signalI location-by-location. If the
  # signal magnitude is 0, add Gaussian noise of constant s.d.,
  # otherwise add noise proportional to the square root of the signal
  # by sig_noise_prop.
  noisySig = c()
  for (i in 1:(1.5*n)) {
    if (signalI[i] != 0) {
      noisySig[[i]] = signalI[i] + rnorm(1, 0, sig_noise_prop*sqrt(signalI[i]))
    } 
    else {
      noisySig[[i]] = signalI[i] + rnorm(1, 0, sig_noise_prop*10)
    }
  }
  
  # Return a list containing the simulated trace and the number of steps,
  # as well as simulated timepoints (0 + 0.1*n).
  return(list(data = data.frame(time = c((1:(1.5*n))/10), 
                                intensity = noisySig), 
              steps = n_steps))
}
