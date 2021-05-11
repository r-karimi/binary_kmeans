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
  } if (nrow(df) >= 5000){
    return(0) # Return 0 (mark for reclustering) if n >= 5000 points.
  } else {
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
