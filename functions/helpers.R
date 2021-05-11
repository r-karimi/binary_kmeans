# This file contains all helper functions used by the main functions
# of the Binary K-means repository.


## shapiro_helper
# Arguments:
  # df: A data frame of time-intensity tuples.
# Returns:
  # A number reflecting the outcome of the Shapiro test
# Description:
  # The shapiro_helper function takes a data frame (usually the time-intensity
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
    return(0)
  } else {
    return(1)
  }
}




kmeans_helper = function(df){
  df[,c(1,2)] %>% kmeans(centers = 2) %>% return()
}

min_time_helper = function(df){
  return(min(df[,1]))
}

median_helper = function(df){
  median(df$intensity) %>% return()
}

sd_helper = function(df){
  if(length(df$intensity) > 2){
    sd(df$intensity) %>% return()
  }
  else{
    return(0)
  }
}

merge_helper = function(df1, df2, cluster_number){
  
  min_time = min(df1[,1])
  
  a = bind_rows(df1, df2) %>% 
    nest(data = everything())
  
  b = cbind(.cluster = cluster_number, med = median(a[[1]][[1]]$intensity), shap = 1, reclust = F, min_time = min_time, data = a) 
  
  return(b)
  
}

breakpoint_helper = function(df, max_cluster){
  
  # look at the first and last fifth of the signal for breakpoints
  L = length(df$intensity)
  
  first_fifth = diff(df$intensity[1:(0.2*round(L))])
  last_fifth = diff(df$intensity[(0.8*round(L)):L])
  middle = diff(df$intensity[(0.2*round(L)):(0.8*round(L))])
  
  first_breaks =  which(abs(first_fifth - mean(middle)) >=  3*sd(middle))
  
  last_breaks = which(abs(last_fifth - mean(middle)) >=  3*sd(middle))
  
  breakpoints = c(first_breaks, last_breaks + round(0.8*L))
  
  for(i in breakpoints){
    df$.cluster[i:L] = min(df$.cluster[i:L]) + 1
  }
  
  df$.cluster = df$.cluster + max_cluster
  
  df %>% 
    select(time, intensity, .cluster) %>%
    group_by(.cluster) %>% 
    nest() %>% 
    mutate(med = map(data, median_helper), shap = map(data, shapiro_helper), reclust = F, min_time = map(data, min_time_helper)) %>% 
    return()
  
}
