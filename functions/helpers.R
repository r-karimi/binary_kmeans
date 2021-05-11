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