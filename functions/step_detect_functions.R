# define the thresholding helper function

thresholding = function(vec, mean, std){
  return(abs(vec - mean) >=  5*std)
}

# write the function to find the window of the signal

find_window = function(data){
  
  # find the mean and std of the last 25 observations
  
  threshold = data[,2] %>% 
    map(rev) %>% 
    unlist(recursive = F) %>%
    as_tibble() %>%
    slice(1:25) %>% 
    summarise(mean = mean(value), std = sd(value))
  
  # check intensities and see if they are above the threshold. look for first changepoint
  
  data = data %>% 
    mutate(noise = thresholding(intensity, threshold$mean, threshold$std)) %>%
    slice(1:(sum(noise) + 10))
  
  data = data[,-c(3)]
  
  return(data)
}

pre_process = function(data){
  scale = max(data$time)/max(data$intensity)
  scaled_data = data %>% transmute(time = time, intensity = intensity*scale)
  list(data = scaled_data, scale = scale) %>% return()
}


shapiro_helper = function(df){
  if(nrow(df) >= 3 && nrow(df) < 5000){
    return(shapiro.test(df$intensity)$p.value)
  } 
  if (nrow(df) >= 5000){
    return(0)
  } 
  else {
    return(1)
  }
}

# write a new function, ad_helper, to test
# sampling of the Poisson distribution

# library("ADGofTest")
# 
# ad_helper = function(df){
#   if(sum(which(df$intensity < 0)) != 0){
#     return(1)
#   }
#   if(nrow(df) >= 3 && nrow(df) < 5000){
#     return(ad.test(df$intensity, ppois, lambda = median(df$intensity))$p.value %>% as.numeric())
#   } 
#   if (nrow(df) >= 5000){
#     return(0)
#   } 
#   else {
#     return(1)
#   }
# }

pre_clust = function(processed){
  processed %>% 
    mutate(.cluster = 1) %>% 
    group_by(.cluster) %>% 
    nest() %>% 
    mutate(shap = map(data, shapiro_helper)) %>%
    mutate(reclust = ifelse(shap <= 0.05, T, F)) %>%
    return()
}

kmeans_helper = function(df){
  df[,c(1,2)] %>% kmeans(centers = 2) %>% return()
}

cluster_paste = function(recluster_set, reclust_data){
  #extract the data columns
  for(i in 1:length(recluster_set)){
    if(i == 1){
      recluster_set[[i]] = tibble(time = recluster_set[[i]][[1]], intensity = recluster_set[[i]][[2]], .cluster = as.numeric(reclust_data[[i]][["cluster"]]))
    } else { # this is a successful implementation of index bumping!
      recluster_set[[i]] = tibble(time = recluster_set[[i]][[1]], intensity = recluster_set[[i]][[2]], .cluster = reclust_data[[i]][["cluster"]] + max(recluster_set[[i-1]][[".cluster"]]))
    }
    
    
    colnames(recluster_set[[i]]) = c("time", "intensity", ".cluster")
    
    recluster_set[[i]] = recluster_set[[i]] %>% as_tibble()
  }
  
  return(recluster_set)
}

re_clust = function(clusters){
  
  # only work with the recluster rows,
  # recombine all rows later. the goal
  # is to return to the 'clusters'
  # data structure.
  
  no_recluster = clusters %>% 
    filter(!reclust)
  
  recluster_set = clusters %>% 
    filter(reclust) %>% 
    select(data, .cluster) %>%
    unnest(cols = c(data)) %>%
    group_by(.cluster) %>%
    group_split() 
  
  # now we are working with a list structure, so we need to use map functions onto the list.
  
  # perform k-means on each element of the above list
  
  reclust_data = recluster_set %>%
    map(kmeans_helper)
  
  # add the cluster vector onto each data frame in the above list, and bump up the index
  
  recluster_set = cluster_paste(recluster_set, reclust_data)
  
  # unlist and re-nest the data for shapiro tests
  
  reclustered = recluster_set %>% 
    bind_rows() %>%
    group_by(.cluster) %>%
    nest() %>%
    mutate(shap = map(data, shapiro_helper)) %>%
    mutate(reclust = ifelse(shap <= 0.05, T, F)) 
  
  
  # bump the indices of the clusters that didn't need to be reclustered
  
  no_recluster$.cluster = no_recluster$.cluster + max(reclustered$.cluster)
  
  # bind the rows that didn't need reclustering initially and return
  
  bind_rows(reclustered, no_recluster) %>% return()
  
}

# re_clust_ad = function(clusters){
#   
#   # only work with the recluster rows,
#   # recombine all rows later. the goal
#   # is to return to the 'clusters'
#   # data structure.
#   
#   no_recluster = clusters %>% 
#     filter(!reclust)
#   
#   recluster_set = clusters %>% 
#     filter(reclust) %>% 
#     select(data, .cluster) %>%
#     unnest(cols = c(data)) %>%
#     group_by(.cluster) %>%
#     group_split() 
#   
#   # now we are working with a list structure, so we need to use map functions onto the list.
#   
#   # perform k-means on each element of the above list
#   
#   reclust_data = recluster_set %>%
#     map(kmeans_helper)
#   
#   # add the cluster vector onto each data frame in the above list, and bump up the index
#   
#   recluster_set = cluster_paste(recluster_set, reclust_data)
#   
#   # unlist and re-nest the data for shapiro tests
#   
#   reclustered = recluster_set %>% 
#     bind_rows() %>%
#     group_by(.cluster) %>%
#     nest() %>%
#     mutate(shap = map(data, ad_helper)) %>%
#     mutate(reclust = ifelse(shap <= 0.05, T, F)) 
#   
#   # bump the indices of the clusters that didn't need to be reclustered
#   
#   no_recluster$.cluster = no_recluster$.cluster + max(reclustered$.cluster)
#   
#   # bind the rows that didn't need reclustering initially and return
#   
#   bind_rows(reclustered, no_recluster) %>% return()
#   
# }

min_time_helper = function(df){
  return(min(df[,1]))
}

cluster_sort = function(clusters){
  
  temp = clusters %>% 
    mutate(min_time = map(data, min_time_helper)) %>%
    unnest(c(data, shap, med, min_time)) %>%
    arrange(min_time) %>%
    nest(data = c(time, intensity))
  
  for(i in 1:length(temp$.cluster)){
    temp$.cluster[[i]] = i
  }
  
  return(temp)
  
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

cluster_compress = function(clusters, compression){
  
  clusters = clusters %>% 
    mutate(med = map(data, median_helper)) %>%
    cluster_sort()
  
  max = clusters %>% 
    unnest(cols = c(data))
  
  max = max(max$intensity)
  
  index = 1
  while(index <= (length(clusters$med)-1)){
    
    if(abs(clusters$med[index] - clusters$med[index+1]) <= compression*max){
      
      ## something is wrong with this line here. it is not correctly returning the median value of the compressed cluster.
      clusters[index,] = merge_helper(clusters$data[[index]], clusters$data[[index+1]], clusters$.cluster[index])
      clusters = clusters[-(index+1),]
    } else{
      index = index + 1
    }
    
    
  }
  
  return(clusters)
  
}

cluster_compress_2 = function(clusters){
  
  clusters = clusters %>% 
    mutate(med = map(data, median_helper), sd = map(data, sd_helper)) %>%
    cluster_sort()
  
  max = clusters %>% 
    unnest(cols = c(data))
  
  max = max(max$intensity)
  
  index = 1
  while(index <= (length(clusters$med)-1)){
    
    if(abs(clusters$med[index] - clusters$med[index+1]) <= 3*clusters$sd[[index]]){
      
      clusters[index,] = merge_helper(clusters$data[[index]], clusters$data[[index+1]], clusters$.cluster[index])
      
      clusters = clusters[-(index+1),]
      
    } 
    # if(clusters$sd[[index]] == 0 && abs(clusters$med[index] - clusters$med[index+1]) <= 3*clusters$sd[[index+1]]){
    # 
    #     clusters[index,] = merge_helper(clusters$data[[index]], clusters$data[[index+1]], clusters$.cluster[index])
    # 
    #     clusters = clusters[-(index+1),]
    # 
    # }
    else{
      index = index + 1
    }
    
    
  }
  
  return(clusters)
  
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

breakpoints = function(clusters){
  
  max_cluster = max(clusters$.cluster)
  
  t = clusters %>% unnest(cols = c(data)) %>% group_by(.cluster) %>% group_split() %>% map(breakpoint_helper, max_cluster) %>% bind_rows()
  
  newIndices = seq(1, nrow(t))
  t$.cluster = newIndices
  
  return(t)
}

# pipe = function(data){
#   
#   # structure and name the data
#   
#   data = as_tibble(data[,1:2])
#   colnames(data) = c("time", "intensity")
#   
#   # preprocess the data
#   
#   processed = data %>% find_window() %>% pre_process()
#   
#   scale_factor = processed$scale
#   
#   clusters = processed$data %>%
#     pre_clust()
#   
#   # perform binary segmentation either 5 times (way more steps than we'd ever get) or until all clusters are normally distribtued
#   
#   it_count = 0
#   while((it_count <= 10) && (clusters$reclust %>% sum() != 0)){
#     clusters = clusters %>% re_clust()
#     it_count = it_count + 1
#   }
#   
#   clusters = clusters %>%
#     mutate(med = map(data, median_helper)) %>%
#     cluster_sort() %>%
#     breakpoints() %>%
#     cluster_sort() %>%
#     cluster_compress_2() %>%
#     cluster_sort() %>%
#     unnest(cols = c(data)) %>%
#     mutate(intensity = intensity/scale_factor) %>%
#     group_by(.cluster) %>%
#     nest()
#   
#   return(clusters)
#   
# }

pipe_old = function(data, compression){
  
  # structure and name the data
  
  data = as_tibble(data[,1:2])
  colnames(data) = c("time", "intensity")
  
  # preprocess the data
  
  processed = data %>% 
    find_window() %>% 
    pre_process()
  
  scale_factor = processed$scale
  
  clusters = processed$data %>%
    pre_clust()
  
  # perform binary segmentation either 5 times (way more steps than we'd ever get) or until all clusters are normally distribtued
  
  it_count = 0
  while((it_count <= 10) && (clusters$reclust %>% sum() != 0)){
    clusters = clusters %>% re_clust()
    it_count = it_count + 1
  }
  
  clusters = clusters %>%
    mutate(med = map(data, median_helper)) %>%
    cluster_sort() %>%
    breakpoints() %>%
    cluster_sort() %>%
    cluster_compress(compression) %>%
    cluster_sort() %>%
    unnest(cols = c(data)) %>%
    mutate(intensity = intensity/scale_factor) %>%
    group_by(.cluster) %>%
    nest()
  
  return(clusters)
  
}

# pipe_ad = function(data, compression){
#   
#   # structure and name the data
#   
#   data = as_tibble(data[,1:2])
#   colnames(data) = c("time", "intensity")
#   
#   # preprocess the data
#   
#   processed = data %>% find_window() %>% pre_process()
#   
#   scale_factor = processed$scale
#   
#   clusters = processed$data %>%
#     pre_clust()
#   
#   # perform binary segmentation either 5 times (way more steps than we'd ever get) or until all clusters are normally distribtued
#   
#   it_count = 0
#   while((it_count <= 10) && (clusters$reclust %>% sum() != 0)){
#     clusters = clusters %>% re_clust_ad()
#     it_count = it_count + 1
#   }
#   
#   clusters = clusters %>%
#     mutate(med = map(data, median_helper)) %>%
#     cluster_sort() %>%
#     breakpoints() %>%
#     cluster_sort() %>%
#     cluster_compress(compression) %>%
#     cluster_sort() %>%
#     unnest(cols = c(data)) %>%
#     mutate(intensity = intensity/scale_factor) %>%
#     group_by(.cluster) %>%
#     nest()
#   
#   return(clusters)
#   
# }

cluster_plot = function(clusters, name){
  plotR = clusters %>% unnest(cols = c(data))
  
  processed_plot = ggplot(data = plotR, aes(x = time, y = intensity, colour = factor(.cluster))) +
    geom_point(size = 0.75, alpha = 1) +
    geom_line() +
    labs(x = "Time (s)", y = "Intensity (a.u.)", colour = "Cluster") +
    theme_classic()
  
  return(processed_plot)
}

stoSig = function(n, SN){
  
  randoms = runif(n, min = 0, max = 1)
  randoms[which(randoms >= 0.95)] = 1
  randoms[which(randoms < 0.95)] = 0
  
  stoSigSteps = sum(randoms)
  
  #construct the clean signal backwards
  
  signalI = c()
  signalAmp = 0
  for(i in 1:n){
    
    if(randoms[i] == 0){
      signalI[[i]] = signalAmp
    } 
    
    if(randoms[i] == 1){
      signalAmp = signalAmp + 100
      signalI[[i]] = signalAmp
    }
  }
  signalI = rev(signalI)
  #add buffer at end
  signalI = c(signalI, rep(0, 0.5*n))
  
  #add noise
  
  noisySig = c()
  for(i in 1:(1.5*n)){
    if(signalI[i] != 0){
      noisySig[[i]] = signalI[i] + rnorm(1, 0, SN*sqrt(signalI[i]))
    } else {
      noisySig[[i]] = signalI[i] + rnorm(1, 0, SN*10)
    }
  }
  
  #plot(noisySig, type = "l")
  return(list(data = data.frame(time = c((1:(1.5*n))/10), intensity = noisySig), steps = stoSigSteps))
  
}

# function for extracting the R^2 of a log fit of a dataset

log_fit = function(data){
  
  data = as_tibble(data[,1:2])
  colnames(data) = c("time", "intensity")

  processed = data %>%
    select(time, intensity) %>%
    find_window()
  
  fit = lm(log(abs(intensity) + 1) ~ time, data = processed) %>% summary()
  
  return(fit$r.squared)
  
}

# functions to help extract stats

ladder_extract = function(data){
  data$drops %>% median() %>% return()
}

med_ext = function(data){
  return(data$intensity %>% median())
}

stdv_ext = function(data){
  return(data$intensity %>% sd())
}

stat_extract = function(cluster){
  
  new = cluster
  
  medians = map(new$data, med_ext) %>% unlist(recursive = F)
  
  lifetimes = map(new$data, nrow) %>% unlist(recursive = F)
  lifetimes = lifetimes / 10
  
  stdv = map(new$data, stdv_ext) %>% unlist(recursive = F)
  
  drops = c()
  for(i in 1:nrow(new)){
    if(i != nrow(new)){
      drops[i] = medians[i] - medians[i+1]
    } else {
      drops[i] = 0
    }
  }
  
  return(cbind(new, medians = medians, lifetimes = lifetimes, stdv = stdv, drops = drops))
  
}

ladder_ext = function(cluster){
  
  ladder = cluster %>%
    arrange(desc(drops))
  
  ladder = cbind(ladder, position = c(1:nrow(ladder)))
  
  return(ladder)
  
}

unlist_helper = function(list){
  q = list %>% 
    unlist(recursive = F) %>% 
    as_tibble() %>% 
    set_names(c("time", "intensity", "steps")) %>% 
    group_by(steps) %>% nest() %>% 
    return()
}

normalize_int = function(cluster){
  max_drop = max(cluster$drops)
  cluster = cluster %>%
    mutate(norm_med = drops/max_drop)
  r = lm(data = cluster, norm_med ~ log(position)) %>% summary()
  return(r$coefficients[2, 1])
}

ladder_extract = function(data){
  data$drops[which(data$drops >= 0)] %>% median() %>% return()
}


