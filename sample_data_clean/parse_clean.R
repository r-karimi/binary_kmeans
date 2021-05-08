# Function to process Casey's test traces for the manuscript
# April 2021

# Import packages
library(tidyverse)

setwd("~/Desktop/Cosa Lab/Algorithm_2.0/Data for RK April 20 2021")

filenames = list.files()[str_detect(list.files(), ".dat")]

# Loop through files and plot each raw data

for(i in filenames){
  data = read_table2(i, col_names = FALSE)[-1,c(1, 2)] %>%
    set_names(c("time", "signal"))
  
  raw_plot = ggplot(data = data, aes(x = time, y = signal)) +
    geom_line(alpha = 0.5) +
    #geom_point(alpha = 0.5) +
    theme_classic() +
    labs(x = "Time (s)", y = "Intensity (a.u.)")
  
  ggsave(plot = raw_plot, filename = paste("output_plots_raw/", str_sub(i, end = -5), ".pdf", sep = ""), device = "pdf", width = 3.3, height = 2.5)
}

# Loop through files and plot each clustering
for(i in filenames) {
  data = read_table2(i, col_names = FALSE)[-1,]
  processed = pipe_old(data, 0.075) %>%
    cluster_plot(i)
  ggsave(filename = paste("output_plots_c75/", str_sub(i, end = -5), "_processed.pdf", sep = ""), plot = processed, device = "pdf", width = 7, height = 5.5)
}

# Loop through files and extract each number of steps
step_data = tibble(trace = rep(0, length(filenames)), predicted_steps = rep(0, length(filenames)))

for(i in 1:length(filenames)) {
  
  # update label
  step_data$trace[[i]] = str_extract_all(filenames[i], "[0-9]+") %>%
    as.numeric()
  
  # process trace
  data = read_table2(filenames[i], col_names = FALSE)
  processed = pipe_old(data, 0.075)
  step_data$predicted_steps[[i]] = nrow(processed) - 1
  
}

# Read in Casey's counts and merge datasets

casey_counts = readxl::read_xlsx("counts.xlsx")
plot_data = inner_join(casey_counts, step_data, by = "trace") #%>%
mutate(error = predicted_steps - real_steps)

plot_data_freq = plot_data %>% 
  select(-trace) %>% 
  table() %>% 
  as_tibble() %>%
  mutate(real_steps = as.numeric(real_steps),
         predicted_steps = as.numeric(predicted_steps)) %>%
  filter(n != 0)
  

# Make correlation scatterplot

correlation_plot = ggplot(data = plot_data_freq, aes(x = real_steps, y = predicted_steps, colour = n)) +
  geom_point(size = 3) +
  geom_abline(slope = 1, intercept = 0) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed") +
  geom_abline(slope = 1, intercept = -1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 10), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_y_continuous(limits = c(0, 10), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8, 10)) +
  scale_colour_distiller(palette = "Blues", direction = 1) +
  theme_bw() +
  labs(x = "Real Steps", y = "Predicted Steps")

ggsave("correlation_plot_c75.pdf", correlation_plot, device = "pdf", height = 5.5, width = 7)

# Make histogram of counts, no offset

histogram_data = plot_data %>%
  set_names(c("trace", "Real Steps", "Predicted Steps")) %>%
  pivot_longer(cols = -trace)

histogram_plot = ggplot(data = histogram_data, aes(colour = name, fill = name, x = value)) +
  geom_bar(position = position_dodge(preserve = "single"), alpha = 0.5) +
  scale_x_continuous(limits = c(0, 12), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8, 10, 12)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_classic() +
  labs(x = "Steps", y = "Count", fill = "") +
  theme(legend.position = "top") +
  guides(colour = FALSE)

ggsave("histogram_plot_c75.pdf", histogram_plot, device = "pdf", height = 5.5, width = 7)

# Make error plot

pairwise_data = inner_join(casey_counts, step_data, by = "trace") %>%
  mutate(error = `predicted_steps` - real_steps)

pairwise_error_plot = ggplot(data = pairwise_data, aes(x = error)) +
  geom_bar(fill = "black", colour = "black") +
  scale_x_continuous(limits = c(-6, 6), breaks = c(-6, -3, 0, 3, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  theme_classic() +
  labs(x = "Error (Predicted - Real)", y = "Count")

ggsave("pairwise_error_plot_c75.pdf", pairwise_error_plot, device = "pdf", height = 5.5, width = 7)

write_csv(pairwise_data, file = "pairwise_error_c75.csv")


# Make raw data files for Gonzalo

pairwise_data %>%
  count(real_steps) %>%
  write_csv("real_steps_count_c75.csv")

pairwise_data %>%
  count(predicted_steps) %>%
  write_csv("predicted_steps_count_c75.csv")

pairwise_data %>%
  count(error) %>%
  write_csv("error_count_c75.csv")


# Make C15 Plots

# Loop through files and plot each clustering
for(i in filenames) {
  data = read_table2(i, col_names = FALSE)[-1,]
  processed = pipe_old(data, 0.15) %>%
    cluster_plot(i)
  ggsave(filename = paste("output_plots_c15/", str_sub(i, end = -5), "_processed.pdf", sep = ""), plot = processed, device = "pdf", width = 3.3, height = 2.5)
}

# Loop through files and extract each number of steps
step_data = tibble(trace = rep(0, length(filenames)), predicted_steps = rep(0, length(filenames)))

for(i in 1:length(filenames)) {
  
  # update label
  step_data$trace[[i]] = str_extract_all(filenames[i], "[0-9]+") %>%
    as.numeric()
  
  # process trace
  data = read_table2(filenames[i], col_names = FALSE)
  processed = pipe_old(data, 0.15)
  step_data$predicted_steps[[i]] = nrow(processed) - 1
  
}

# Read in Casey's counts and merge datasets

casey_counts = readxl::read_xlsx("counts.xlsx")
plot_data = inner_join(casey_counts, step_data, by = "trace") #%>%
mutate(error = predicted_steps - real_steps)


# Make correlation scatterplot

plot_data_freq = plot_data %>% 
  select(-trace) %>% 
  table() %>% 
  as_tibble() %>%
  mutate(real_steps = as.numeric(real_steps),
         predicted_steps = as.numeric(predicted_steps)) %>%
  filter(n != 0)

correlation_plot = ggplot(data = plot_data_freq, aes(x = real_steps, y = predicted_steps, colour = n, size = n)) +
  scale_colour_distiller(palette = "Blues", direction = 1, limits = c(0, 15),
                         breaks = 3*c(1:5), guide = "legend") +
  scale_size_continuous(limits = c(0, 15), breaks = 3*c(1:5)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, alpha = 0.25) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", alpha = 0.25) +
  geom_abline(slope = 1, intercept = -1, linetype = "dashed", alpha = 0.25) +
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0), breaks = c(0:8)) +
  scale_y_continuous(limits = c(0, 8), expand = c(0, 0), breaks = c(0:8)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  labs(x = "Real Steps", y = "Predicted Steps")

correlation_plot

ggsave("correlation_plot_c15.pdf", correlation_plot, device = "pdf", height = 2.5, width = 3.3)

# Make histogram of counts, no offset

histogram_data = plot_data %>%
  set_names(c("trace", "Real Steps", "Predicted Steps")) %>%
  pivot_longer(cols = -trace)

histogram_plot = ggplot(data = histogram_data, aes(colour = name, fill = name, x = value)) +
  geom_bar(position = position_dodge(preserve = "single"), alpha = 0.5) +
  scale_x_continuous(limits = c(0, 8), expand = c(0, 0), breaks = c(0, 2, 4, 6, 8)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0), breaks = c(0, 5, 10, 15, 20, 25, 30)) +
  theme_classic() +
  labs(x = "Steps", y = "Count", fill = "") +
  theme(legend.position = "top") +
  guides(colour = FALSE)

ggsave("histogram_plot_c15.pdf", histogram_plot, device = "pdf", height = 2.5, width = 3.3)

# Make error plot

pairwise_data = inner_join(casey_counts, step_data, by = "trace") %>%
  mutate(error = `predicted_steps` - real_steps)

pairwise_error_plot = ggplot(data = pairwise_data, aes(x = error)) +
  geom_bar(fill = "gray", colour = "black") +
  scale_x_continuous(limits = c(-6, 6), breaks = c(-6, -3, 0, 3, 6), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  theme_classic() +
  labs(x = "Error (Predicted Steps - Real Steps)", y = "Count")

ggsave("pairwise_error_plot_c15.pdf", pairwise_error_plot, device = "pdf", height = 2.5, width = 3.3)

write_csv(pairwise_data, file = "pairwise_error_c15.csv")


# Make raw data files for Gonzalo

pairwise_data %>%
  count(real_steps) %>%
  write_csv("real_steps_count_c15.csv")

pairwise_data %>%
  count(predicted_steps) %>%
  write_csv("predicted_steps_count_c15.csv")

pairwise_data %>%
  count(error) %>%
  write_csv("error_count_c15.csv")



# Last traces for the paper

# Import packages
library(tidyverse)

setwd("~/Desktop/Cosa Lab/Algorithm_2.0/last_data")

red_trace_raw = read.delim("tr1red.txt")
red_trace_processed = pipe_old(red_trace_raw, 0.075) %>%
  cluster_plot("Red Trace")
red_trace_processed
ggsave("red_trace_clustered.pdf", plot = red_trace_processed, device = "pdf",
       height = 2.5, width = 3.3)


green_trace_raw = read.delim("tr1green.txt")
green_trace_processed = pipe_old(green_trace_raw, 0.076) %>%
  cluster_plot("Green Trace") +
  scale_x_continuous(limits = c(0, 37), breaks = c(0, 36)) +
  scale_y_continuous(limits = c(-10, 1250), breaks = c(0, 1250)) +
  theme(legend.key.size = unit(0.5, "cm"))
green_trace_processed
ggsave("green_trace_clustered.pdf", plot = green_trace_processed, device = "pdf",
       height = 2.5, width = 3.3)
