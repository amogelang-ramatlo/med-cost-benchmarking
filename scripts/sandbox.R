library(ggplot2)

# 1. Extract the results and convert Cost to a Factor for a clean legend
svm_results <- svm_tuned$results %>% mutate(C = as.factor(C))

# 2. Build the optimized plot
ggplot(svm_results, aes(x = sigma, y = ROC, group = C, color = C)) +
  geom_line(linewidth = 1) + 
  
  theme_bw() +
  labs(
    x = "Sigma (Kernel Spread)",
    y = "Area Under ROC",
    color = "Cost Parameter (C)"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 0) 
  )


library(ggplot2)
library(dplyr)

# 1. Prepare the data by duplicating it
# We bind the results to themselves, creating a 'General' and 'Zoomed' version
plot_data <- bind_rows(
  svm_results %>% mutate(View = "1. General Trend (Full Scale)"),
  svm_results %>% mutate(View = "2. Zoomed (Optimal Region)")
) %>%
  mutate(View = factor(View))

# 2. Create the Faceted Plot
ggplot(plot_data, aes(x = sigma, y = ROC, group = C, color = as.factor(C))) +
  geom_line(linewidth = 1) +
  
  # This is the magic line: 'free_y' allows the Zoomed facet to 
  # automatically adjust its limits to the 'action' area.
  facet_wrap(~View, scales = "free_y") +
  
  theme_bw() +
  labs(
    title = "SVM Tuning Profile: Identifying the Global Optimum",
    x = "Sigma (Kernel Spread - Log10 Scale)",
    y = "Area Under ROC",
    color = "Cost (C)"
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95")
  )


library(ggplot2)
library(patchwork) # You may need to install this: install.packages("patchwork")

# 1. The "General Trend" Plot
# We set a wide Y-axis to show the overall improvement
p1 <- ggplot(svm_tuned$results, aes(x = sigma, y = ROC, group = C, color = as.factor(C))) +
  geom_line(linewidth = 1) +
  theme_bw() +
  coord_cartesian(ylim = c(0.85, 1)) + # Shows the "big picture"
  labs(title = "A: General Tuning Trend", x = "Sigma", y = "AUROC", color = "Cost (C)") +
  theme(legend.position = "none", panel.grid.minor = element_blank()) # Hide legend on first plot to save space


# 2. The "Zoomed" Plot
# We use coord_cartesian to zoom into the 0.95 - 0.98 range where the 'action' is
p2 <- ggplot(svm_tuned$results, aes(x = sigma, y = ROC, group = C, color = as.factor(C))) +
  geom_line(linewidth = 1) +
  theme_bw() +
  coord_cartesian(
    xlim = c(0.01 - 0.005, 0.02), # Adjust based on your best Sigma values
    ylim = c(0.955, 0.975)  # Tight focus on the peak
  ) +
  labs(title = "B: Optimal Region (Zoomed)", x = "Sigma", y = NULL, color = "Cost (C)")+
  theme(panel.grid.minor = element_blank())

# Combine them side-by-side
p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")



library(dplyr)
library(ggplot2)
library(patchwork)

# 1. Extract results and label the 'View'


# 2. Merge into one master plotting data frame
svm_plot_data <- bind_rows(
  svm_general$results %>% mutate(view = "General Trend"),
  svm_zoomed$results  %>% mutate(view = "Optimal Region (Zoomed)")
)

# 3. Create the Plot
# We use facet_wrap with 'scales = "free"' so the zoomed side can auto-focus
ggplot(svm_plot_data, aes(x = sigma, y = ROC, group = C, color = as.factor(C))) +
  geom_line(linewidth = 1) +
  facet_wrap(~view, scales = "free") + 
  theme_bw() +
  labs(
    x = "Sigma",
    y = "Area Under ROC",
    color = "Cost (C)"
  ) +
  theme(legend.position = "bottom")
