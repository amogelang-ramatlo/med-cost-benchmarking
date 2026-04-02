# Reshape into long format and create visualization
temp <- model_metrics %>% select(-AU_PRC) %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

ggplot(temp, aes(x = Metric, y = Value, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_cartesian(ylim = c(0.86, 1)) +
  scale_y_continuous(breaks = seq(0.8, 1, 0.02)) +
  labs(y = "Score", x = NULL, fill = "Model") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )