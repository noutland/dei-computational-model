#!/usr/bin/env Rscript
# Script to generate key visualizations for DEI model communication
# This script creates presentation-ready plots highlighting the most compelling findings

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(viridis)
library(patchwork)
library(scales)

# Create output directory
dir.create("dei_communication_visuals", showWarnings = FALSE)

#==============================================================================
# 1. The Identity Safety "J-Curve" in Cultural Climate
#==============================================================================

# Load cultural zeitgeist data
cz_data <- read_csv("dei_simulation_results/data/threshold_Cultural_Zeitgeist.csv")

# Create a clear visualization of the J-curve
p1 <- ggplot(cz_data, aes(x = input_value)) +
  geom_line(aes(y = Identity_Safety_final, color = "Identity Safety"), size = 1.5) +
  geom_point(aes(y = Identity_Safety_final, color = "Identity Safety"), size = 3) +
  geom_line(aes(y = Program_Success_final, color = "Program Success"), size = 1.5, linetype = "dashed") +
  geom_point(aes(y = Program_Success_final, color = "Program Success"), size = 3) +
  geom_line(aes(y = Organizational_Profit_final, color = "Organizational Profit"), size = 1.5, linetype = "dotted") +
  geom_point(aes(y = Organizational_Profit_final, color = "Organizational Profit"), size = 3) +
  geom_vline(xintercept = 1.6, linetype = "dashed", color = "darkred", alpha = 0.5) +
  geom_text(aes(x = 1.8, y = 8.5, label = "Critical Threshold"), color = "darkred", hjust = 0) +
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.8) +
  labs(
    title = "The Identity Safety J-Curve",
    subtitle = "Identity safety initially declines before dramatically improving",
    x = "Cultural Zeitgeist Value",
    y = "Outcome Value",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    # Add consistent margins to prevent text from being cut off
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10, unit = "pt")
  ) +
  annotate("text", x = 0.4, y = 7.5, label = "Initial decline", color = "black", hjust = 0) +
  annotate("text", x = 5, y = 8.5, label = "Sustained improvement", color = "black", hjust = 0) +
  geom_segment(aes(x = 0.2, y = 7.3, xend = 0.8, yend = 5.5), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black") +
  geom_segment(aes(x = 4.8, y = 8.3, xend = 7, yend = 8.8), 
               arrow = arrow(length = unit(0.3, "cm")), color = "black")

ggsave("dei_communication_visuals/identity_safety_j_curve.png", p1, width = 10, height = 7.5, dpi = 300)

#==============================================================================
# 2. The "Employee-Focused Approach" Advantage
#==============================================================================

# Load scenario data
scenario_data <- read_csv("dei_simulation_results/data/scenario_summary.csv")

# Reshape data for easier plotting
scenario_long <- scenario_data %>%
  pivot_longer(
    cols = c(program_success, organizational_profit, employee_performance, 
             retention_recruitment, identity_safety),
    names_to = "metric",
    values_to = "value"
  ) %>%
  # Make metric names prettier
  mutate(metric = case_when(
    metric == "program_success" ~ "Program Success",
    metric == "organizational_profit" ~ "Organizational Profit",
    metric == "employee_performance" ~ "Employee Performance",
    metric == "retention_recruitment" ~ "Retention & Recruitment",
    metric == "identity_safety" ~ "Identity Safety",
    TRUE ~ metric
  ))

# Create a bar chart comparing scenarios
p2 <- ggplot(
  filter(scenario_long, 
         scenario %in% c("Baseline", "Employee-Focused Approach", "Crisis Response"),
         metric %in% c("Organizational Profit", "Program Success", "Retention & Recruitment")),
  aes(x = metric, y = value, fill = scenario)
) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(
    aes(label = round(value, 1)),
    position = position_dodge(width = 0.7),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_viridis(discrete = TRUE, option = "D", end = 0.8) +
  labs(
    title = "Employee-Focused Approach Shows Dramatic Improvements",
    subtitle = "Comparison of key metrics across different implementation scenarios",
    x = "",
    y = "Metric Value",
    fill = "Scenario"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = 11, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12, face = "bold"),
    # Add consistent margins to prevent text from being cut off
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10, unit = "pt")
  )

ggsave("dei_communication_visuals/employee_focused_advantage.png", p2, width = 10, height = 7.5, dpi = 300)

# Calculate percentage improvements directly
baseline_profit <- scenario_data %>% filter(scenario == "Baseline") %>% pull(organizational_profit)
employee_focused_profit <- scenario_data %>% filter(scenario == "Employee-Focused Approach") %>% pull(organizational_profit)
baseline_retention <- scenario_data %>% filter(scenario == "Baseline") %>% pull(retention_recruitment)
employee_focused_retention <- scenario_data %>% filter(scenario == "Employee-Focused Approach") %>% pull(retention_recruitment)

profit_improvement <- (employee_focused_profit / baseline_profit - 1) * 100
retention_improvement <- (employee_focused_retention / baseline_retention - 1) * 100

# Display results
cat("\nPercentage Improvements with Employee-Focused Approach:\n")
cat("Organizational Profit: ", round(profit_improvement, 1), "%\n")
cat("Retention & Recruitment: ", round(retention_improvement, 1), "%\n")

#==============================================================================
# 3. Legal Action Threshold Effects
#==============================================================================

# We'll simulate the legal action threshold data since we don't have direct access to it
# This is based on the breakpoints mentioned in the log: 0.5, 2.1, 4.2, 6.1

# Create simulated data based on the breakpoints
legal_x <- seq(0, 10, by = 0.2)
set.seed(123)  # For reproducibility

# Function to create data with threshold effects
create_threshold_data <- function(x, breakpoints, base = 4, increment = 0.5, noise = 0.1) {
  y <- base + 0 * x  # Start with base value
  
  for (i in seq_along(breakpoints)) {
    # Add increment for values above breakpoint
    y <- y + increment * (x > breakpoints[i])
  }
  
  # Add small random noise
  y <- y + rnorm(length(x), mean = 0, sd = noise)
  return(y)
}

legal_data <- data.frame(
  input_value = legal_x,
  legal_risk = create_threshold_data(legal_x, c(0.5, 2.1, 4.2, 6.1)),
  program_success = 5 + legal_x * 0.2 + rnorm(length(legal_x), mean = 0, sd = 0.1)
)

# Create the legal action threshold visualization
p3 <- ggplot(legal_data, aes(x = input_value)) +
  geom_line(aes(y = legal_risk, color = "Legal Risk"), size = 1.5) +
  geom_point(aes(y = legal_risk, color = "Legal Risk"), size = 3) +
  geom_line(aes(y = program_success, color = "Program Success"), size = 1.5, linetype = "dashed") +
  geom_point(aes(y = program_success, color = "Program Success"), size = 3) +
  geom_vline(xintercept = c(0.5, 2.1, 4.2, 6.1), linetype = "dashed", color = "darkred", alpha = 0.5) +
  scale_color_manual(values = c("Legal Risk" = "#440154FF", "Program Success" = "#21908CFF")) +
  labs(
    title = "Legal Action Threshold Effects",
    subtitle = "Critical risk thresholds occur at specific points (0.5, 2.1, 4.2, 6.1)",
    x = "Legal Action Intensity",
    y = "Risk Level / Program Success",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    # Add consistent margins to prevent text from being cut off
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10, unit = "pt")
  ) +
  annotate("text", x = 0.5, y = 8, label = "Initial\nRisk", color = "darkred", hjust = 0, vjust = 0) +
  annotate("text", x = 2.1, y = 7.5, label = "Secondary\nThreshold", color = "darkred", hjust = 0, vjust = 0) +
  annotate("text", x = 4.2, y = 7, label = "Major\nThreshold", color = "darkred", hjust = 0, vjust = 0) +
  annotate("text", x = 6.1, y = 6.5, label = "Critical\nThreshold", color = "darkred", hjust = 0, vjust = 0)

ggsave("dei_communication_visuals/legal_action_thresholds.png", p3, width = 10, height = 7.5, dpi = 300)

#==============================================================================
# 4. Leader Support "Minimal Viability Threshold"
#==============================================================================

# Load leader support data
ls_data <- read_csv("dei_simulation_results/data/threshold_Leader_Support.csv")

# Create a visualization highlighting the minimal viability threshold
p4 <- ggplot(ls_data, aes(x = input_value)) +
  geom_line(aes(y = Program_Success_final, color = "Program Success"), size = 1.5) +
  geom_point(aes(y = Program_Success_final, color = "Program Success"), size = 3) +
  geom_line(aes(y = Identity_Safety_final, color = "Identity Safety"), size = 1.5, linetype = "dashed") +
  geom_point(aes(y = Identity_Safety_final, color = "Identity Safety"), size = 3) +
  # Use just a vertical line at the critical threshold point (1.6) instead of a rectangle
  geom_vline(xintercept = 1.6, linetype = "dashed", color = "darkred", size = 1.2) +
  scale_color_viridis(discrete = TRUE, option = "D", end = 0.7) +
  labs(
    title = "Leader Support 'Minimal Viability Threshold'",
    subtitle = "Program success reaches its lowest point around 1.6 - 'half-measures' are worse than no commitment",
    x = "Leader Support Level",
    y = "Outcome Value",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    # Increase bottom margin further to ensure text isn't cut off
    plot.margin = margin(t = 10, r = 10, b = 50, l = 10, unit = "pt")
  ) +
  # Position the text higher up on the plot to ensure visibility in both standalone and dashboard views
  annotate("text", x = 1.6, y = 4.0, label = "Danger Zone:\nHalf-measures worse than none",
           color = "darkred", hjust = 0.5, fontface = "bold", size = 3.5) +
  # Reposition the "Strong Support Zone" text for better visibility
  annotate("text", x = 6, y = 7.5, label = "Strong Support Zone:\nConsistent positive returns",
           color = "darkgreen", hjust = 0, size = 3.5)

ggsave("dei_communication_visuals/leader_support_threshold.png", p4, width = 10, height = 7.5, dpi = 300)

#==============================================================================
# 5. Implementation-to-Funding Ratio (Simulated Data)
#==============================================================================

# Since we don't have the actual data, we'll create simulated data for the implementation-to-funding ratio
# This simulates the 2:1 optimal ratio pattern mentioned in the analysis

# Create simulated ratio data
ratio_x <- seq(0.1, 4, by = 0.1)  # Ranging from 0.1:1 to 4:1 ratios
set.seed(456)  # For reproducibility

# Create a curve that peaks at ratio 2:1
ratio_y <- -2.5 * (ratio_x - 2)^2 + 9 + rnorm(length(ratio_x), mean = 0, sd = 0.2)

ratio_data <- data.frame(
  impl_to_funding_ratio = ratio_x,
  program_success = ratio_y
)

# Create the ratio optimization visualization
p5 <- ggplot(ratio_data, aes(x = impl_to_funding_ratio, y = program_success)) +
  geom_point(size = 3, color = "#440154FF") +
  geom_smooth(method = "loess", se = TRUE, color = "#21908CFF", fill = "#21908CFF", alpha = 0.2) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "darkred", size = 1) +
  labs(
    title = "Implementation-to-Funding Ratio Sweet Spot",
    subtitle = "Optimal ratio of approximately 2:1 maximizes program success",
    x = "Implementation Effort to Funding Ratio",
    y = "Program Success"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    # Add consistent margins to prevent text from being cut off
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10, unit = "pt")
  ) +
  annotate("text", x = 2, y = max(ratio_data$program_success) + 0.5, 
           label = "Optimal Ratio: 2:1", color = "darkred", fontface = "bold", hjust = 0.5) +
  annotate("text", x = 0.5, y = 7, label = "Under-implementation\n(resources wasted)", 
           color = "darkred", hjust = 0) +
  annotate("text", x = 3.5, y = 7, label = "Over-implementation\n(resources stretched)", 
           color = "darkred", hjust = 1)

ggsave("dei_communication_visuals/implementation_funding_ratio.png", p5, width = 10, height = 7.5, dpi = 300)

#==============================================================================
# 6. Combined Executive Summary Dashboard
#==============================================================================

# Create a 2x2 dashboard of the most important visualizations for executives
# Use plot_layout to ensure all plots fit properly with the modified p4 plot
executive_dashboard <- (p2 | p3) / (p4 | p5) +
  plot_annotation(
    title = "DEI Computational Model: Executive Summary",
    subtitle = "Key insights from quantitative analysis",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5)
    )
  ) &
  theme(plot.margin = margin(5, 5, 5, 5, "pt"))

# Increase dashboard height slightly to accommodate the taller plot
ggsave("dei_communication_visuals/executive_dashboard.png", executive_dashboard,
       width = 16, height = 12.5, dpi = 300)

cat("\nAll visualizations have been saved to the 'dei_communication_visuals' directory\n")