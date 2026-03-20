# ===============================
# Data Exploration Script
# VTPEH6270 Project
# Author: Xinran Chen
# ===============================

# -------------------------------
# Load libraries
# -------------------------------
library(dplyr)
library(janitor)
library(ggplot2)
dir.create("output", showWarnings = FALSE)

# -------------------------------
# Load data
# -------------------------------
data_wastewater <- read.csv(
  "data/Wastewater Co-digestion and Biogas-to-grid Performance Indicators (New York City).csv"
)

# -------------------------------
# Clean column names
# -------------------------------
data_wastewater <- clean_names(data_wastewater)

# -------------------------------
# Rename variables
# -------------------------------
data_wastewater <- data_wastewater %>%
  rename(
    sludge_digested = sludge_digested_wet_tons,
    food_scraps_digested = food_scraps_digested_wet_tons,
    rng_produced = rng_production_mm_btu,
    rng_system_uptime = rng_system_uptime,
    biogas_flared = flared_biogas_mscf,
    flaring_reduction = reduction_in_flaring
  )

# -------------------------------
# Inspect data
# -------------------------------
str(data_wastewater)
summary(data_wastewater)

# -------------------------------
# Save cleaned dataset
# -------------------------------
write.csv(
  data_wastewater,
  "data/wastewater.csv",
  row.names = FALSE
)

# -------------------------------
# Create derived variable
# -------------------------------
data_wastewater <- data_wastewater %>%
  mutate(
    total_organic_input = sludge_digested + food_scraps_digested
  )

# -------------------------------
# Categorize into tertiles
# -------------------------------
data_wastewater <- data_wastewater %>%
  mutate(
    organic_input_level = ntile(total_organic_input, 3),
    organic_input_level = factor(
      organic_input_level,
      levels = c(1, 2, 3),
      labels = c("Low", "Medium", "High")
    )
  )

# -------------------------------
# Summary of input ranges
# -------------------------------
organic_range_summary <- data_wastewater %>%
  group_by(organic_input_level) %>%
  summarise(
    min_value = min(total_organic_input, na.rm = TRUE),
    max_value = max(total_organic_input, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(organic_range_summary)

# -------------------------------
# Summary of RNG production
# -------------------------------
grouped_summary <- data_wastewater %>%
  group_by(organic_input_level) %>%
  summarise(
    n = n(),
    mean_rng = mean(rng_produced, na.rm = TRUE),
    median_rng = median(rng_produced, na.rm = TRUE),
    sd_rng = sd(rng_produced, na.rm = TRUE),
    min_rng = min(rng_produced, na.rm = TRUE),
    max_rng = max(rng_produced, na.rm = TRUE),
    .groups = "drop"
  )

print(grouped_summary)

# -------------------------------
# Visualization
# -------------------------------
p <- ggplot(data_wastewater,
            aes(x = organic_input_level, y = rng_produced)) +
  geom_boxplot(width = 0.6, size = 0.5, outlier.shape = NA) +
  geom_jitter(width = 0.08, size = 1.8, alpha = 0.7) +
  labs(
    x = "Total organic input (tertiles)",
    y = "RNG production (MMBtu per month)"
  ) +
  theme_classic(base_size = 9) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

print(p)

# -------------------------------
# Save figure
# -------------------------------
ggsave(
  "output/Figure1_RNG_by_input.pdf",
  plot = p,
  width = 3.5,
  height = 3,
  units = "in"
)