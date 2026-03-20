# ===============================
# Simulation Script
# VTPEH6270 Project
# Author: Xinran Chen
# ===============================

# -------------------------------
# Load libraries
# -------------------------------
library(dplyr)
library(janitor)
library(knitr)
library(ggplot2)

# Create output folder (reproducibility)
dir.create("output", showWarnings = FALSE)

# -------------------------------
# Load dataset
# -------------------------------
data_wastewater <- read.csv(
  "data/Wastewater Co-digestion and Biogas-to-grid Performance Indicators (New York City).csv",
  check.names = FALSE
)

# -------------------------------
# Data Description
# -------------------------------
data_description <- data.frame(
  Variable_Name = c(
    "Food Scraps Digested (wet tons)",
    "RNG Production (MMBtu)",
    "Food Scraps Input Level (low, medium, high)"
  ),
  Variable_Type = c(
    "Continuous",
    "Continuous",
    "Categorical"
  ),
  R_Class = c(
    "numeric",
    "numeric",
    "factor"
  )
)

kable(
  data_description,
  caption = "Description of key variables used in the analysis."
)

# -------------------------------
# Data Preparation
# -------------------------------
data_wastewater <- data_wastewater %>%
  mutate(
    total_organic_input =
      `Sludge Digested (wet tons)` + `Food Scraps Digested (wet tons)`,
    
    food_scraps_input_level = cut(
      `Food Scraps Digested (wet tons)`,
      breaks = quantile(
        `Food Scraps Digested (wet tons)`,
        probs = c(0, 1/3, 2/3, 1),
        na.rm = TRUE
      ),
      labels = c("low", "medium", "high"),
      include.lowest = TRUE
    )
  )

data_wastewater$food_scraps_input_level <- factor(
  data_wastewater$food_scraps_input_level,
  levels = c("low", "medium", "high")
)

# -------------------------------
# Visualization
# -------------------------------
p_obs <- ggplot(
  data_wastewater,
  aes(x = food_scraps_input_level, y = `RNG Production (MMBtu)`)
) +
  geom_boxplot() +
  labs(
    x = "Food Scraps Input Level",
    y = "Monthly RNG Production (MMBtu)"
  )

print(p_obs)

ggsave(
  "output/Observed_boxplot.pdf",
  plot = p_obs,
  width = 4,
  height = 3
)

# -------------------------------
# Simulation Parameters Table
# -------------------------------
parameters_table <- data.frame(
  Parameter = c(
    "Mean RNG production for low input group",
    "Mean RNG production for medium input group",
    "Mean RNG production for high input group",
    "Within-group variability"
  ),
  Symbol = c("u1", "u2", "u3", "sigma"),
  Description = c(
    "Average RNG production (low input)",
    "Average RNG production (medium input)",
    "Average RNG production (high input)",
    "Standard deviation within groups"
  )
)

kable(
  parameters_table,
  caption = "Simulation parameters for RNG production by input level."
)

# -------------------------------
# Simulation Basis
# -------------------------------
set.seed(123)

n <- 30
u1 <- 23000
u2 <- 22000
u3 <- 15000
sigma <- 5000

sim_data <- data.frame(
  food_scraps_input_level = rep(c("low", "medium", "high"), each = n),
  rng_production = c(
    rnorm(n, mean = u1, sd = sigma),
    rnorm(n, mean = u2, sd = sigma),
    rnorm(n, mean = u3, sd = sigma)
  )
)

sim_data$food_scraps_input_level <- factor(
  sim_data$food_scraps_input_level,
  levels = c("low", "medium", "high")
)

# Preview table
sim_preview <- head(sim_data)
kable(sim_preview, caption = "Preview of simulated dataset")

# -------------------------------
# Simulation Summary
# -------------------------------
sim_summary <- sim_data %>%
  group_by(food_scraps_input_level) %>%
  summarise(
    n = n(),
    mean_rng = mean(rng_production),
    sd_rng = sd(rng_production),
    median_rng = median(rng_production),
    q1_rng = quantile(rng_production, 0.25),
    q3_rng = quantile(rng_production, 0.75),
    min_rng = min(rng_production),
    max_rng = max(rng_production)
  )

kable(sim_summary, caption = "Simulated RNG production summary statistics.")

# -------------------------------
# Simulation Function
# -------------------------------
simulate_rng <- function(effect_size, noise, sample_size) {
  
  u1 <- 23000
  u2 <- 23000 + effect_size / 2
  u3 <- 23000 - effect_size / 2
  
  sim_data <- data.frame(
    food_scraps_input_level = rep(c("low", "medium", "high"), each = sample_size),
    rng_production = c(
      rnorm(sample_size, mean = u1, sd = noise),
      rnorm(sample_size, mean = u2, sd = noise),
      rnorm(sample_size, mean = u3, sd = noise)
    )
  )
  
  sim_data$food_scraps_input_level <- factor(
    sim_data$food_scraps_input_level,
    levels = c("low", "medium", "high")
  )
  
  sim_summary <- sim_data %>%
    group_by(food_scraps_input_level) %>%
    summarise(mean_rng = mean(rng_production))
  
  return(sim_summary)
}

# Test run
simulate_rng(8000, 5000, 30)

# -------------------------------
# Simulation Automation
# -------------------------------
effect_sizes <- seq(2000, 20000, length.out = 10)
sample_sizes <- seq(10, 100, length.out = 10)
noise_levels <- c(3000, 5000, 8000)

simulation_results <- list()
counter <- 1

for (e in effect_sizes) {
  for (s in sample_sizes) {
    for (n in noise_levels) {
      
      sim_data <- simulate_rng(e, n, s)
      
      simulation_results[[counter]] <- data.frame(
        effect_size = e,
        sample_size = s,
        noise = n,
        low = sim_data$mean_rng[1],
        medium = sim_data$mean_rng[2],
        high = sim_data$mean_rng[3]
      )
      
      counter <- counter + 1
    }
  }
}

simulation_plot_data <- do.call(rbind, simulation_results)

simulation_plot_data$mean_difference <-
  simulation_plot_data$low - simulation_plot_data$high

# -------------------------------
# Heatmap
# -------------------------------
p_sim <- ggplot(
  simulation_plot_data,
  aes(x = sample_size, y = effect_size, fill = mean_difference)
) +
  geom_tile() +
  facet_wrap(~ noise) +
  labs(
    x = "Sample Size",
    y = "Effect Size",
    fill = "Mean Difference"
  )

print(p_sim)

ggsave(
  "output/Simulation_heatmap.pdf",
  plot = p_sim,
  width = 5,
  height = 4
)
