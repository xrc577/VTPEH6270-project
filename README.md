# VTPEH6270 Project – Wastewater Co-digestion Analysis

## Project Overview

This project investigates the relationship between organic waste input and renewable natural gas (RNG) production using wastewater co-digestion data from New York City.

Both observational analysis and simulation are conducted to evaluate how varying levels of organic input influence RNG production and system performance.

---

## Repository Structure

- data/ : raw dataset, cleaned dataset, and variable documentation  
- script/ : R scripts for data exploration and simulation  
- output/ : generated figures and results  

---

## How to Run

1. Open RStudio in the project root folder  
2. Run `script/exploration.R`  
3. Run `script/simulation.R`  

All outputs will be automatically saved in the `output/` folder.

---

## Required Packages

- dplyr  
- janitor  
- ggplot2  
- knitr  

---

## Reproducibility

This project is fully reproducible. All scripts are designed to run from the project root without requiring manual path adjustments. The `output/` folder will be created automatically if it does not exist.