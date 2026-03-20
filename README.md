# VTPEH6270 Project – Wastewater Co-digestion Analysis

## Author
Xinran Chen, Cornell University

## Contact
xc577@cornell.edu

---

## Project Overview

This project examines the relationship between organic waste input and renewable natural gas (RNG) production using wastewater co-digestion data from New York City. 

Both observational analysis and simulation approaches are used to evaluate how different levels of food scraps input influence RNG production and system performance.

---

## Research Question

How does food scraps input level influence RNG production?

---

## Data Source

NYC wastewater co-digestion and biogas-to-grid performance dataset.

---

## Repository Structure

- `data/` : raw data, processed data, and variable documentation  
- `script/` : R scripts for data exploration and simulation  
- `output/` : generated figures and results  

---

## How to Run

1. Open RStudio in the project root directory  
2. Run `script/exploration.R`  
3. Run `script/simulation.R`  

All outputs will be saved automatically in the `output/` folder.

---

## Required Packages

- dplyr  
- janitor  
- ggplot2  
- knitr  

---

## Reproducibility

This project is fully reproducible. All scripts are designed to run from the project root directory without requiring manual path adjustments. The `output/` folder will be created automatically if it does not already exist.

---

## AI Disclosure

ChatGPT was used to assist with code structuring and documentation. All content was reviewed, modified, and validated by the author.