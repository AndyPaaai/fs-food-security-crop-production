# fs-food-security-crop-production

Code and data to reproduce the *income-group* analyses for Manuscript 1.

## What this repo contains

- **data/**
  - `step_1_income_food_production.xlsx` — Step 1 (stratified bivariate models by income group)
  - `step_2_income_food_production.xlsx` — Step 2 (hierarchical/mixed-effects panel models)
  - `step_3_income_food_production.xlsx` — Step 3 (mixed-effects moderator screening)

- **src/**
  - `Main Analysis.R` — main analysis pipeline (requires the master input dataset; update the input path at the top of the script).
  - `Plots.R` — figure generation from the `data/step_*` files (update the `read_excel()` paths at the top to point to `data/`).

## Quick start (R)

1. Open R in the repository root.
2. Install packages (if needed): `readxl`, `dplyr`, `tidyr`, `ggplot2`, `scales`, `gridExtra`, `grid`, plus modeling packages used in the pipeline (`MASS`, `sandwich`, `lmtest`, `lme4`, `glmmTMB`).
3. Reproduce figures:
   - Edit the three `read_excel()` lines at the top of `src/Plots.R` to use `data/step_*.xlsx`.
   - Run: `source("src/Plots.R")`

## Data Availability

This project uses openly available data from public databases. The bibliometric dataset used is **available upon reasonable request**.

## License

This repository is licensed under the **MIT License**, allowing free use, modification, and distribution with attribution. See `LICENSE` file for more details.

[![DOI](https://zenodo.org/badge/1137306768.svg)](https://doi.org/10.5281/zenodo.18298032)

