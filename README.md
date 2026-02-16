# relationship-dissolution-model
R code for modelling relationship dissolution using Understanding Society panel data.

# Relationship Dissolution Modelling

This repository contains the R code used to build the longitudinal panel, estimate the relationship‑dissolution model, and generate the empirical inputs for the cost–benefit analysis in my dissertation. The scripts reproduce the full workflow: data preparation, variable harmonisation, transition construction, logistic regression, and the heterogeneity checks reported in the main text.

## Contents

- `analysis.R` – main script for constructing the panel, estimating the model, and producing the quantities used in the separations‑avoided calculations.
- (No data files are included.)

## Data access

The analysis uses the Understanding Society Innovation Panel. These datasets are **not included in this repository** for licensing reasons. They can be accessed directly through the UK Data Service by registered users.

Once downloaded, place the `.tab` files in a folder called `tab/` in the working directory before running the script, and adjust the working directory in the code to match the users.

## Running the code

The script is written to run end‑to‑end without modification, assuming the data files are stored in `tab/`. It:

1. Builds a long-format panel across waves.
2. Harmonises life‑satisfaction variables where naming differs.
3. Constructs the satisfaction‑fall indicator.
4. Defines union transitions and extracts valid breakup events.
5. Estimates the logistic regression model.
6. Computes the empirical quantities used in the cost–benefit modelling.
7. Runs the heterogeneity checks (parents vs non‑parents; married/civil/cohabiting).

All results used in the dissertation are generated directly from this script.

## Reproducibility

The repository contains only the code required to reproduce the analysis. No licensed data or derived datasets are stored here. Users with access to the Innovation Panel data should be able to replicate the modelling by placing the files in the expected directory structure and running `analysis.R`.


