# BST260
Final Project: This study is titled "Characterizing Excess Mortality in Puerto Rico After Hurricane María".

## Overview
This project estimates and analyzes excess mortality in Puerto Rico following Hurricane María, focusing on variations across age groups and sexes. The analysis uses mortality and population data from 2002 to 2018 and applies advanced statistical modeling to quantify deviations from expected mortality rates during the hurricane's aftermath.

The results are reproducible using the provided dataset and R scripts, leveraging the `excessmort` package.

## Project Structure
- **`data/`**: Contains the input dataset `puerto_rico_counts.rds` (from the `excessmort` package).
- **`code/`**: Contains the R script (`excess-mort.R`) for data preparation, modeling, and visualization.
- **`docs/`**: Contains supplementary materials (i.e., figures and references).
- **`main folder`**: Contains output files (`final-project.qmd` and `final-project.pdf`) and supplemental methods (`sup-methods.pdf` and `sup-methods.qmd`).

---
## Reproducing the Results
The results of this project can be reproduced in its entirety by running the R script `excess-mort.R` contained within the **`code/`** folder.

---

## Supplemental Materials
Refer to the `main folder` folder and look for the supplemental methods (`sup-methods.pdf` and `sup-methods.qmd`) files. These contain:
- Additional descriptions of the methodology and decisions for the analysis.
- Additional visualizations.
- A step-by-step walkthrough of how models were configured and run.

---

## `excessmort` R package
The conduct of this project relied heavily on the `excessmort` R package and the scientific manuscript that introduced the package.
- Acosta RJ, Irizarry RA. "A flexible statistical framework for estimating excess mortality." *Epidemiology.* 2022;33(5):707–716. https://doi.org/10.1097/EDE.0000000000001445

---

## Contact
- **Author**: J. Christopher Polanco
