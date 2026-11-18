# HDS_MM_GARG_2611 — diabetic foot referral dataset (NHANES 1999–2004)

## Contents
- `HDS_MM_GARG_2611.Rmd` — the report source. Knitting it runs the whole pipeline.
- `HDS_MM_GARG_2611.html` — the knitted report.
- `01_build_dataset.R` — builds all three data layers from the raw CDC files and runs the integrity checks.
- `figures/pipeline.png` — Figure 1.
- `data_ard/`
  - `layer1_linked.rds` — Layer 1: all 31,126 sampled people, source files joined.
  - `layer2_curated.rds` — Layer 2: 9,145 examined people aged 40+, foot checks derived, diabetes as a flag.
  - `foot_risk_ard.csv` / `.rds` — Layer 3: the analysis-ready dataset, 1,627 people with diabetes.
  - `foot_risk_full.rds` — Layer 3 with all source and derived columns.
  - `data_dictionary.csv` — metadata for the analysis-ready dataset.
  - `cleaning_log.csv` — every cleaning and derivation decision, with counts.
  - `attrition.csv` — people remaining at each step.

## To reproduce
1. Open this folder in RStudio (R 4.3 or later; last built with R 4.5.3).
2. Install the report theme once, since it is needed before knitting starts:
   `install.packages("rmdformats")`
3. Knit `HDS_MM_GARG_2611.Rmd`. Any other missing packages (haven, dplyr, purrr, tibble, tidyr, ggplot2, survey, knitr, DT) are installed, and the 15 raw NHANES files are downloaded from the CDC into `data_raw/` if not present.
4. All outputs are rewritten to `data_ard/`. The build stops if any integrity check fails.

To rebuild the datasets without the report, run `Rscript 01_build_dataset.R` from this folder.

Raw CDC files are not included; they are public and downloaded by the script. Exact package versions used are listed in the report's appendix (A9, R session information).

## Data source
Centers for Disease Control and Prevention, National Center for Health Statistics. National Health and Nutrition Examination Survey (NHANES) 1999–2004 public-use data. https://wwwn.cdc.gov/nchs/nhanes/

## Licence
- Code (`.R`, `.Rmd`): MIT, see `LICENSE`.
- Derived datasets and metadata (`data_ard/`) and the report: CC BY 4.0 (https://creativecommons.org/licenses/by/4.0/). The underlying NHANES data are US federal public-use data.

This is a research classification, not a validated clinical decision tool.
