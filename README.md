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
1. Open this folder in RStudio (R 4.3 or later).
2. Knit `HDS_MM_GARG_2611.Rmd`. Missing packages are installed, and the 15 raw NHANES files are downloaded from the CDC into `data_raw/` if not present.
3. All outputs are rewritten to `data_ard/`. The build stops if any integrity check fails.

Raw CDC files are not included; they are public and downloaded by the script.
