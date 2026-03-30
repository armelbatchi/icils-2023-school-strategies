# School strategies linked to safer and more balanced digital behavior

Reproducible analysis code for the IEA Research & Policy Grant project using ICILS 2023 data.

## Repository structure

```
R/
  00_setup.R              # Packages, paths, global theme
  01_download_data.R      # Download and unzip ICILS 2023 IDB
  02_load_merge.R         # Load BSG/BCG files, merge student + school
  03_build_variables.R    # Outcome, strategy index, quartiles
  04_fig1_prevalence.R    # Figure 1: Prevalence by strategy quartile
  05_fig2_system_rd.R     # Figure 2: System-level Q4-Q1 differences
  06_fig3_cil_ct.R        # Figure 3: CIL/CT by strategy quartile
  07_fig4_roc.R           # Figure 4: ROC + calibration (AUC)
  08_fig5_forest.R        # Figure 5: Forest plot of system ORs
  09_fig6_heatmap.R       # Figure 6: Heatmap of prevalence by system x quartile
  10_render_all.R         # Master script to run everything
                          # ICILS data downloaded here (gitignored)
                          # Figures saved here
```

## Requirements

- R ≥ 4.3
- Packages: `ggplot2`, `dplyr`, `tidyr`, `forcats`, `scales`, `patchwork`, `kableExtra`, `ggrepel`, `RColorBrewer`
- ICILS 2023 R IDB (downloaded automatically by `01_download_data.R`)

## Quick start

```r
source("R/11_render_all.R")
```

## Figure–proposal mapping

| Figure | Proposal reference | RQ addressed |
|--------|-------------------|--------------|
| Figure 1 | Prevalence of frequent social-media checking by quartile | RQ1, RQ5 |
| Figure 2 | System-level prevalence differences (Q4 – Q1) | RQ5 |
| Figure 3 | CIL and CT mean scores by strategy quartile | RQ4, RQ5 |
| Figure 4 | ROC curve and predictive metrics (AUC ≈ 0.52) | Motivation |
| Figure 5 | Forest plot of odds ratios by system | RQ1, RQ5 |
| Figure 6 | Heatmap of prevalence by system × quartile | RQ5 |

## Data access

ICILS 2023 public-use files: <https://doi.org/10.58150/ICILS_2023_data>

## License

Code: MIT. Data: subject to IEA terms of use.
