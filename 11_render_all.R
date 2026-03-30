############################################################
# 11_render_all.R
# Master script: run all analyses in order
############################################################

cat("=== ICILS 2023 Preliminary Analyses ===\n")
cat("=== School strategies & digital behavior ===\n\n")

# Each script sources its dependencies, so we run them
# sequentially. The data loading (01-03) is cached: once
# icils is built in 03, subsequent scripts reuse it.

cat("[1/6] Figure 1: Prevalence by quartile\n")
source("R/04_fig1_prevalence.R")

cat("[2/6] Figure 2: System Q4-Q1 differences\n")
source("R/05_fig2_system_rd.R")

cat("[3/6] Figure 3: CIL/CT by quartile\n")
source("R/06_fig3_cil_ct.R")

cat("[4/6] Figure 4: ROC + calibration\n")
source("R/07_fig4_roc.R")

cat("[5/6] Figure 5: Forest plot of ORs\n")
source("R/08_fig5_forest.R")

cat("[6/6] Figure 6: Heatmap\n")
source("R/09_fig6_heatmap.R")

cat("\n=== All outputs in: ", out_dir, " ===\n")
cat("Files:\n")
print(list.files(out_dir, full.names = FALSE))
