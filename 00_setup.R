############################################################
# 00_setup.R
# Packages, paths, and publication-quality ggplot2 theme
############################################################

# ─── Packages ────────────────────────────────────────────
pkgs <- c("ggplot2", "dplyr", "tidyr", "forcats", "scales",
          "patchwork", "kableExtra", "ggrepel", "RColorBrewer", "xfun")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
invisible(lapply(pkgs, library, character.only = TRUE))

# ─── Paths ───────────────────────────────────────────────
work_dir  <- file.path(getwd(), "data")
out_dir   <- file.path(getwd(), "outputs")
dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

icils_zip_url   <- "https://www.iea.nl/sites/default/files/data-repository/ICILS/ICILS2023/ICILS2023_IDB_R.zip"
icils_zip_file  <- file.path(work_dir, "ICILS2023_IDB_R.zip")
icils_unzip_dir <- file.path(work_dir, "ICILS2023_IDB_R")

# Optional: restrict to specific ISO3 systems (NULL = all)
countries_keep <- NULL

# ─── Publication theme ───────────────────────────────────
theme_pub <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      text              = element_text(family = "sans", colour = "grey20"),
      plot.title        = element_text(size = rel(1.15), face = "bold",
                                       hjust = 0, margin = margin(b = 6)),
      plot.subtitle     = element_text(size = rel(0.85), colour = "grey40",
                                       hjust = 0, margin = margin(b = 10)),
      plot.caption      = element_text(size = rel(0.7), colour = "grey50",
                                       hjust = 1, margin = margin(t = 8)),
      axis.title        = element_text(size = rel(0.9)),
      axis.text         = element_text(size = rel(0.8)),
      strip.text        = element_text(size = rel(0.9), face = "bold"),
      legend.title      = element_text(size = rel(0.85), face = "bold"),
      legend.text       = element_text(size = rel(0.8)),
      panel.grid.major  = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.minor  = element_blank(),
      plot.margin       = margin(12, 12, 12, 12)
    )
}
theme_set(theme_pub())

# ─── Colour palettes ────────────────────────────────────
pal_quartile <- c("Q1 lowest" = "#3B4CC0", "Q2" = "#7B9EF0",
                  "Q3" = "#F4A582", "Q4 highest" = "#B2182B")
pal_scale    <- c("CIL" = "#2166AC", "CT" = "#B2182B")
q_levels     <- c("Q1 lowest", "Q2", "Q3", "Q4 highest")
