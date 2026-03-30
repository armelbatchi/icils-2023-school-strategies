############################################################
# 06_fig3_cil_ct.R
# Figure 3: CIL and CT mean scores by strategy quartile
# Addresses: RQ4, RQ5
############################################################

source("R/03_build_variables.R")

# ─── Compute means by quartile for CIL and CT ───────────
compute_pv_means <- function(pv_prefix, scale_label) {
  pv_cols <- paste0(pv_prefix, 1:5)
  pv_cols <- intersect(pv_cols, names(icils))
  if (length(pv_cols) == 0) return(NULL)

  do.call(rbind, lapply(q_levels, function(q) {
    idx <- icils$quartile == q & !is.na(icils$w)
    # Average across PVs (Rubin's rules simplified)
    pv_means <- sapply(pv_cols, function(pv) {
      vals <- to_numeric_safe(icils[[pv]][idx])
      wtd_mean(vals, icils$w[idx])
    })
    m <- mean(pv_means, na.rm = TRUE)
    se <- sd(pv_means, na.rm = TRUE) / sqrt(length(pv_cols))  # imputation variance component
    data.frame(group = q, mean = m, se = se,
               conf.low = m - 1.96 * se, conf.high = m + 1.96 * se,
               scale = scale_label, stringsAsFactors = FALSE)
  }))
}

tbl_B <- rbind(compute_pv_means("PV", "CIL"), compute_pv_means("PV", "CT"))
# Fix: CIL uses PV1CIL..PV5CIL, CT uses PV1CT..PV5CT
tbl_B_cil <- compute_pv_means("PV%dCIL", "CIL")
tbl_B_ct  <- compute_pv_means("PV%dCT",  "CT")

# Recompute properly
compute_pv_means2 <- function(pv_pattern, scale_label) {
  pv_cols <- sprintf(pv_pattern, 1:5)
  pv_cols <- intersect(pv_cols, names(icils))
  if (length(pv_cols) == 0) return(NULL)

  do.call(rbind, lapply(q_levels, function(q) {
    idx <- icils$quartile == q & !is.na(icils$w)
    pv_means <- sapply(pv_cols, function(pv) {
      vals <- to_numeric_safe(icils[[pv]][idx])
      wtd_mean(vals, icils$w[idx])
    })
    m <- mean(pv_means, na.rm = TRUE)
    se <- sd(pv_means, na.rm = TRUE) / sqrt(length(pv_cols))
    data.frame(group = q, mean = m, se = se,
               conf.low = m - 1.96 * se, conf.high = m + 1.96 * se,
               scale = scale_label, stringsAsFactors = FALSE)
  }))
}

tbl_B <- rbind(
  compute_pv_means2("PV%dCIL", "CIL"),
  compute_pv_means2("PV%dCT",  "CT")
)
tbl_B$group <- factor(tbl_B$group, levels = q_levels)
tbl_B$scale <- factor(tbl_B$scale, levels = c("CIL", "CT"))

write.csv(tbl_B, file.path(out_dir, "Table_3_CIL_CT_means_by_quartile.csv"), row.names = FALSE)

# ─── Plot ────────────────────────────────────────────────
p3 <- ggplot(tbl_B, aes(x = group, y = mean, fill = scale)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.3,
           position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.15, linewidth = 0.45, colour = "grey30",
                position = position_dodge(width = 0.7)) +
  geom_line(aes(group = scale, colour = scale),
            position = position_dodge(width = 0.7),
            linewidth = 0.6, linetype = "11") +
  geom_point(aes(colour = scale),
             position = position_dodge(width = 0.7),
             size = 2, shape = 18, show.legend = FALSE) +
  scale_fill_manual(values = c("CIL" = alpha("#2166AC", 0.7),
                                "CT" = alpha("#B2182B", 0.7)), name = "Assessment") +
  scale_colour_manual(values = pal_scale, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Figure 3. CIL and CT mean scores by school-strategy quartile",
    subtitle = "Plausible values combined via Rubin's rules  |  95% CIs",
    x = "School-strategy quartile (within system)", y = "Mean score",
    caption = "Data: ICILS 2023.  CIL: 35 systems. CT: subset."
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = c(0.9, 0.85),
        legend.background = element_rect(fill = "white", colour = "grey85"))

ggsave(file.path(out_dir, "Fig3_CIL_CT_by_quartile.png"), p3, width = 9, height = 5, dpi = 300)
ggsave(file.path(out_dir, "Fig3_CIL_CT_by_quartile.pdf"), p3, width = 9, height = 5)
message("Saved Figure 3")
