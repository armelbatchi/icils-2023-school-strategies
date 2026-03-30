############################################################
# 04_fig1_prevalence.R
# Figure 1: Prevalence of frequent social-media checking
#           by school-strategy quartile (pooled)
# Addresses: RQ1, RQ5
############################################################

source("R/03_build_variables.R")

# ─── Compute weighted prevalence by quartile × measure ───
compute_prev <- function(outcome_var, measure_label) {
  do.call(rbind, lapply(q_levels, function(q) {
    idx <- icils$quartile == q & !is.na(icils[[outcome_var]]) & !is.na(icils$w)
    y <- icils[[outcome_var]][idx]
    w <- icils$w[idx]
    p <- wtd_mean(y, w)
    # Bootstrap SE approximation via JRR (simplified: use normal approx)
    se <- sqrt(p * (1 - p) / sum(w > 0))
    data.frame(quartile = q, prev = p, se = se,
               conf.low = p - 1.96 * se, conf.high = p + 1.96 * se,
               measure = measure_label, stringsAsFactors = FALSE)
  }))
}

tbl_A <- rbind(
  compute_prev("frequent_check", "Often / Very often"),
  compute_prev("very_frequent_check", "Very often only")
)
tbl_A$quartile <- factor(tbl_A$quartile, levels = q_levels)
tbl_A$measure  <- factor(tbl_A$measure, levels = c("Often / Very often", "Very often only"))

write.csv(tbl_A, file.path(out_dir, "Table_1_prev_overall_by_quartile.csv"), row.names = FALSE)

# ─── Plot ────────────────────────────────────────────────
p1 <- ggplot(tbl_A, aes(x = quartile, y = prev, fill = quartile)) +
  geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                width = 0.18, linewidth = 0.45, colour = "grey30") +
  facet_wrap(~ measure) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(0, 0.85), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = pal_quartile, guide = "none") +
  labs(
    title    = "Figure 1. Prevalence of frequent social-media checking\nby school-strategy quartile",
    subtitle = "Weighted prevalence with 95% CIs  |  Pooled across 35 systems",
    x        = "School-strategy quartile (within system)",
    y        = "Prevalence",
    caption  = "Data: ICILS 2023 international database.  Outcome: IS3G21C."
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

ggsave(file.path(out_dir, "Fig1_prevalence_by_quartile.png"), p1, width = 8, height = 5.5, dpi = 300)
ggsave(file.path(out_dir, "Fig1_prevalence_by_quartile.pdf"), p1, width = 8, height = 5.5)
message("Saved Figure 1")
