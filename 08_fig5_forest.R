############################################################
# 08_fig5_forest.R
# Figure 5: Forest plot of system-level odds ratios
# Addresses: RQ1, RQ5
############################################################

source("R/03_build_variables.R")

# ─── Compute OR per system ───────────────────────────────
sys_list <- sort(unique(icils$ISO3))

tbl_C <- do.call(rbind, lapply(sys_list, function(iso) {
  d <- icils[icils$ISO3 == iso & !is.na(icils$very_frequent_check) &
               !is.na(icils$strategy_index_z) & !is.na(icils$w), ]
  if (nrow(d) < 30) return(NULL)
  fit <- tryCatch(
    glm(very_frequent_check ~ strategy_index_z, family = binomial(),
        data = d, weights = w),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  ci <- tryCatch(confint.default(fit), error = function(e) NULL)
  if (is.null(ci)) return(NULL)
  or <- exp(coef(fit)["strategy_index_z"])
  data.frame(ISO3 = iso, OR = or,
             conf.low = exp(ci["strategy_index_z", 1]),
             conf.high = exp(ci["strategy_index_z", 2]),
             n = nrow(d), stringsAsFactors = FALSE)
}))

tbl_C <- tbl_C %>%
  mutate(
    sig  = ifelse(conf.low > 1 | conf.high < 1, "Significant", "Non-significant"),
    ISO3 = fct_reorder(ISO3, OR)
  )

write.csv(tbl_C, file.path(out_dir, "Table_4_OR_by_system.csv"), row.names = FALSE)

# ─── Plot ────────────────────────────────────────────────
p5 <- ggplot(tbl_C, aes(x = OR, y = ISO3)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  annotate("rect", xmin = 0.6, xmax = 1, ymin = -Inf, ymax = Inf,
           fill = "#2166AC", alpha = 0.04) +
  annotate("rect", xmin = 1, xmax = 1.4, ymin = -Inf, ymax = Inf,
           fill = "#B2182B", alpha = 0.04) +
  geom_segment(aes(x = conf.low, xend = conf.high, y = ISO3, yend = ISO3, colour = sig),
               linewidth = 0.5) +
  geom_point(aes(colour = sig), size = 2.2) +
  scale_x_log10(breaks = c(0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3),
                labels = label_number(accuracy = 0.1)) +
  scale_colour_manual(values = c("Significant" = "grey15", "Non-significant" = "grey65"), name = "") +
  annotate("text", x = 0.72, y = 1, label = "\u2190 Lower odds\n(protective)",
           size = 2.8, colour = "#2166AC", fontface = "italic", hjust = 0) +
  annotate("text", x = 1.28, y = 1, label = "Higher odds \u2192\n(risk)",
           size = 2.8, colour = "#B2182B", fontface = "italic", hjust = 1) +
  labs(
    title    = "Figure 5. Odds ratios for very-frequent social-media checking\nper +1 SD in school-strategy index, by system",
    subtitle = "Weighted logistic regression (IS3G21C = 'very often')  |  95% CIs  |  Log scale",
    x = "Odds ratio (log scale)", y = NULL,
    caption  = "Data: ICILS 2023.  Each system estimated separately."
  ) +
  theme(legend.position = "top", legend.justification = "left",
        panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "Fig5_forest_OR.png"), p5, width = 9, height = 10, dpi = 300)
ggsave(file.path(out_dir, "Fig5_forest_OR.pdf"), p5, width = 9, height = 10)
message("Saved Figure 5")
