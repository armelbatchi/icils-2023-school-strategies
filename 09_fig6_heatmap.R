############################################################
# 09_fig6_heatmap.R
# Figure 6: Heatmap of prevalence by system x quartile
# Addresses: RQ5
############################################################

source("R/03_build_variables.R")

# ─── Compute prevalence grid ─────────────────────────────
iso_levels <- sort(unique(icils$ISO3))
heat_dat <- expand.grid(ISO3 = iso_levels, quartile = q_levels, stringsAsFactors = FALSE)
heat_dat$prev <- NA_real_

for (i in seq_len(nrow(heat_dat))) {
  iso <- heat_dat$ISO3[i]
  q   <- heat_dat$quartile[i]
  idx <- icils$ISO3 == iso & icils$quartile == q &
         !is.na(icils$frequent_check) & !is.na(icils$w)
  if (sum(idx) > 0) heat_dat$prev[i] <- wtd_mean(icils$frequent_check[idx], icils$w[idx])
}

# ─── Plot ────────────────────────────────────────────────
p6 <- ggplot(heat_dat, aes(x = quartile, y = ISO3, fill = prev)) +
  geom_tile(colour = "white", linewidth = 0.3) +
  scale_fill_distiller(palette = "RdYlBu", direction = -1,
                       na.value = "grey90",
                       labels = percent_format(accuracy = 1),
                       limits = c(0.4, 0.95),
                       name = "Prevalence") +
  labs(
    title    = "Figure 6. Frequent social-media checking by system and strategy quartile",
    subtitle = "Weighted prevalence (often/very often on IS3G21C)",
    x = "School-strategy quartile (within system)", y = NULL,
    caption = "Data: ICILS 2023.  Grey cells = insufficient data."
  ) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        axis.text.y = element_text(size = rel(0.75)))

ggsave(file.path(out_dir, "Fig6_heatmap.png"), p6, width = 9, height = 10, dpi = 300)
ggsave(file.path(out_dir, "Fig6_heatmap.pdf"), p6, width = 9, height = 10)
message("Saved Figure 6")
