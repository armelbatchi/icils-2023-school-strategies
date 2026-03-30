############################################################
# 05_fig2_system_rd.R
# Figure 2: System-level prevalence differences (Q4 - Q1)
# Addresses: RQ5
############################################################

source("R/03_build_variables.R")

# ─── Compute Q4-Q1 risk difference per system ────────────
sys_list <- sort(unique(icils$ISO3))
tbl_A2 <- do.call(rbind, lapply(sys_list, function(iso) {
  d <- icils[icils$ISO3 == iso & !is.na(icils$frequent_check) & !is.na(icils$quartile), ]
  q1 <- d[d$quartile == "Q1 lowest", ]
  q4 <- d[d$quartile == "Q4 highest", ]
  if (nrow(q1) < 5 || nrow(q4) < 5) return(NULL)
  p1 <- wtd_mean(q1$frequent_check, q1$w)
  p4 <- wtd_mean(q4$frequent_check, q4$w)
  data.frame(ISO3 = iso, rd_q4_minus_q1 = p4 - p1, n = nrow(d), stringsAsFactors = FALSE)
}))

tbl_A2 <- tbl_A2 %>%
  mutate(
    direction = case_when(
      rd_q4_minus_q1 > 0.01  ~ "Higher in Q4",
      rd_q4_minus_q1 < -0.01 ~ "Lower in Q4",
      TRUE ~ "Near zero"
    ),
    ISO3 = fct_reorder(ISO3, rd_q4_minus_q1)
  )

write.csv(tbl_A2, file.path(out_dir, "Table_2_prev_by_system_Q4minusQ1.csv"), row.names = FALSE)

# ─── Plot ────────────────────────────────────────────────
p2 <- ggplot(tbl_A2, aes(x = rd_q4_minus_q1, y = ISO3, colour = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4) +
  geom_segment(aes(x = 0, xend = rd_q4_minus_q1, y = ISO3, yend = ISO3), linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_x_continuous(labels = label_number(accuracy = 0.01, suffix = " pp", scale = 100)) +
  scale_colour_manual(
    values = c("Higher in Q4" = "#B2182B", "Lower in Q4" = "#2166AC", "Near zero" = "grey50"),
    name = "Direction"
  ) +
  labs(
    title    = "Figure 2. System-level prevalence differences (Q4 \u2212 Q1)",
    subtitle = "Risk difference: prevalence(Q4) \u2212 prevalence(Q1)\nPositive = higher prevalence in highest-strategy schools",
    x = "Risk difference (percentage points)", y = NULL,
    caption = "Data: ICILS 2023.  Outcome: often/very often on IS3G21C."
  ) +
  theme(legend.position = c(0.82, 0.12),
        legend.background = element_rect(fill = "white", colour = "grey85"),
        panel.grid.major.y = element_blank())

ggsave(file.path(out_dir, "Fig2_system_prevalence_Q4minusQ1.png"), p2, width = 8, height = 9, dpi = 300)
ggsave(file.path(out_dir, "Fig2_system_prevalence_Q4minusQ1.pdf"), p2, width = 8, height = 9)
message("Saved Figure 2")
