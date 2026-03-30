############################################################
# 07_fig4_roc.R
# Figure 4: ROC curve + calibration + AUC metrics
# Addresses: Motivation for the full project
############################################################

source("R/03_build_variables.R")

# ─── Weighted AUC ────────────────────────────────────────
weighted_auc <- function(y, score, w) {
  ok <- !is.na(y) & !is.na(score) & !is.na(w)
  y <- ifelse(y[ok] > 0, 1, 0); score <- score[ok]; w <- w[ok]
  sp <- score[y == 1]; wp <- w[y == 1]
  sn <- score[y == 0]; wn <- w[y == 0]
  if (length(sp) == 0 || length(sn) == 0) return(NA_real_)
  ordn <- order(sn); sn <- sn[ordn]; wn <- wn[ordn]; cwn <- cumsum(wn)
  auc_num <- 0
  for (i in seq_along(sp)) {
    idx_lt <- max(which(sn < sp[i]), 0)
    w_lt <- if (idx_lt == 0) 0 else cwn[idx_lt]
    w_eq <- sum(wn[sn == sp[i]])
    auc_num <- auc_num + wp[i] * (w_lt + 0.5 * w_eq)
  }
  auc_num / (sum(wp) * sum(wn))
}

weighted_roc_points <- function(y, score, w, n_thresh = 200) {
  y <- ifelse(to_numeric_safe(y) > 0, 1, 0)
  ok <- !is.na(y) & !is.na(score) & !is.na(w)
  y <- y[ok]; score <- score[ok]; w <- w[ok]
  qs <- sort(unique(as.numeric(quantile(score, probs = seq(0, 1, length.out = n_thresh)))))
  wpos <- sum(w[y == 1]); wneg <- sum(w[y == 0])
  out <- data.frame(threshold = qs, tpr = NA_real_, fpr = NA_real_)
  for (i in seq_along(qs)) {
    pred_pos <- score >= qs[i]
    out$tpr[i] <- sum(w[y == 1 & pred_pos]) / wpos
    out$fpr[i] <- sum(w[y == 0 & pred_pos]) / wneg
  }
  out
}

# ─── Fit model ───────────────────────────────────────────
model_df <- data.frame(y = icils$frequent_check, x = icils$strategy_index_z, w = icils$w)
model_df <- model_df[complete.cases(model_df), ]

fit  <- glm(y ~ x, family = binomial(), data = model_df, weights = w)
phat <- predict(fit, type = "response")
auc_w <- weighted_auc(model_df$y, phat, model_df$w)

# ─── ROC plot ────────────────────────────────────────────
roc_pts <- weighted_roc_points(model_df$y, phat, model_df$w)

p4a <- ggplot(roc_pts, aes(x = fpr, y = tpr)) +
  geom_line(linewidth = 0.9, colour = "#2166AC") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  coord_equal() +
  labs(
    title    = "Figure 4a. ROC curve for predicting frequent social-media checking",
    subtitle = paste0("Weighted AUC = ", round(auc_w, 3), "  |  Model: y ~ school-strategy index"),
    x = "False positive rate", y = "True positive rate",
    caption = "Data: ICILS 2023.  Near-chance discrimination motivates multi-domain modelling."
  )

# ─── Calibration plot ────────────────────────────────────
logit_fn <- function(p) log(p / (1 - p))
lp <- logit_fn(pmin(pmax(phat, 1e-6), 1 - 1e-6))
cal_fit <- glm(model_df$y ~ lp, family = binomial(), weights = model_df$w)
cal_int   <- round(coef(cal_fit)[1], 3)
cal_slope <- round(coef(cal_fit)[2], 3)

# Decile calibration table
grp <- cut(phat, breaks = unique(quantile(phat, seq(0, 1, 0.1))), include.lowest = TRUE)
cal_tab <- do.call(rbind, lapply(levels(grp), function(g) {
  idx <- grp == g
  data.frame(pred_mean = wtd_mean(phat[idx], model_df$w[idx]),
             obs_rate  = wtd_mean(model_df$y[idx], model_df$w[idx]))
}))

p4b <- ggplot(cal_tab, aes(x = pred_mean, y = obs_rate)) +
  geom_point(size = 2.5, colour = "#2166AC") +
  geom_line(linewidth = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  labs(
    title    = "Figure 4b. Calibration by decile of predicted risk",
    subtitle = paste0("Slope = ", cal_slope, "  |  Intercept = ", cal_int),
    x = "Mean predicted risk", y = "Observed weighted rate"
  )

p4 <- p4a + p4b + plot_layout(ncol = 2)

ggsave(file.path(out_dir, "Fig4_ROC_calibration.png"), p4, width = 12, height = 5, dpi = 300)
ggsave(file.path(out_dir, "Fig4_ROC_calibration.pdf"), p4, width = 12, height = 5)

# Save metrics
cat("Weighted AUC =", round(auc_w, 4), "\n",
    "Calibration slope =", cal_slope, "\n",
    "Calibration intercept =", cal_int, "\n",
    file = file.path(out_dir, "Fig4_metrics.txt"))
message("Saved Figure 4")
