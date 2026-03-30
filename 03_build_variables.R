############################################################
# 03_build_variables.R
# Build outcome, strategy index, and school quartiles
############################################################

source("R/02_load_merge.R")

# ─── Outcomes ────────────────────────────────────────────
if (!("IS3G21C" %in% names(icils))) stop("IS3G21C not found.")
icils$frequent_check      <- as.integer(make_frequent_flag(icils$IS3G21C))
icils$very_frequent_check <- as.integer(make_very_frequent_flag(icils$IS3G21C))

# ─── Weights ─────────────────────────────────────────────
if ("TOTWGTS" %in% names(icils)) {
  icils$w <- to_numeric_safe(icils$TOTWGTS)
  icils$w[is.na(icils$w)] <- 1
} else {
  icils$w <- 1
}

# ─── Safety learning proxy (student-level) ───────────────
proxy_items <- intersect(c("IS3G20CA","IS3G20CB","IS3G20CC"), names(icils))
if (length(proxy_items) >= 2) {
  zmat <- sapply(proxy_items, function(v) zscore(icils[[v]]))
  icils$safety_learning_proxy <- rowMeans(zmat, na.rm = TRUE)
} else {
  icils$safety_learning_proxy <- NA
}

# ─── School-level strategy domains ───────────────────────
gov_items <- intersect(c("II3G11A4","II3G11B2","IP3G10F","IP3G10J"), names(icils))
if (length(gov_items) >= 2) {
  icils$gov_monitor <- rowMeans(sapply(gov_items, function(v) to_binary01(icils[[v]])), na.rm = TRUE)
} else {
  icils$gov_monitor <- NA
}

icils$safe_participation <- if ("IP3G11E" %in% names(icils)) to_numeric_safe(icils$IP3G11E) else NA
icils$teacher_capacity   <- if ("IP3G12M" %in% names(icils)) to_numeric_safe(icils$IP3G12M) else NA

# Combined strategy index
bcg_signal <- rowMeans(cbind(
  zscore(icils$gov_monitor),
  zscore(icils$safe_participation),
  zscore(icils$teacher_capacity)
), na.rm = TRUE)

icils$strategy_index_raw <- ifelse(!is.na(bcg_signal), bcg_signal, icils$safety_learning_proxy)
icils$strategy_index_z   <- zscore(icils$strategy_index_raw)

# ─── School-level quartiles (within system) ──────────────
school_keys <- paste(icils$ISO3, icils$IDCNTRY, icils$IDSCHOOL, sep = "_")
school_df <- data.frame(
  school_key      = school_keys,
  ISO3            = icils$ISO3,
  strategy_school = icils$strategy_index_raw,
  stringsAsFactors = FALSE
)
school_df <- school_df[!duplicated(school_df$school_key), ]

school_df$quartile <- NA
for (iso in unique(school_df$ISO3)) {
  idx <- school_df$ISO3 == iso
  school_df$quartile[idx] <- as.character(assign_quartile(school_df$strategy_school[idx]))
}
school_df$quartile <- factor(school_df$quartile, levels = q_levels)

quartile_map <- setNames(as.character(school_df$quartile), school_df$school_key)
icils$quartile <- factor(quartile_map[school_keys], levels = q_levels)

message("Variables built. Strategy index available for ",
        sum(!is.na(icils$strategy_index_z)), " / ", nrow(icils), " rows.")
