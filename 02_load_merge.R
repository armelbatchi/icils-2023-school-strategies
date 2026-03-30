############################################################
# 02_load_merge.R
# Helper functions, load BSG/BCG files, merge student + school
############################################################

source("R/01_download_data.R")

# ─── Helper functions ────────────────────────────────────

load_iea_r <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") return(as.data.frame(readRDS(path)))
  if (ext %in% c("rda", "rdata")) {
    e <- new.env(parent = emptyenv())
    nm <- load(path, envir = e)
    for (name in nm) if (is.data.frame(e[[name]])) return(e[[name]])
    stop("No data.frame in: ", path)
  }
  stop("Unsupported: ", path)
}

get_iso3_from_filename <- function(f) {
  nm <- toupper(basename(f))
  m <- regexec("^[A-Z]{3}([A-Z]{3})I3\\.", nm)
  reg <- regmatches(nm, m)[[1]]
  if (length(reg) >= 2) return(reg[2])
  NA_character_
}

rbind_fill <- function(dfs) {
  all_names <- unique(unlist(lapply(dfs, names)))
  dfs2 <- lapply(dfs, function(df) {
    miss <- setdiff(all_names, names(df))
    for (m in miss) df[[m]] <- NA
    df[all_names]
  })
  do.call(rbind, dfs2)
}

to_numeric_safe <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  suppressWarnings(as.numeric(x))
}

make_frequent_flag <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(tolower(trimws(x)) %in% c("often", "very often"))
  to_numeric_safe(x) >= 3
}

make_very_frequent_flag <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) return(tolower(trimws(x)) == "very often")
  to_numeric_safe(x) >= 4
}

wtd_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

zscore <- function(x) {
  xn <- to_numeric_safe(x)
  (xn - mean(xn, na.rm = TRUE)) / sd(xn, na.rm = TRUE)
}

to_binary01 <- function(x) {
  xn <- to_numeric_safe(x)
  if (all(xn %in% c(0, 1, NA), na.rm = TRUE)) return(xn)
  if (all(xn %in% c(1, 2, NA), na.rm = TRUE)) return(ifelse(xn == 1, 1, ifelse(xn == 2, 0, NA)))
  ifelse(is.na(xn), NA, ifelse(xn > 0, 1, 0))
}

assign_quartile <- function(strategy_vec) {
  if (sum(!is.na(strategy_vec)) < 10) return(rep(NA, length(strategy_vec)))
  qs <- quantile(strategy_vec, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
  cut(strategy_vec,
      breaks = c(-Inf, qs[1], qs[2], qs[3], Inf),
      labels = q_levels, include.lowest = TRUE, right = TRUE)
}

# ─── Find files ──────────────────────────────────────────

bsg_files <- all_files[grepl("^BSG[A-Z]{3}I3\\.(rds|rda|RData)$",
                             basename(all_files), ignore.case = TRUE)]
bcg_files <- all_files[grepl("^BCG[A-Z]{3}I3\\.(rds|rda|RData)$",
                             basename(all_files), ignore.case = TRUE)]

if (length(bsg_files) == 0) stop("No BSG files found in: ", icils_unzip_dir)

iso3_bsg <- sort(unique(na.omit(vapply(bsg_files, get_iso3_from_filename, character(1)))))
if (!is.null(countries_keep)) iso3_bsg <- iso3_bsg[iso3_bsg %in% countries_keep]

bsg_map <- setNames(bsg_files, vapply(bsg_files, get_iso3_from_filename, character(1)))
bcg_map <- setNames(bcg_files, vapply(bcg_files, get_iso3_from_filename, character(1)))

# ─── Load and merge ──────────────────────────────────────

student_keep <- c("IDCNTRY","CNTRY","IDSCHOOL","IDSTUD",
                  "IS3G21A","IS3G21B","IS3G21C",
                  "IS3G20CA","IS3G20CB","IS3G20CC",
                  "TOTWGTS",
                  "PV1CIL","PV2CIL","PV3CIL","PV4CIL","PV5CIL",
                  "PV1CT","PV2CT","PV3CT","PV4CT","PV5CT")

school_keep <- c("IDCNTRY","CNTRY","IDSCHOOL",
                 "II3G11A4","II3G11B2","IP3G10F","IP3G10J","IP3G11E","IP3G12M",
                 "TOTWGTC")

merged_list <- list()

for (iso in iso3_bsg) {
  message("Loading: ", iso)
  bsg_path <- bsg_map[[iso]]
  if (is.null(bsg_path)) next
  bsg <- load_iea_r(bsg_path)
  bsg <- bsg[intersect(student_keep, names(bsg))]
  bsg$ISO3 <- iso

  bcg <- NULL
  bcg_path <- bcg_map[[iso]]
  if (!is.null(bcg_path)) {
    bcg0 <- load_iea_r(bcg_path)
    bcg0 <- bcg0[intersect(school_keep, names(bcg0))]
    if (all(c("IDCNTRY","IDSCHOOL") %in% names(bcg0))) {
      bcg <- bcg0
      bcg$ISO3 <- iso
    }
  }

  if (!is.null(bcg)) {
    bsg$key <- paste(bsg$IDCNTRY, bsg$IDSCHOOL, sep = "_")
    bcg$key <- paste(bcg$IDCNTRY, bcg$IDSCHOOL, sep = "_")
    m <- merge(bsg, bcg, by = "key", all.x = TRUE, suffixes = c("_S","_C"))
    if (!("IDCNTRY" %in% names(m)) && ("IDCNTRY_S" %in% names(m))) m$IDCNTRY <- m$IDCNTRY_S
    if (!("IDSCHOOL" %in% names(m)) && ("IDSCHOOL_S" %in% names(m))) m$IDSCHOOL <- m$IDSCHOOL_S
    if (!("ISO3" %in% names(m)) && ("ISO3_S" %in% names(m))) m$ISO3 <- m$ISO3_S
    merged_list[[iso]] <- m
  } else {
    merged_list[[iso]] <- bsg
  }
}

icils <- rbind_fill(merged_list)
message("Merged: ", nrow(icils), " rows × ", ncol(icils), " cols")
