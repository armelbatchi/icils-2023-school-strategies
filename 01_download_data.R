############################################################
# 01_download_data.R
# Download and unzip the ICILS 2023 R International Database
############################################################

source("R/00_setup.R")

options(timeout = max(3600, getOption("timeout")))

# Check for partial downloads
if (file.exists(icils_zip_file)) {
  sz <- file.size(icils_zip_file)
  if (!is.na(sz) && sz < 60000000) {
    message("ZIP looks incomplete (", sz, " bytes). Re-downloading.")
    file.remove(icils_zip_file)
  }
}

# Download
if (!file.exists(icils_zip_file)) {
  message("Downloading ICILS 2023 R IDB to: ", icils_zip_file)
  xfun::download_file(icils_zip_url, output = icils_zip_file, mode = "wb")
}

# Unzip
if (!dir.exists(icils_unzip_dir) || length(list.files(icils_unzip_dir, recursive = TRUE)) == 0) {
  message("Unzipping to: ", icils_unzip_dir)
  dir.create(icils_unzip_dir, showWarnings = FALSE, recursive = TRUE)
  unzip(icils_zip_file, exdir = icils_unzip_dir)
}

all_files <- list.files(icils_unzip_dir, recursive = TRUE, full.names = TRUE)
message("Total files after unzip: ", length(all_files))
