#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#L O A D   P A C K A G E S   F O R   P I P E L I N E
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  if (!require("pacman")) {
    install.packages("pacman", repos = "https://cloud.r-project.org")
  }
  pacman::p_unload()
  pacman::p_load(
    here,
    argparse, # config
    googlesheets4,
    googledrive,
    rio,
    readxl,
    openxlsx,
    googledrive, # importing and exporting
    tidyverse,
    janitor,
    lubridate,
    rquery,
    rqdatatable,
    stringr, # manipulation
    devtools,
    cli, # analysis
    ggpubr,
    wesanderson,
    paletteer,
    leaflet,
    patchwork,
    plotly, # plotting
    devtools
  )
  Sys.setenv(R_QPDF = "true") # to build the vignette during the package build
  library(devtools) # need to install this if you do not have it
})

# --- Non-CRAN packages: install once, then only warn -------------------------
# These used to be reinstalled from GitHub on EVERY run. That made each render
# network-dependent, silently pinned the pipeline to whatever happened to be on
# GitHub main at that moment, and overwrote local development installs of
# wnvSurv mid-session.
#
# Now: install only when missing. When a local source checkout of wnvSurv is
# present, compare its newest R/ file against the installed build timestamp and
# warn if the checkout is ahead. wnvSurv's Version is static (0.0.0.9000), so a
# version comparison would never fire -- file mtime is the only signal that
# actually changes between builds.
#
# NOTE: deliberately OUTSIDE the suppressMessages() block above, or the
# "needs reinstall" warning would never reach you.

# When was the installed package built? Built field looks like:
#   "R 4.4.3; ; 2026-07-20 22:01:31 UTC; unix"
pkg_built_time <- function(pkg) {
  desc <- system.file("DESCRIPTION", package = pkg)
  if (desc == "") return(NA)
  built <- read.dcf(desc, fields = "Built")[1]
  if (is.na(built)) return(NA)
  as.POSIXct(trimws(strsplit(built, ";")[[1]][3]), tz = "UTC")
}

# Newest source file in a package checkout's R/ directory.
src_newest_time <- function(src_dir) {
  files <- list.files(file.path(src_dir, "R"), pattern = "[.][Rr]$", full.names = TRUE)
  if (length(files) == 0) return(NA)
  max(file.info(files)$mtime)
}

# Install `pkg` from `remote` only if absent. If `src_dir` is a local checkout,
# warn when it is newer than the installed build.
ensure_pkg <- function(pkg, remote, src_dir = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("Installing missing package '", pkg, "' from ", remote)
    devtools::install_github(remote, build_vignettes = TRUE)
    return(invisible(FALSE))
  }

  if (!is.null(src_dir) && dir.exists(src_dir)) {
    built <- pkg_built_time(pkg)
    newest <- src_newest_time(src_dir)
    if (!is.na(built) && !is.na(newest) && newest > built) {
      # Both are POSIXct so the comparison is absolute-time correct, but `built`
      # is parsed as UTC and mtime is local -- print both in local time or the
      # two stamps look reversed.
      tz <- Sys.timezone()
      warning(
        "\n  '", pkg, "' source in ", src_dir, " is NEWER than the installed build.\n",
        "  installed: ", format(built, tz = tz, usetz = TRUE),
        "   source: ", format(newest, tz = tz, usetz = TRUE), "\n",
        "  The pipeline is running STALE shared functions. Reinstall with:\n",
        "    R CMD INSTALL ", src_dir, "\n",
        call. = FALSE, immediate. = TRUE
      )
    }
  }
  invisible(TRUE)
}

ensure_pkg("PooledInfRate", "https://github.com/CDCgov/PooledInfRate")

# wnvSurv: single source of truth for shared surveillance functions
# (calc_pir, calc_season_week, etc). Not on CRAN, so loaded explicitly rather
# than via pacman::p_load. The local checkout lives beside this repo.
ensure_pkg("wnvSurv", "https://github.com/rtobiaskoch/wnv-ss_functions",
           src_dir = normalizePath(here::here("..", "wnv-ss_functions"),
                                   mustWork = FALSE))

suppressMessages(library(wnvSurv))

# --- Google auth account (per-user; NOT hardcoded) ---------------------------
# Pushing to the master Google Sheets (update = T) needs gargle to know WHICH
# cached account/token to use. A non-interactive `quarto render` will not prompt,
# so we resolve the email here and set `gargle_oauth_email`. Resolution order:
#   1. GARGLE_OAUTH_EMAIL env var   (each user sets this once in ~/.Renviron)
#   2. interactive prompt           (when run in RStudio / an interactive R session)
# This only SELECTS among cached tokens — every user still authenticates ONCE
# interactively to populate the cache:
#   googledrive::drive_auth(email = "<you>@gmail.com")
#   googlesheets4::gs4_auth(email  = "<you>@gmail.com")
gargle_email <- Sys.getenv("GARGLE_OAUTH_EMAIL", unset = "")
if (!nzchar(gargle_email) && interactive()) {
  gargle_email <- trimws(readline(
    "Google account email for Drive/Sheets push (blank to skip): "
  ))
}
if (nzchar(gargle_email)) {
  options(gargle_oauth_email = gargle_email)
  message("gargle OAuth email set to: ", gargle_email)
} else {
  message(
    "No Google email set (GARGLE_OAUTH_EMAIL unset and non-interactive). ",
    "A GSheet push (update = T) will fail until you set GARGLE_OAUTH_EMAIL ",
    "in ~/.Renviron and authenticate once with drive_auth()/gs4_auth()."
  )
}
