#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#L O A D   P A C K A G E S   F O R   P I P E L I N E 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(
    argparse, # config
    googlesheets4, googledrive, rio, readxl, openxlsx, # importing and exporting
    tidyverse, janitor, lubridate, rquery, stringr, # manipulation
    devtools, # analysis
    ggpubr, wesanderson, paletteer, leaflet, patchwork, plotly # plotting
  )
  # Only install PooledInfRate if not already installed
  if (!requireNamespace("PooledInfRate", quietly = TRUE)) {
    Sys.setenv(R_QPDF="true") # to build the vignette during the package build
    devtools::install_github("https://github.com/CDCgov/PooledInfRate", build_vignettes = TRUE)
  }
  library(PooledInfRate)
})