#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#L O A D   P A C K A G E S   F O R   P I P E L I N E 
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
suppressMessages({
  if (!require("pacman")) install.packages("pacman")
  pacman::p_unload()
  pacman::p_load(argparse, #config
                 googlesheets4, googledrive, rio, readxl, openxlsx, googledrive, #importing and exporting
                 tidyverse, janitor, lubridate, rquery, stringr, #manipulation
                 PooledInfRate, #analysis
                 ggpubr, wesanderson, paletteer, leaflet, patchwork# plotting
                )
})