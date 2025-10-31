##########################################
### CAPTND Dashboard - global.R (Alan) ###
##########################################

library(dplyr)
library(stringr)
library(lubridate)

library(forcats)
library(openxlsx)
library(tidyr)
library(arrow)
library(shiny)
library(plotly)
library(DT)

# Constants  - NEED TO MAKE THIS A USER INPUT
month_end = ymd(readline(prompt = 'Please enter latest reporting month (end of last quarter) (YYYY-MM-DD format): '))


# Relative paths ####
CAPTND_PATH             <- "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/alan/CAPTND"
CAPTND_DASHBOARD_PATH   <- "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/alan/CAPTND_dashboard_development"
OUTPUT_PATH             <- "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/output"
SHOREWISE_PUB_PATH      <- "//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/shorewise_data/publication"

Sys.setenv(
  CAPTND_PATH           = CAPTND_PATH,
  CAPTND_DASHBOARD_PATH = CAPTND_DASHBOARD_PATH,
  OUTPUT_PATH           = OUTPUT_PATH,
  SHOREWISE_PUB_PATH    = SHOREWISE_PUB_PATH
)

# Source CAPTND scripts ####
source_captnd <- function(path, ...) {
  base <- Sys.getenv("CAPTND_PATH")
  full_path <- file.path(base, path)
  owd <- getwd(); on.exit(setwd(owd), add = TRUE)
  setwd(base)
  source(path, chdir = FALSE, ...)

}

# Find latest analysis date ####

analysis_folders <- list.dirs(OUTPUT_PATH, recursive = FALSE, full.names = FALSE)
analysis_folders <- grep("^analysis_", analysis_folders, value = TRUE)

data_analysis_latest_date <- analysis_folders |>
  str_replace("analysis_", "") |>
  ymd() |>
  max(na.rm = TRUE)



# source CAPTND scripts into the global environment ####
base <- Sys.getenv("CAPTND_PATH")
stopifnot(base != "", dir.exists(base))

owd <- getwd(); on.exit(setwd(owd), add = TRUE)
setwd(base)

source_captnd("02_setup/set_dir_structure.R",                      local = globalenv())
source_captnd("07_publication/script/chapters/2_load_functions.R", local = globalenv())
source_captnd("07_publication/script/chapters/3_set_constants.R",  local = globalenv())

# Do we need this???
source("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/Ideas Space/create_captnd_dashboard_dt.R")


# Default UI values (from ref_master_df)
default_dataset <- unique(ref_master_df$dataset_type)[1]
default_hb      <- unique(ref_master_df$hb_name)[1]
default_measure <- unique(ref_master_df$measure_name)[1]
default_choices  <- unique(ref_master_df$measure_breakdown)
default_selected <- head(default_choices, 2)

message("global.R loaded successfully...!!!.")
# ============================================================
