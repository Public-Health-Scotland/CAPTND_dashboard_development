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
library(shinydashboard)
library(rlang)
conflicted::conflicts_prefer(shinydashboard::box)
conflicted::conflicts_prefer(dplyr::filter)

# Constants  - NEED TO MAKE THIS A USER INPUT
#month_end = ymd(readline(prompt = 'Please enter latest reporting month (end of last quarter) (YYYY-MM-DD format): '))
month_end <- as.Date('2025-10-01')

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

#load data
source("//PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_data/create_captnd_dashboard_dt.R")
#load graph functions
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/graph_functions/create_line_graph_function.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/graph_functions/create_horizontal_bar_graph_function.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/graph_functions/create_new_horz_bar_graph_function.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/graph_functions/create_bar_graph_function.R")
#load text inputs
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/sex_ref_reactive_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/age_ref_reactive_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/simd_ref_reactive_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/ref_source_reactive_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/ref_non_accept_reason_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/ref_non_accept_action_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/appt_loc_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/appt_care_prof_text.R")
source("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/text_chunks.R")

#Markdown for large text sections
#render .Rmd file for text in 'Home' tab 
rmarkdown::render("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/captnd_dashboard_intro.Rmd",
                  output_format = "html_fragment", 
                  output_file = "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/captnd_dashboard_intro.html")

#render .Rmd file for referral intro
#PT
rmarkdown::render("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/captnd_db_referrals_text.Rmd",
                  params = list(
                    dataset_choice = "PT",
                    hb_choice = "NHSScotland"),
                  envir = globalenv(),
                  output_format = "html_fragment", 
                  output_file = "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/captnd_db_ref_text_pt.html")
#CAMHS
rmarkdown::render("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/captnd_db_referrals_text.Rmd",
                  params = list(
                    dataset_choice = "CAMHS",
                    hb_choice = "NHSScotland"),
                  envir = globalenv(),
                  output_format = "html_fragment", 
                  output_file = "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/captnd_db_ref_text_camhs.html")

#render .Rmd for glossary
rmarkdown::render("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/captnd_dashboard_glossary.Rmd",
                  output_format = "html_fragment", 
                  output_file = "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/captnd_dashboard_glossary.html")

#render .Rmd for PT metadata
rmarkdown::render("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/captnd_dashboard_pt_metadata.Rmd",
                  output_format = "html_fragment", 
                  output_file = "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/captnd_dashboard_pt_metadata.html")

#render .Rmd for CAMHS metadata
rmarkdown::render("/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/dashboard_text/captnd_dashboard_camhs_metadata.Rmd",
                  output_format = "html_fragment", 
                  output_file = "/PHI_conf/MentalHealth5/CAPTND/CAPTND_shorewise/scripts/luke/captnd_dashboard/captnd_dashboard_camhs_metadata.html")


# Default UI values 
default_dataset <- unique(ref_master_df$dataset_type)[1]
default_hb <- 'NHSScotland'
default_measure <- ref_master_df %>% filter(dataset_type == default_dataset, hb_name == default_hb) %>% pull(measure_name) %>% unique() %>% first()
default_quarter <- ref_master_df %>% filter(dataset_type == 'PT') %>% pull(quarter_ending) %>% max()

message("global.R loaded successfully...!!!.")
# ============================================================
