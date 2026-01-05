
# PT demo status filters
init_pt_selects <- function(session,
                            df,
                            id_measure = "pt_measure_demo_status",
                            id_hb      = "pt_health_board_demo_status",
                            id_quarter = "pt_quarter_demo_status",
                            default_hb = NULL,
                            default_quarter = NULL) {
  # Filter once for PT
  df_demo_status_pt <- df %>% filter(dataset_type == "PT")
  
  # Compute distinct choices
  measure_choices <- sort(unique(df_demo_status_pt$measure_name))
  hb_choices      <- sort(unique(df_demo_status_pt$hb_name))
  quarter_choices <- sort(unique(df_demo_status_pt$ref_quarter_ending))
  
  # Safe defaults
  default_measure <- measure_choices[1]
  default_hb      <- if (!is.null(default_hb) && default_hb %in% hb_choices) default_hb else hb_choices[1]
  default_quarter <- if (!is.null(default_quarter) && default_quarter %in% quarter_choices) default_quarter else quarter_choices[1]
  
  updateSelectInput(session, id_measure, choices = measure_choices, selected = default_measure)
  updateSelectInput(session, id_hb,      choices = hb_choices,      selected = default_hb)
  updateSelectInput(session, id_quarter, choices = quarter_choices,  selected = default_quarter)
}


# CAMHS demo status filters
init_camhs_selects <- function(session,
                            df,
                            id_measure = "camhs_measure_demo_status",
                            id_hb      = "camhs_health_board_demo_status",
                            id_quarter = "camhs_quarter_demo_status",
                            default_hb = NULL,
                            default_quarter = NULL) {
  # Filter once for PT
  df_demo_status_camhs <- df %>% filter(dataset_type == "CAMHS")
  
  # Compute distinct choices
  measure_choices <- sort(unique(df_demo_status_camhs$measure_name))
  hb_choices      <- sort(unique(df_demo_status_camhs$hb_name))
  quarter_choices <- sort(unique(df_demo_status_camhs$ref_quarter_ending))
  
  # Safe defaults
  default_measure <- measure_choices[1]
  default_hb      <- if (!is.null(default_hb) && default_hb %in% hb_choices) default_hb else hb_choices[1]
  default_quarter <- if (!is.null(default_quarter) && default_quarter %in% quarter_choices) default_quarter else quarter_choices[1]
  
  updateSelectInput(session, id_measure, choices = measure_choices, selected = default_measure)
  updateSelectInput(session, id_hb,      choices = hb_choices,      selected = default_hb)
  updateSelectInput(session, id_quarter, choices = quarter_choices,  selected = default_quarter)
}
