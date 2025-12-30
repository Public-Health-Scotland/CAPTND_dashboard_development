
generate_appt_loc_text <- function(appt_loc_df, dataset_type, hb_name) {
  
  latest_quarter <- max(appt_loc_df$app_quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  highest_appt_loc_name <-  appt_loc_df |> 
    filter(app_quarter_ending == latest_quarter,
           rank == 1) |>
    pull(measure_breakdown)
  
  highest_appt_loc_prop <-  appt_loc_df |> 
    filter(app_quarter_ending == latest_quarter,
           rank == 1) |>
    pull(prop)
  
  # second_appt_loc_name <- appt_loc_df |> 
  #   filter(quarter_ending == latest_quarter,
  #          rank == 2) |>
  #   pull(measure_breakdown)
  # 
  # second_appt_loc_prop <- appt_loc_df |> 
  #   filter(quarter_ending == latest_quarter,
  #          rank == 2) |>
  #   pull(prop)
  
  text <- paste0(
    "<p>The most common contact location recorded in which ", dataset_type, " appointments were conducted across ", 
    hb_name, " for the quarter ending ", as.character(date_abb), " , was '", highest_appt_loc_name, "'(", 
    highest_appt_loc_prop,"%).<p>") 
  
  HTML(text)
}

