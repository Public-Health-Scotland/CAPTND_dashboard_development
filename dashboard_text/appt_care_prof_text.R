
generate_appt_prof_text <- function(appt_prof_df, dataset_type, hb_name) {
  
  latest_quarter <- max(appt_prof_df$app_quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  highest_appt_prof_name <-  appt_prof_df |> 
    filter(app_quarter_ending == latest_quarter,
           rank == 1) |>
    pull(measure_breakdown)
  
  highest_appt_prof_prop <-  appt_prof_df |> 
    filter(app_quarter_ending == latest_quarter,
           rank == 1) |>
    pull(prop)
  
  # second_appt_prof_name <- appt_prof_df |> 
  #   filter(quarter_ending == latest_quarter,
  #          rank == 2) |>
  #   pull(measure_breakdown)
  # 
  # second_appt_prof_prop <- appt_prof_df |> 
  #   filter(quarter_ending == latest_quarter,
  #          rank == 2) |>
  #   pull(prop)
  
  text <- paste0(
    "<p>The most common contact location recorded in which ", dataset_type, " appointments were conducted across ", 
    hb_name, " for the quarter ending ", as.character(date_abb), " , was '", highest_appt_prof_name, "'(", 
    highest_appt_prof_prop,"%).<p>") 
  
  HTML(text)
}


