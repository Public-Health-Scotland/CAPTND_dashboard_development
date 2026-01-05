
generate_appt_prof_text <- function(appt_prof_df, dataset_type, hb_name) {
  
  latest_quarter <- max(appt_prof_df$app_quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  highest_appt_prof_name <-  appt_prof_df |> 
    filter(measure_breakdown != 'Missing data',
           app_quarter_ending == latest_quarter) |>
    slice(1) |>
    pull(measure_breakdown)
  
  if (length(highest_appt_prof_name) == 0) highest_appt_prof_name <- NA_character_
  
  highest_appt_prof_prop <-  appt_prof_df |> 
    filter(measure_breakdown != 'Missing data',
           app_quarter_ending == latest_quarter) |>
    slice(1) |>
    pull(prop)
  
  second_appt_prof_name <- appt_prof_df |>
    filter(measure_breakdown != 'Missing data',
           app_quarter_ending == latest_quarter) |>
    slice(2) |>
    pull(measure_breakdown)
  
  if (length(second_appt_prof_name) == 0) second_appt_prof_name <- NA_character_

  second_appt_prof_prop <- appt_prof_df |>
    filter(measure_breakdown != 'Missing data',
           app_quarter_ending == latest_quarter) |>
    slice(2) |>
    pull(prop)
  
  missing_data <- appt_prof_df |>
    filter(measure_breakdown == 'Missing data',
           app_quarter_ending == latest_quarter) |>
    pull(prop) 
  
  if (length(missing_data) == 0) missing_data <- NA_real_
  
  text <- paste0("<p>The following chart shows the care professionals most commonly recorded as conducting 
                 appointments in each health board across the publication period.",
    if(!is.na(highest_appt_prof_name)) {
      paste0(" The professional group most commonly recorded as conducting ", dataset_type, " appointments across ", 
    hb_name, " for the quarter ending ", as.character(date_abb), " was '", highest_appt_prof_name, "' (", 
    highest_appt_prof_prop,"%)")
    } else {paste0("")},
    
    if (!is.na(second_appt_prof_name)) {
      paste0(", followed by '", second_appt_prof_name, "' (" , second_appt_prof_prop, "%).")
  } else {paste0("")},
  
  if (!is.na(missing_data)) {
    paste0(" Professional group data were missing for ", missing_data, "% of total ", dataset_type,
           " appointments across ", hb_name, " for the most recent quarter.</p>")
  } else {paste0("")},
  "</p>")
  
  HTML(text)
}


