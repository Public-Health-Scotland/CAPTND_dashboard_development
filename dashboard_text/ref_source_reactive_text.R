
generate_referral_source_text <- function(ref_source_df, dataset_type, hb_name) {
  
  latest_quarter <- max(ref_source_df$quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  highest_source_name <- ref_source_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 1) |>
    pull(measure_breakdown)
  
  highest_source_prop <-  ref_source_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 1) |>
    pull(prop)
  
  second_source_name <- ref_source_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 2) |>
    pull(measure_breakdown)
  
  second_source_prop <- ref_source_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 2) |>
    pull(prop)
  
  
    text <- paste0(
      "<p>The most common source of referral to ", dataset_type, " across ", hb_name, " for the
      quarter ending ", as.character(date_abb), " , was '", highest_source_name, "'(", highest_source_prop,
      "%), followed by '", second_source_name, "'(", second_source_prop, "%).<p>") 
  
  HTML(text)
}


