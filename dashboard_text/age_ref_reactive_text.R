

generate_referrals_age_text <- function(ref_age_df, selected_measure, dataset_type, hb_name) {
  
  latest_quarter <- max(ref_age_df$quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  total_referrals <- ref_age_df |>
    filter(quarter_ending == latest_quarter) |> slice(1) |> pull(total)
  
  highest_count_fig <- ref_age_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(count)
  
  highest_count_age_group <- ref_age_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(measure_breakdown)
  
  highest_prop_fig <- ref_age_df |> 
    filter(quarter_ending == latest_quarter) |>arrange(desc(count)) |> slice(1) |> pull(count)
  
  highest_prop_age_group <- ref_age_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(measure_breakdown)
  
  highest_rate_fig <- ref_age_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(count)
  
  highest_rate_age_group <- ref_age_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(measure_breakdown)
  
  if (selected_measure == "Number of referrals") {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(date_abb), ". ", dataset_type,
      " referrals were highest amongst those in the ", highest_count_age_group, " age group, with ",
      format(highest_count_fig, big.mark = ","), " referrals recieved in ", hb_name, " for the most recent quarter.</p>"
    )
  } else if (selected_measure == "Proportion of referrals (%)") {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(date_abb), ". ",
      "The highest proportion of ", dataset_type, " referrals were for patients in the ", 
      highest_prop_age_group, " age group, making up ", highest_prop_fig,
      "% of total referrals in ", hb_name, " for the most recent quarter.</p>"
    )
  } else {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(date_abb), ". ",
      "The ", dataset_type, " referral rate was highest for patients in the ", 
      highest_rate_age_group , " age group in ", hb_name, ", with ", highest_rate_fig, 
      " referrals received per 1,000 population in the most recent quarter.</p>"
    )
  }
  
  HTML(text)
}


