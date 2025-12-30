

generate_referrals_simd_text <- function(ref_simd_df, selected_measure, dataset_type, hb_name) {
  
  latest_quarter <- max(ref_simd_df$quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  total_referrals <- ref_simd_df |>
    filter(quarter_ending == latest_quarter) |> slice(1) |> pull(total)
  
  highest_count_fig <- ref_simd_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(count)
  
  highest_count_simd <- ref_simd_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> 
    transmute(measure_breakdown = str_extract(as.character(measure_breakdown), "^\\d+")) |> pull(measure_breakdown)
  
  highest_prop_fig <- ref_simd_df |> 
    filter(quarter_ending == latest_quarter) |>arrange(desc(count)) |> slice(1) |> pull(count)
  
  highest_prop_simd <- ref_simd_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |>
    transmute(measure_breakdown = str_extract(as.character(measure_breakdown), "^\\d+")) |> pull(measure_breakdown) 
  
  highest_rate_fig <- ref_simd_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |> pull(count)
  
  highest_rate_simd <- ref_simd_df |> 
    filter(quarter_ending == latest_quarter) |> arrange(desc(count)) |> slice(1) |>
    transmute(measure_breakdown = str_extract(as.character(measure_breakdown), "^\\d+")) |> pull(measure_breakdown) 
  
  if (selected_measure == "Number of referrals") {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(date_abb), ". ", dataset_type,
      " referrals were highest amongst those in deprivation quintile ", highest_count_simd, " , with ",
      format(highest_count_fig, big.mark = ","), " referrals recieved for patients living in this quintile 
      based on postcode at referral in ", hb_name, " for the most recent quarter. Deprivation quintile 1
      represents the most deprived 20% of neighbourhhods across Scotland, with deprivation quintile 5 the least
      deprived 20% of neighbourhoods. Across NHSScotland, there appears a higher demand for ", dataset_type, 
      " service in areas of higher deprivation. Please note that NHS board areas may not have all five quintiles
      represented.</p>"
    )
  } else if (selected_measure == "Proportion of referrals (%)") {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(date_abb), ". ",
      "The highest proportion of ", dataset_type, " referrals were for patients living in deprivation quintile ", 
      highest_prop_simd, ", making up ", highest_prop_fig,
      "% of total referrals in ", hb_name, " for the most recent quarter. Deprivation quintile 1
      represents the most deprived 20% of neighbourhhods across Scotland, with deprivation quintile 5 the least
      deprived 20% of neighbourhoods. Across NHSScotland, there appears a higher demand for ", dataset_type, 
      " service in areas of higher deprivation. Please note that NHS board areas may not have all five quintiles
      represented.</p>"
    )
  } else {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(date_abb), ". ",
      "The ", dataset_type, " referral rate was highest for patients living in deprivation quintile ", 
      highest_rate_simd , " in ", hb_name, ", with ", highest_rate_fig, 
      " referrals received per 1,000 population in the most recent quarter. Deprivation quintile 1
      represents the most deprived 20% of neighbourhhods across Scotland, with deprivation quintile 5 the least
      deprived 20% of neighbourhoods. Across NHSScotland, there appears a higher demand for ", dataset_type, 
      " service in areas of higher deprivation. Please note that NHS board areas may not have all five quintiles
      represented.</p>"
    )
  }
  
  HTML(text)
}



