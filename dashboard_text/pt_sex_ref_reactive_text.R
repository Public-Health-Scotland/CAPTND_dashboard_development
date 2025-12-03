


generate_referrals_sex_text <- function(ref_sex_pt_df, measure_type, dataset_type) {
  
  latest_quarter <- max(ref_sex_pt_df$quarter_ending)
  total_referrals <- ref_sex_pt_df[10, 5] |> pull(total)
  total_female <- ref_sex_pt_df[9, 9] |> pull(count)
  total_male <- ref_sex_pt_df[10, 9] |> pull(count)
  hb_name <- ref_sex_pt_df[1, 3] |> pull(hb_name)
  
  if (measure_type == "Number of referrals") {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(latest_quarter), ". ",
      "There were ", format(total_female, big.mark = ","), " females referred to PT services, and ",
      format(total_male, big.mark = ","), " male patients in the most recent quarter.</p>"
    )
  } else if (measure_type == "Proportion of referrals (%)") {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(latest_quarter), ". ",
      "Females made up ", format(total_female), "% of referrals to ", dataset_type, " services, with males making up the remaining ",
      format(total_male), "% of total referrals in the most recent quarter.</p>"
    )
  } else {
    text <- paste0(
      "<p>There were a total of ", format(total_referrals, big.mark = ","), 
      " referrals to ", dataset_type, " in ", hb_name, " for the quarter ending ",
      as.character(latest_quarter), ". ",
      "The referral rate for females referred to ", dataset_type, " was ", format(total_female), 
      " per 1,000 population, and for males the rate was ",
      format(total_male), " per 1,000 population in the most recent quarter.</p>"
    )
  }
  
  HTML(text)
}




