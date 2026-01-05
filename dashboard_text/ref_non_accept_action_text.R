
generate_non_accept_action_text <- function(non_accept_action_df, dataset_type, hb_name) {
  
  latest_quarter <- max(non_accept_action_df$quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  highest_non_accept_action_name <- non_accept_action_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 1) |>
    pull(measure_breakdown)
  
  highest_non_accept_action_prop <- non_accept_action_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 1) |>
    pull(prop)
  
  second_non_accept_action_name <- non_accept_action_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 2) |>
    pull(measure_breakdown)
  
  second_non_accept_action_prop <- non_accept_action_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 2) |>
    pull(prop)
  
  
  text <- paste0(
    "<p>The following graph shows the actions taken by services following referral non-acceptance in ", hb_name, 
    " for the quarter ending ", as.character(date_abb), ".",
    if (highest_non_accept_action_name == 'No data') {
      paste0(" There is no data available for ", hb_name," in the most recent quarter.")
    } else {paste0(" For ", dataset_type, ", the most commonly recorded action following referral non-acceptance in ", hb_name,
                   " was '", highest_non_accept_action_name, "' (", highest_non_accept_action_prop, "%).")},
    if (isTRUE(nzchar(second_non_accept_action_name)) &&
        !identical(toupper(trimws(second_non_accept_action_name)), "NA")) {
      paste0(
        " The next most common action following referral non-acceptance was recorded as '",
        second_non_accept_action_name, "' (", second_non_accept_action_prop, "%).",
        ifelse(second_non_accept_action_name == "Signposted" | highest_non_accept_action_name == "Signposted" |
                 second_non_accept_action_name == "Returned to original referrer - with signposting" |
                 highest_non_accept_action_name == "Returned to original referrer - with signposting",
               " Signposting is where the individual referred to the service is given further information on
           where to seek help.",
               ""))
    } else { "" },
    " Please note that these are data under development and not all NHS boards are currently able to 
  record this information. This accounts for the high proportion of referrals where non-acceptance 
  reason was ‘Not recorded at board level’ or where no data had been provided.</p>")
  
  HTML(text)
}


