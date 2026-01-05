

generate_non_accept_reason_text <- function(non_accept_reason_df, dataset_type, hb_name) {
  
  latest_quarter <- max(non_accept_reason_df$quarter_ending)
  date_abb <- format(latest_quarter, "%b-%y") 
  
  highest_non_accept_reason_name <- non_accept_reason_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 1) |>
    pull(measure_breakdown)
  
  highest_non_accept_reason_prop <- non_accept_reason_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 1) |>
    pull(prop)
  
  second_non_accept_reason_name <- non_accept_reason_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 2) |>
    pull(measure_breakdown)
  
  second_non_accept_reason_prop <- non_accept_reason_df |> 
    filter(quarter_ending == latest_quarter,
           rank == 2) |>
    pull(prop)
  
  
  text <- paste0(
    "<p>The following graph shows the reported reason for referral non-acceptance in ", hb_name, 
    " for the quarter ending ", as.character(date_abb), ".",
    if (highest_non_accept_reason_name == 'No data') {
      paste0(" There is no data available for ", hb_name," in the most recent quarter.")
    } else {paste0(" For ", dataset_type, ", the primary reason for referral non-acceptance in ", hb_name,
      " was recorded as '", highest_non_accept_reason_name, "' (", highest_non_accept_reason_prop, "%)",
      ifelse(highest_non_accept_reason_name == "Unsuitable",
             " - meaning that the patient is unsuitable for the service to which they have been referred. 
           A patient may be deemed unsuitable for PT because their needs would be better met within another 
           mental health service, e.g. psychiatry.",
             "."))},
    if (isTRUE(nzchar(second_non_accept_reason_name)) &&
      !identical(toupper(trimws(second_non_accept_reason_name)), "NA")) {
      paste0(
        " The next most common reason for referral non-acceptance was recorded as '",
        second_non_accept_reason_name, "' (", second_non_accept_reason_prop, "%)",
        ifelse(second_non_accept_reason_name == "Unsuitable",
               " - meaning that the patient is unsuitable for the service to which they have been referred. 
             A patient may be deemed unsuitable for PT because their needs would be better met within another 
             mental health service, e.g. psychiatry.",
               "."))
    } else { "" },
    " Please note that these are data under development and not all NHS boards are currently able to 
  record this information. This accounts for the high proportion of referrals where non-acceptance 
  reason was ‘Not recorded at board level’ or where no data had been provided.</p>")
  
  HTML(text)
}

