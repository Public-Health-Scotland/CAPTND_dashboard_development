

#' Build a filtered data.frame for download data button
build_df <- function(df, ds, measure) {
  df |>
    filter(dataset_type == ds,
           measure_name == measure)
}


#' Attach a download handler that writes a data.frame to CSV
attach_download_csv <- function(output, id, df, prefix) {
  
  get_df <- if (is.function(df)) df else function() df
  
  output[[id]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      write.csv(get_df(), file, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    },
    contentType = "text/csv"
  )
}



