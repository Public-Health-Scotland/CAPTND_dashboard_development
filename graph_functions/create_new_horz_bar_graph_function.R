

create_new_horz_bar_graph <- function(data, label_name, measure_title, label_title, xaxis_name, quarter) {
  
  label_order <- data$measure_breakdown
  ifelse(any(label_order == "Not known"),
         label_order <- c(label_order[-which(label_order == "Not known")], "Not known"), "") # put not known and missing to the bottom of the plot
  ifelse(any(label_order == "Missing data"),
         label_order <- c(label_order[-which(label_order == "Missing data")], "Missing data"), "")
  
  req(nrow(data) > 0)
  
  pad <- 0.04 * max(data$count, na.rm = TRUE)  
  
  threshold <- 5.0
  
  data <- data |>
    mutate(label = prop,
           label = ifelse(label >= threshold, label, ""))
  
  p <- data |>
    mutate(measure_breakdown = factor(measure_breakdown, levels = label_order)) |>
    ggplot(aes(
      x = fct_rev(measure_breakdown),
      y = count,
      text = paste(
        paste0("<b>", label_title ,"<b>"), #label_title
        "<br>Quarter ending: ", quarter,
        paste0("<br>", label_name, " : "), measure_breakdown, #label_name
        paste0("<br>", measure_title,": "), count, "(",prop, "%)")
    )) +
    geom_col(width = 0.7, fill = "#3F3685") +
    geom_text(aes(y = pmax(0, count - pad),
                  label = ifelse(label != "", paste0(label, "%"), NA)), 
              hjust = 1.2, 
              colour = "white", 
              size = 10/.pt,
              na.rm = TRUE) +
    coord_flip() +
    labs(x = xaxis_name, y = measure_title) + 
    theme_captnd() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    expand_limits(y = 0)
  
  ggplotly(p, tooltip = "text")
  
}