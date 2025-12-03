


create_line_graph <- function(data, measure_type, palette, quarters_in_data, label_name, label_title) {
  req(nrow(data) > 0) 
  
  data$measure_breakdown <- factor(data$measure_breakdown, levels = sort(unique(data$measure_breakdown)))
  
  p <- ggplot(data, aes(
    x = quarter_ending,
    y = count,
    group = measure_breakdown,
    color = measure_breakdown,
    text = paste(
      paste0("<b>", label_title,"<b>"),
      "<br>Quarter ending: ", quarter_ending,
      paste0("<br>", label_name, ": "), measure_breakdown,
      paste0("<br>", measure_type, ": "), count
    )
  )) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = palette, breaks = levels(data$measure_breakdown)) +
    scale_x_date(breaks = quarters_in_data, labels = function(x) format(x, "%b-%y")) +
    labs(x = "Quarter Ending", y = measure_type, color = label_name) +
    theme_captnd() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    expand_limits(y = 0)
  
  ggplotly(p, tooltip = "text")
}


