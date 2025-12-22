
create_bar_graph <- function(data, measure_type, palette, label_name, label_title, quarter) {
  req(nrow(data) > 0) 
  
  data$measure_breakdown <- factor(data$measure_breakdown, levels = sort(unique(data$measure_breakdown)))
  
  p <- ggplot(data, aes(
    x = measure_breakdown,
    y = count,
    fill = measure_breakdown,
    text = paste(
      paste0("<b>", label_title,"<b>"),
      "<br>Quarter ending: ", quarter,
      paste0("<br>", label_name, ": "), measure_breakdown,
      paste0("<br>", measure_type, ": "), count
    )
  )) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = demo_palette, breaks = levels(data$measure_breakdown)) +
    labs(x = "SIMD Quintile", y = measure_type, fill = label_name) +
    theme_captnd() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    expand_limits(y = 0)
  
  ggplotly(p, tooltip = "text")
}

