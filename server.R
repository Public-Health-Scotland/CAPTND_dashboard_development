#### Server ####
server <- function(input, output, session) {
  
  # Update Health Board dropdown dynamically for ref_accept_df
  observe({
    hb_choices <- ref_accept_df %>%
      filter(dataset_type == input$dataset_type_accept) %>%
      pull(hb_name) %>%
      unique()
    updateSelectInput(session, "health_board_accept", choices = hb_choices, selected = hb_choices[1])
  })
  
  # Update Health Board dropdown dynamically for ref_master_df
  observe({
    hb_choices <- ref_master_df %>%
      filter(dataset_type == input$dataset_type_master) %>%
      pull(hb_name) %>%
      unique()
    updateSelectInput(session, "health_board_master", choices = hb_choices, selected = hb_choices[1])
  })
  
  # Update Health Board dropdown dynamically for master_appts_df
  observe({
    hb_choices <- master_appts_df %>%
      filter(dataset_type == input$dataset_type_appts) %>%
      pull(hb_name) %>%
      unique()
    updateSelectInput(session, "health_board_appts", choices = hb_choices, selected = hb_choices[1])
  })
  
  
  observeEvent(
    list(input$dataset_type_master, input$health_board_master, input$demographic_type_master),
    ignoreInit = TRUE,
    {
      bd_choices <- ref_master_df %>%
        filter(
          dataset_type == input$dataset_type_master,
          hb_name      == input$health_board_master,
          measure_name == input$demographic_type_master
        ) %>%
        pull(measure_breakdown) %>%
        unique() %>%
        sort()
      
      bd_choices <- bd_choices[!is.na(bd_choices)]
      
      
      preferred <- intersect(c("Female", "Male"), bd_choices)
      if (length(preferred) == 0) {
        selected <- head(bd_choices, 2)   
      } else {
        selected <- preferred
      }
      
      updateSelectInput(
        session,
        "demographic_options_master",
        choices  = bd_choices,
        selected = selected
      )
    }
  )
  
  # Plot for ref_accept_df
  output$accept_plot <- renderPlotly({
    filtered_data <- ref_accept_df %>%
      filter(
        dataset_type == input$dataset_type_accept,
        hb_name == input$health_board_accept,
        measure_type == input$measure_type_accept
      )
    
    p <- ggplot(filtered_data, aes(x = quarter_ending, y = count, group = measure_breakdown, color = measure_breakdown,
                                   text = paste("Quarter ending: ", quarter_ending,
                                                "<br>Acceptance status: ", measure_breakdown,
                                                paste0("<br>", input$measure_type_master, ": "), count))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = ref_acc_palette) +
      scale_x_date(breaks = quarters_in_data,
                   labels = function(x) format(x, "%b-%y")) +
      labs(title = paste("Referrals Acceptance Status -", input$measure_type_accept),
           x = "Quarter Ending",
           y = input$measure_type_master,
           color = 'Acceptance Status') +
      theme_captnd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot for ref_master_df
  filtered_ref_data <- reactive({
    req(input$demographic_options_master)
    req(length(input$demographic_options_master) > 0)
    
    ref_master_df %>%
      filter(
        dataset_type      == input$dataset_type_master,
        hb_name           == input$health_board_master,
        measure_name      == input$demographic_type_master,
        measure_type      == input$measure_type_master,
        measure_breakdown %in% input$demographic_options_master
      )
  })
  
  
  output$master_plot <- renderPlotly({
    data <- filtered_ref_data()
    req(nrow(data) > 0)  # Prevent plotting if no data
    data$measure_breakdown <- factor(data$measure_breakdown, levels = sort(unique(data$measure_breakdown)))
    
    p <- ggplot(data, aes(x = quarter_ending, y = count, group = measure_breakdown, color = measure_breakdown,
                          text  = paste("Quarter ending: ", quarter_ending,
                                        "<br>Demographic group: ", measure_breakdown,
                                        paste0("<br>", input$measure_type_master, ": "), count))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = demo_palette, breaks = levels(data$measure_breakdown)) +
      scale_x_date(breaks = quarters_in_data,
                   labels = function(x) format(x, "%b-%y")) +
      labs(title = paste("Referrals by Demographic -", input$measure_type_master),
           x = "Quarter Ending",
           y = input$measure_type_master,
           color = 'Demographic group') +
      theme_captnd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  
  #add table function
  observeEvent(input$show_table_master, {
    output$table_master <- renderDT({
      filtered_ref_data()
    })
    
  })
  
  #downlaod data function
  output$download_master_data <- downloadHandler(
    filename = function() {
      paste("referrals_by_demographic_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_ref_data(), file, row.names = FALSE)
    }
  )
  
  
  # Plot for master_appts_df
  output$appt_plot <- renderPlotly({
    filtered_data <- master_appts_df %>%
      filter(
        dataset_type == input$dataset_type_appts,
        hb_name == input$health_board_appts,
        measure_type == input$measure_type_appts,
        measure_name == input$contact_type_appts,
        measure_breakdown %in% input$attendance_status_appts
      )
    
    
    filtered_data$measure_breakdown <- factor(
      filtered_data$measure_breakdown,
      levels = sort(unique(filtered_data$measure_breakdown))
    )
    
    
    p <- ggplot(filtered_data, aes(x = app_quarter_ending, y = count, group = measure_breakdown, color = measure_breakdown,
                                   text = paste(paste0("<b><br>", input$contact_type_appts, "<b>"),
                                                "<br>Quarter ending: ", app_quarter_ending,
                                                "<br>Attendance status: ", measure_breakdown,
                                                paste0("<br>", input$measure_type_appts, ": "), count))) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      scale_color_manual(values = appt_att_palette) +
      scale_x_date(breaks = quarters_in_data,
                   labels = function(x) format(x, "%b-%y")) +
      labs(title = paste0(input$contact_type_appts, " - ", input$measure_type_appts),
           x = "Quarter Ending",
           y = input$measure_type_appt,
           color = 'Attendance Status') +
      theme_captnd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  
}