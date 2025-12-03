#### Server ####
server <- function(input, output, session) {
  
  #PT referral tab radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_pt",
      choices = unique(ref_master_df$hb_name),
      selected = unique(ref_master_df$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_pt",
      choices = unique(ref_master_df$quarter_ending),
      selected = unique(ref_master_df$quarter_ending)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_pt",
      choices = unique(ref_master_df$measure_type),
      selected = unique(ref_master_df$measure_type)[1]
    )
    
  })
  
  #CAMHS referral tab radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(ref_master_df$hb_name),
      selected = unique(ref_master_df$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_camhs",
      choices = unique(ref_master_df$quarter_ending),
      selected = unique(ref_master_df$quarter_ending)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_camhs",
      choices = unique(ref_master_df$measure_type),
      selected = unique(ref_master_df$measure_type)[1]
    )
    
  })
  
  #PT referral acceptance tab radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_pt",
      choices = unique(ref_accept_df$hb_name),
      selected = unique(ref_accept_df$hb_name)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_pt",
      choices = unique(ref_accept_df$measure_type),
      selected = unique(ref_accept_df$measure_type)[1]
    )
    
  })
  
  #CAMHS referral acceptance tab radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(ref_accept_df$hb_name),
      selected = unique(ref_accept_df$hb_name)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_camhs",
      choices = unique(ref_accept_df$measure_type),
      selected = unique(ref_accept_df$measure_type)[1]
    )
    
  })
  
  
  #PT appointments tab radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_pt",
      choices = unique(master_appts_df$hb_name),
      selected = unique(master_appts_df$hb_name)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_pt",
      choices = unique(master_appts_df$measure_type),
      selected = unique(master_appts_df$measure_type)[1]
    )
    
  })
  
  #CAMHS appointments tab radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(master_appts_df$hb_name),
      selected = unique(master_appts_df$hb_name)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_camhs",
      choices = unique(master_appts_df$measure_type),
      selected = unique(master_appts_df$measure_type)[1]
    )
    
  })
  
  #Using this plot button
  observeEvent(input$info_btn, {
    showModal(modalDialog(
      how_to_use_plot_text,
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  
  ####PT Referrals Demographic####
  
  # Reactive for sex graph
  filtered_ref_data_sex_pt <- reactive({
    req(input$pt_health_board_sex, input$pt_measure_type_sex)
    ref_master_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_sex,
             measure_type == input$pt_measure_type_sex)
  })
  
  #PT referrals by sex - reactive text
  output$pt_referrals_sex_text <- renderUI({
    
    ref_sex_pt_df <- filtered_ref_data_sex_pt()
    
    generate_referrals_sex_text(ref_sex_pt_df, input$pt_measure_type_sex, dataset_type = 'PT')
    
  })
  
  # Reactive for age group graph
  filtered_ref_data_age_pt <- reactive({
    req(input$pt_health_board_age, input$pt_measure_type_age)
    ref_master_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_age,
             measure_type == input$pt_measure_type_age)
  })
  
  # Reactive for SIMD quintile graph
  filtered_ref_data_simd_pt <- reactive({
    req(input$pt_health_board_simd, input$pt_quarter_simd, input$pt_measure_type_simd)
    ref_master_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_simd,
             quarter_ending == input$pt_quarter_simd,
             measure_type == input$pt_measure_type_simd)
  })
  
  ####PT Referrals Acceptance####
  filtered_ref_data_accept_pt <- reactive({
    req(input$pt_health_board_accept, input$pt_measure_type_accept)
    ref_accept_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_accept,
             measure_type == input$pt_measure_type_accept)
  })
  
  ####CAMHS Referrals Demographic####
  
  # Reactive for sex graph
  filtered_ref_data_sex_camhs <- reactive({
    req(input$camhs_health_board_sex, input$camhs_measure_type_sex)
    ref_master_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_sex,
             measure_type == input$camhs_measure_type_sex)
  })
  
  #CAMHS referrals by sex - reactive text
  output$camhs_referrals_sex_text <- renderUI({
    
    ref_sex_camhs_df <- filtered_ref_data_sex_camhs()
    
    generate_referrals_sex_text(ref_sex_camhs_df, input$camhs_measure_type_sex, dataset_type = 'CAMHS')
    
  })
  
  # Reactive for age group graph
  filtered_ref_data_age_camhs <- reactive({
    req(input$camhs_health_board_age, input$camhs_measure_type_age)
    ref_master_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_age,
             measure_type == input$camhs_measure_type_age)
  })
  
  #Reactive for SIMD quintile graph
  filtered_ref_data_simd_camhs <- reactive({
    req(input$camhs_health_board_simd, input$camhs_quarter_simd, input$camhs_measure_type_simd)
    ref_master_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_simd,
             quarter_ending == input$camhs_quarter_simd,
             measure_type == input$camhs_measure_type_simd)
  })
  
  ####CAMHS Referrals Acceptance####
  filtered_ref_data_accept_camhs <- reactive({
    req(input$camhs_health_board_accept, input$camhs_measure_type_accept)
    ref_accept_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_accept,
             measure_type == input$camhs_measure_type_accept)
  })
  
  ####PT Appointments####
  filtered_app_att_data_pt <- reactive({
    req(input$pt_health_board_att, input$pt_measure_type_att)
    master_appts_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_att,
             measure_type == input$pt_measure_type_att)
  })
  
  ####CAMHS Appointments####
  filtered_app_att_data_camhs <- reactive({
    req(input$camhs_health_board_att, input$camhs_measure_type_att)
    master_appts_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_att,
             measure_type == input$camhs_measure_type_att)
  })
  
  
  #### Plots ####
  
  #PT referrals by sex plot
  output$ref_sex_plot_pt <- renderPlotly({
    
    data <- filtered_ref_data_sex_pt() |>
      filter(measure_name == 'Referrals by sex')
    
    create_line_graph(data, input$pt_measure_type_sex, demo_palette, quarters_in_data, 
                      label_name = "Sex at birth", label_title = "PT Referrals")
    
  })
  
  #CAMHS referrals by sex plot
  output$ref_sex_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_sex_camhs() |>
      filter(measure_name == 'Referrals by sex')
    
    create_line_graph(data, input$camhs_measure_type_sex, demo_palette, quarters_in_data, 
                      label_name = "Sex at birth", label_title = "CAMHS Referrals")
    
  })
  
  
  #PT referrals by age group plot
  output$ref_age_plot_pt <- renderPlotly({
    
    data <- filtered_ref_data_age_pt() |>
      filter(measure_name == 'Referrals by age')
    
    create_line_graph(data, input$pt_measure_type_age, demo_palette, quarters_in_data, 
                      label_name = "Age group", label_title = "PT Referrals")
    
  })
  
  #CAMHS referrals by age group plot
  output$ref_age_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_age_camhs() |>
      filter(measure_name == 'Referrals by age')
    
    create_line_graph(data, input$camhs_measure_type_age, demo_palette, quarters_in_data, 
                      label_name = "Age group", label_title = "CAMHS Referrals")
    
  })
  
  #PT referrals by SIMD plot
  output$ref_simd_plot_pt <- renderPlotly({
    data <- filtered_ref_data_simd_pt() |>
      filter(measure_name == 'Referrals by SIMD')
    req(nrow(data) > 0)  # Prevent plotting if no data
    data$measure_breakdown <- factor(data$measure_breakdown, levels = sort(unique(data$measure_breakdown)))
    
    p <- ggplot(data, aes(x = measure_breakdown, y = count, fill = measure_breakdown,
                          text  = paste("<b>PT Referrals<b>",
                                        "<br>Quarter ending: ", quarter_ending,
                                        "<br>SIMD Quintile: ", measure_breakdown,
                                        paste0("<br>", input$pt_measure_type_simd, ": "), count))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = demo_palette, breaks = levels(data$measure_breakdown)) +
      labs(x = "SIMD Quintile", y = input$pt_measure_type_simd, fill = "SIMD Quintile") +
      theme_captnd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  #CAMHS referrals by SIMD plot
  output$ref_simd_plot_camhs <- renderPlotly({
    data <- filtered_ref_data_simd_camhs() |>
      filter(measure_name == 'Referrals by SIMD')
    req(nrow(data) > 0)  # Prevent plotting if no data
    data$measure_breakdown <- factor(data$measure_breakdown, levels = sort(unique(data$measure_breakdown)))
    
    p <- ggplot(data, aes(x = measure_breakdown, y = count, fill = measure_breakdown,
                          text  = paste("<b>CAMHS Referrals<b>",
                                        "<br>Quarter ending: ", quarter_ending,
                                        "<br>SIMD Quintile: ", measure_breakdown,
                                        paste0("<br>", input$camhs_measure_type_simd, ": "), count))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = demo_palette, breaks = levels(data$measure_breakdown)) +
      labs(x = "SIMD Quintile", y = input$camhs_measure_type_simd, fill = "SIMD Quintile") +
      theme_captnd() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      expand_limits(y = 0)
    
    ggplotly(p, tooltip = "text")
  })
  
  #PT referral acceptance plot
  output$ref_accept_plot_pt <- renderPlotly({
    
    data <- filtered_ref_data_accept_pt() |>
      filter(measure_name == 'Referrals acceptance status')
    
    create_line_graph(data, input$pt_measure_type_accept, ref_acc_palette, quarters_in_data, 
                      label_name = "Acceptance status", label_title = "CAMHS Referrals")
    
  })
  
  #CAMHS referral acceptance plot
  output$ref_accept_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_accept_camhs() |>
      filter(measure_name == 'Referrals acceptance status')
    
    create_line_graph(data, input$camhs_measure_type_accept, ref_acc_palette, quarters_in_data, 
                      label_name = "Acceptance status", label_title = "CAMHS Referrals")
    
  })
  
  
  #PT appointments attendance plot
  output$appt_att_plot_pt <- renderPlotly({
    
    data <- filtered_app_att_data_pt() |>
      filter(measure_name == 'Total appointment attendances')
    
    create_line_graph(data, input$pt_measure_type_att, appt_att_palette, quarters_in_data,
                      label_name = "Attendance status", label_title = "PT Appointment Attendance")
   
  })
  
  
  #CAMHS appointment attendance plot
  output$appt_att_plot_camhs <- renderPlotly({
    
    data <- filtered_app_att_data_camhs() |>
      filter(measure_name == 'Total appointment attendances')
    
    create_line_graph(data, input$camhs_measure_type_att, appt_att_palette, quarters_in_data,
                      label_name = "Attendance status", label_title = "CAMHS Appointment Attendance")
    
  })
  
}

