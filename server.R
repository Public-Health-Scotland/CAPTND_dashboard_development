#### Server ####
server <- function(input, output, session) {
  
  
  #PT referral radio buttons
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
  
  #CAMHS referral radio buttons
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
  
  #PT referral acceptance radio buttons
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
  
  #CAMHS referral acceptance radio buttons
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
  
  
  #PT appointments radio buttons
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
  
  #CAMHS appointments radio buttons
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
  
  #PT appt location/professional group radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_pt",
      choices = unique(master_loc_prof_df$hb_name),
      selected = unique(master_loc_prof_df$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_pt",
      choices = unique(master_loc_prof_df$app_quarter_ending),
      selected = unique(master_loc_prof_df$app_quarter_ending)[1]
    )
    
    # updateRadioButtons(
    #   session,
    #   inputId = "measure_type_pt",
    #   choices = unique(master_loc_prof_df$measure_type),
    #   selected = unique(master_loc_prof_df$measure_type)[1]
    # )
    
  })
  
  #CAMHS appt location/professional group radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(master_loc_prof_df$hb_name),
      selected = unique(master_loc_prof_df$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_camhs",
      choices = unique(master_loc_prof_df$app_quarter_ending),
      selected = unique(master_loc_prof_df$app_quarter_ending)[1]
    )
    
    # updateRadioButtons(
    #   session,
    #   inputId = "measure_type_camhs",
    #   choices = unique(master_loc_prof_df$measure_type),
    #   selected = unique(master_loc_prof_df$measure_type)[1]
    # )
    
  })
  
  #PT referral rejection reason/actions radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_pt",
      choices = unique(master_non_acceptance_df$hb_name),
      selected = unique(master_non_acceptance_df$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_pt",
      choices = unique(master_non_acceptance_df$quarter_ending),
      selected = unique(master_non_acceptance_df$quarter_ending)[1]
    )
    
    # updateRadioButtons(
    #   session,
    #   inputId = "measure_type_pt",
    #   choices = unique(master_non_acceptance_df$measure_type),
    #   selected = unique(master_non_acceptance_df$measure_type)[1]
    # )
    
  })
  
  #CAMHS referral rejection reason/actions radio buttons
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(master_non_acceptance_df$hb_name),
      selected = unique(master_non_acceptance_df$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_camhs",
      choices = unique(master_non_acceptance_df$quarter_ending),
      selected = unique(master_non_acceptance_df$quarter_ending)[1]
    )
    
    # updateRadioButtons(
    #   session,
    #   inputId = "measure_type_camhs",
    #   choices = unique(master_non_acceptance_df$measure_type),
    #   selected = unique(master_non_acceptance_df$measure_type)[1]
    # )
    
  })
  
  #PT referral source
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(df_ref_source$hb_name),
      selected = unique(df_ref_source$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_camhs",
      choices = unique(df_ref_source$quarter_ending),
      selected = unique(df_ref_source$quarter_ending)[1]
    )
    
    # updateRadioButtons(
    #   session,
    #   inputId = "measure_type_camhs",
    #   choices = unique(df_ref_source$measure_type),
    #   selected = unique(df_ref_source$measure_type)[1]
    # )
    
  })
  
  #CAMHS referral source
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(df_ref_source$hb_name),
      selected = unique(df_ref_source$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_camhs",
      choices = unique(df_ref_source$quarter_ending),
      selected = unique(df_ref_source$quarter_ending)[1]
    )
    
    # updateRadioButtons(
    #   session,
    #   inputId = "measure_type_camhs",
    #   choices = unique(df_ref_source$measure_type),
    #   selected = unique(df_ref_source$measure_type)[1]
    # )
    
  })
  
  #PT First contact dnas
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_pt",
      choices = unique(first_con_dna_simd$hb_name),
      selected = unique(first_con_dna_simd$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_pt",
      choices = unique(first_con_dna_simd$app_quarter_ending),
      selected = unique(first_con_dna_simd$app_quarter_ending)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_pt",
      choices = unique(first_con_dna_simd$measure_type),
      selected = unique(first_con_dna_simd$measure_type)[1]
    )
    
  })
  
  
  #CAMHS First contact dnas
  observeEvent(input$tabs, {
    
    updateSelectInput(
      session,
      inputId = "health_board_camhs",
      choices = unique(first_con_dna_simd$hb_name),
      selected = unique(first_con_dna_simd$hb_name)[1]
    )
    
    updateSelectInput(
      session,
      inputId = "quarter_ending_camhs",
      choices = unique(first_con_dna_simd$app_quarter_ending),
      selected = unique(first_con_dna_simd$app_quarter_ending)[1]
    )
    
    updateRadioButtons(
      session,
      inputId = "measure_type_camhs",
      choices = unique(first_con_dna_simd$measure_type),
      selected = unique(first_con_dna_simd$measure_type)[1]
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
             measure_name == "Referrals by sex",
             hb_name == input$pt_health_board_sex,
             measure_type == input$pt_measure_type_sex)
  })
  
  #PT referrals by sex - reactive text
  output$pt_referrals_sex_text <- renderUI({
    
    ref_sex_df <- filtered_ref_data_sex_pt()
    
    generate_referrals_sex_text(ref_sex_df, input$pt_measure_type_sex, dataset_type = 'PT')
    
  })
  
  # Reactive for age group graph
  filtered_ref_data_age_pt <- reactive({
    req(input$pt_health_board_age, input$pt_measure_type_age)
    ref_master_df %>%
      filter(dataset_type == "PT",
             measure_name == "Referrals by age",
             hb_name == input$pt_health_board_age,
             measure_type == input$pt_measure_type_age)
  })
  
  #PT referrals by age - reactive text
  output$pt_referrals_age_text <- renderUI({
    
    ref_age_df <- filtered_ref_data_age_pt()
    
    generate_referrals_age_text(ref_age_df, input$pt_measure_type_age, dataset_type = 'PT', input$pt_health_board_age)
    
  })
  
  # Reactive for SIMD quintile graph
  filtered_ref_data_simd_pt <- reactive({
    req(input$pt_health_board_simd, input$pt_quarter_simd, input$pt_measure_type_simd)
    ref_master_df %>%
      filter(dataset_type == "PT",
             measure_name == "Referrals by SIMD",
             hb_name == input$pt_health_board_simd,
             quarter_ending == input$pt_quarter_simd,
             measure_type == input$pt_measure_type_simd)
  })
  
  #PT referrals by simd - reactive text
  output$pt_referrals_simd_text <- renderUI({
    
    ref_simd_df <- filtered_ref_data_simd_pt()
    
    generate_referrals_simd_text(ref_simd_df, input$pt_measure_type_simd, dataset_type = 'PT', input$pt_health_board_simd)
    
  })
  
  ####PT Referrals Acceptance####
  #Referral source
  filtered_ref_data_source_pt <- reactive({
    req(input$pt_health_board_ref_source, input$pt_quarter_ref_source)
    df_ref_source %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_ref_source,
             quarter_ending == input$pt_quarter_ref_source)
             #measure_type == input$pt_measure_type_ref_source)
  })
  
  #PT referrals by source - reactive text
  output$pt_referral_source_text <- renderUI({
    
    ref_source_df <- filtered_ref_data_source_pt()
    
    generate_referral_source_text(ref_source_df, dataset_type = 'PT', input$pt_health_board_ref_source)
    
  })
  
  #Acceptance
  filtered_ref_data_accept_pt <- reactive({
    req(input$pt_health_board_accept, input$pt_measure_type_accept)
    ref_accept_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_accept,
             measure_type == input$pt_measure_type_accept)
  })
  
  #Rejected reasons
  filtered_ref_data_rej_reason_pt <- reactive({
    req(input$pt_health_board_rej_reason, input$pt_quarter_rej_reason)
    master_non_acceptance_df %>%
      filter(dataset_type == "PT",
             measure_name == "Non-acceptance reason",
             hb_name == input$pt_health_board_rej_reason,
             quarter_ending == input$pt_quarter_rej_reason)
             #measure_type == input$pt_measure_type_rej_reason)
  })
  
  #PT referrals by rejection reason - reactive text
  output$pt_rej_reason_text <- renderUI({
    
    non_accept_reason_df <- filtered_ref_data_rej_reason_pt()
    
    generate_non_accept_reason_text(non_accept_reason_df, dataset_type = 'PT', input$pt_health_board_rej_reason)
    
  })
  
  #Rejected actions
  filtered_ref_data_rej_action_pt <- reactive({
    req(input$pt_health_board_rej_action, input$pt_quarter_rej_action)
    master_non_acceptance_df %>%
      filter(dataset_type == "PT",
             measure_name == "Non-acceptance action",
             hb_name == input$pt_health_board_rej_action,
             quarter_ending == input$pt_quarter_rej_action)
             #measure_type == input$pt_measure_type_rej_action)
  })
  
  #PT referrals by rejection action - reactive text
  output$pt_rej_action_text <- renderUI({
    
    non_accept_action_df <- filtered_ref_data_rej_action_pt()
    
    generate_non_accept_action_text(non_accept_action_df, dataset_type = 'PT', input$pt_health_board_rej_action)
    
  })
  
  ####CAMHS Referrals Demographic####
  
  # Reactive for sex graph
  filtered_ref_data_sex_camhs <- reactive({
    req(input$camhs_health_board_sex, input$camhs_measure_type_sex)
    ref_master_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == "Referrals by sex",
             hb_name == input$camhs_health_board_sex,
             measure_type == input$camhs_measure_type_sex)
  })
  
  #CAMHS referrals by sex - reactive text
  output$camhs_referrals_sex_text <- renderUI({
    
    ref_sex_df <- filtered_ref_data_sex_camhs()
    
    generate_referrals_sex_text(ref_sex_df, input$camhs_measure_type_sex, dataset_type = 'CAMHS')
    
  })
  
  # Reactive for age group graph
  filtered_ref_data_age_camhs <- reactive({
    req(input$camhs_health_board_age, input$camhs_measure_type_age)
    ref_master_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == "Referrals by age",
             hb_name == input$camhs_health_board_age,
             measure_type == input$camhs_measure_type_age)
  })
  
  #CAMHS referrals by age - reactive text
  output$camhs_referrals_age_text <- renderUI({
    
    ref_age_df <- filtered_ref_data_age_camhs()
    
    generate_referrals_age_text(ref_age_df, input$camhs_measure_type_age, dataset_type = 'CAMHS', input$camhs_health_board_age)
    
  })
  
  #Reactive for SIMD quintile graph
  filtered_ref_data_simd_camhs <- reactive({
    req(input$camhs_health_board_simd, input$camhs_quarter_simd, input$camhs_measure_type_simd)
    ref_master_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == "Referrals by SIMD",
             hb_name == input$camhs_health_board_simd,
             quarter_ending == input$camhs_quarter_simd,
             measure_type == input$camhs_measure_type_simd)
  })
  
  #CAMHS referrals by simd - reactive text
  output$camhs_referrals_simd_text <- renderUI({
    
    ref_simd_df <- filtered_ref_data_simd_camhs()
    
    generate_referrals_simd_text(ref_simd_df, input$camhs_measure_type_simd, dataset_type = 'CAMHS', input$camhs_health_board_simd)
    
  })
  
  ####CAMHS Referrals Acceptance####
  #Referral source
  filtered_ref_data_source_camhs <- reactive({
    req(input$camhs_health_board_ref_source, input$camhs_quarter_ref_source)
    df_ref_source %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_ref_source,
             quarter_ending == input$camhs_quarter_ref_source)
             #measure_type == input$camhs_measure_type_ref_source)
  })
  
  #CAMHS referrals by source - reactive text
  output$camhs_referral_source_text <- renderUI({
    
    ref_source_df <- filtered_ref_data_source_camhs()
    
    generate_referral_source_text(ref_source_df, dataset_type = 'CAMHS', input$camhs_health_board_ref_source)
    
  })
  
  #Acceptance
  filtered_ref_data_accept_camhs <- reactive({
    req(input$camhs_health_board_accept, input$camhs_measure_type_accept)
    ref_accept_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_accept,
             measure_type == input$camhs_measure_type_accept)
  })
  
  #Rejected reasons
  filtered_ref_data_rej_reason_camhs <- reactive({
    req(input$camhs_health_board_rej_reason, input$camhs_quarter_rej_reason)
    master_non_acceptance_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == "Non-acceptance reason",
             hb_name == input$camhs_health_board_rej_reason,
             quarter_ending == input$camhs_quarter_rej_reason)
             #measure_type == input$camhs_measure_type_rej_reason)
  })
  
  #CAMHS referrals by rejection reason - reactive text
  output$camhs_rej_reason_text <- renderUI({
    
    non_accept_reason_df <- filtered_ref_data_rej_reason_camhs()
    
    generate_non_accept_reason_text(non_accept_reason_df, dataset_type = 'CAMHS', input$camhs_health_board_rej_reason)
    
  })
  
  #Rejected actions
  filtered_ref_data_rej_action_camhs <- reactive({
    req(input$camhs_health_board_rej_action, input$camhs_quarter_rej_action)
    master_non_acceptance_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == "Non-acceptance action",
             hb_name == input$camhs_health_board_rej_action,
             quarter_ending == input$camhs_quarter_rej_action)
             #measure_type == input$camhs_measure_type_rej_action)
  })
  
  #CAMHS referrals by rejection action - reactive text
  output$camhs_rej_action_text <- renderUI({
    
    non_accept_action_df <- filtered_ref_data_rej_action_camhs()
    
    generate_non_accept_action_text(non_accept_action_df, dataset_type = 'CAMHS', input$camhs_health_board_rej_action)
    
  })
  
  ####PT Appointments####
  ## All appointments ##
  filtered_appt_att_data_pt <- reactive({
    req(input$pt_health_board_att, input$pt_measure_type_att)
    master_appts_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_att,
             measure_type == input$pt_measure_type_att)
  })
  
  ## Appt location ##
  filtered_appt_loc_data_pt <- reactive({
    req(input$pt_health_board_loc, input$pt_quarter_loc)
    master_loc_prof_df %>%
      filter(dataset_type == "PT",
             measure_name == "Appointment care location",
             hb_name == input$pt_health_board_loc,
             app_quarter_ending == input$pt_quarter_loc)
             #measure_type == input$pt_measure_type_loc)
  })
  
  #PT referrals by rejection action - reactive text
  output$pt_appt_loc_text <- renderUI({
    
    appt_loc_df <- filtered_appt_loc_data_pt()
    
    generate_appt_loc_text(appt_loc_df, dataset_type = 'PT', input$pt_health_board_loc)
    
  })
  
  ## Prof group ##
  filtered_appt_prof_data_pt <- reactive({
    req(input$pt_health_board_prof, input$pt_quarter_prof)
    master_loc_prof_df %>%
      filter(dataset_type == "PT",
             measure_name == "Care professional",
             hb_name == input$pt_health_board_prof,
             app_quarter_ending == input$pt_quarter_prof)
             #measure_type == input$pt_measure_type_prof)
  })
  
  #PT appointment by professional group - reactive text
  output$pt_appt_prof_text <- renderUI({
    
    appt_prof_df <- filtered_appt_prof_data_pt()
    
    generate_appt_prof_text(appt_prof_df, dataset_type = 'CAMHS', input$pt_health_board_prof)
    
  })
  
  ## First contact ##
  filtered_first_appt_data_pt <- reactive({
    req(input$pt_health_board_first_appt, input$pt_measure_type_first_appt)
    master_appts_df %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_first_appt,
             measure_type == input$pt_measure_type_first_appt)
  })
  
  ## First contact DNAs ##
  filtered_first_appt_dnas_data_pt <- reactive({
    req(input$pt_health_board_first_appt_dna, input$pt_quarter_first_appt_dna, input$pt_measure_type_first_appt_dna)
    first_con_dna_simd %>%
      filter(dataset_type == "PT",
             hb_name == input$pt_health_board_first_appt_dna,
             app_quarter_ending == input$pt_quarter_first_appt_dna,
             measure_type == input$pt_measure_type_first_appt_dna)
  })
  
  ####CAMHS Appointments####
  filtered_appt_att_data_camhs <- reactive({
    req(input$camhs_health_board_att, input$camhs_measure_type_att)
    master_appts_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_att,
             measure_type == input$camhs_measure_type_att)
  })
  
  ## Appt location ##
  filtered_appt_loc_data_camhs <- reactive({
    req(input$camhs_health_board_loc, input$camhs_quarter_loc)
    master_loc_prof_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == 'Appointment care location',
             hb_name == input$camhs_health_board_loc,
             app_quarter_ending == input$camhs_quarter_loc)
             #measure_type == input$camhs_measure_type_loc)
  })
  
  #CAMHS appointments by care location - reactive text
  output$camhs_appt_loc_text <- renderUI({
    
    appt_loc_df <- filtered_appt_loc_data_camhs()
    
    generate_appt_loc_text(appt_loc_df, dataset_type = 'CAMHS', input$camhs_health_board_loc)
    
  })
  
  ## Prof group ##
  filtered_appt_prof_data_camhs <- reactive({
    req(input$camhs_health_board_prof, input$camhs_quarter_prof)
    master_loc_prof_df %>%
      filter(dataset_type == "CAMHS",
             measure_name == "Care professional",
             hb_name == input$camhs_health_board_prof,
             app_quarter_ending == input$camhs_quarter_prof)
             #measure_type == input$camhs_measure_type_prof)
  })
  
  #CAMHS appointment by professional group - reactive text
  output$camhs_appt_prof_text <- renderUI({
    
    appt_prof_df <- filtered_appt_prof_data_camhs()
    
    generate_appt_prof_text(appt_prof_df, dataset_type = 'CAMHS', input$camhs_health_board_prof)
    
  })
  
  ## First contact ##
  filtered_first_appt_data_camhs <- reactive({
    req(input$camhs_health_board_first_appt, input$camhs_measure_type_first_appt)
    master_appts_df %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_first_appt,
             measure_type == input$camhs_measure_type_first_appt)
  })
  
  ## First contact DNAs ##
  filtered_first_appt_dnas_data_camhs <- reactive({
    req(input$camhs_health_board_first_appt_dna, input$camhs_quarter_first_appt_dna, input$camhs_measure_type_first_appt_dna)
    first_con_dna_simd %>%
      filter(dataset_type == "CAMHS",
             hb_name == input$camhs_health_board_first_appt_dna,
             app_quarter_ending == input$camhs_quarter_first_appt_dna,
             measure_type == input$camhs_measure_type_first_appt_dna)
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
    
    create_bar_graph(data, input$pt_measure_type_simd, demo_palette,
                      label_name = "SIMD Quintile", label_title = "PT Referrals",
                     quarter = input$pt_quarter_simd)
    
  })
  
  #CAMHS referrals by SIMD plot
  output$ref_simd_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_simd_camhs() |>
      filter(measure_name == 'Referrals by SIMD')
    
    create_bar_graph(data, input$camhs_measure_type_simd, demo_palette,
                     label_name = "SIMD Quintile", label_title = "CAMHS Referrals",
                     quarter = input$camhs_quarter_simd)
    
  })
  
  #PT referral source plot
  output$ref_source_plot_pt <- renderPlotly({

    data <- filtered_ref_data_source_pt()
    
    # create_horz_bar_graph(data, input$pt_measure_type_ref_source, label_name = "Referral Source", 
    #                       label_title = "PT Referral Source", xaxis_name = "Referral source",
    #                       quarter = input$pt_quarter_ref_source) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of referrals', label_name = "Referral Source",
                              label_title = "PT Referral Source", xaxis_name = "Referral source",
                              quarter = input$pt_quarter_ref_source)
    
  })
  
  #PT referral acceptance plot
  output$ref_accept_plot_pt <- renderPlotly({
    
    data <- filtered_ref_data_accept_pt() |>
      filter(measure_name == 'Referrals acceptance status')
    
    create_line_graph(data, input$pt_measure_type_accept, ref_acc_palette, quarters_in_data, 
                      label_name = "Acceptance status", label_title = "CAMHS Referrals")
    
  })
  
  #PT referral rejection reason
  output$ref_rej_reason_plot_pt <- renderPlotly({
    
    data <- filtered_ref_data_rej_reason_pt() |>
      filter(measure_name == 'Non-acceptance reason')
    
    # create_horz_bar_graph(data, input$pt_measure_type_ref_rej, label_name = "Rejection Reason", 
    #                       label_title = "PT Referral Rejection Reason", xaxis_name = "Rejection reason",
    #                       quarter = input$pt_quarter_rej_reason) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of referrals', label_name = "Rejection Reason",
                              label_title = "PT Referral Rejection Reason", xaxis_name = "Rejection reason",
                              quarter = input$pt_quarter_rej_reason)
    
  })
  
  #PT referral rejection actions
  output$ref_rej_action_plot_pt <- renderPlotly({
    
    data <- filtered_ref_data_rej_action_pt() |>
      filter(measure_name == 'Non-acceptance action')
    
    # create_horz_bar_graph(data, input$pt_measure_type_ref_action, label_name = "Rejection Action", 
    #                       label_title = "PT Referral Rejection Action", xaxis_name = "Rejection action",
    #                       quarter = input$pt_quarter_rej_action) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of referrals', label_name = "Rejection Action",
                              label_title = "PT Referral Rejection Action", xaxis_name = "Rejection action",
                              quarter = input$pt_quarter_rej_action)
    
  })
  
  #CAMHS referral source plot
  output$ref_source_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_source_camhs()
    
    # create_horz_bar_graph(data, input$camhs_measure_type_ref_source, label_name = "Referral Source", 
    #                       label_title = "CAMHS Referral Source", xaxis_name = "Referral source",
    #                       quarter = input$camhs_quarter_ref_source) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of referrals', label_name = "Referral Source",
                              label_title = "CAMHS Referral Source", xaxis_name = "Referral source",
                              quarter = input$camhs_quarter_ref_source)
    
  })
  
  #CAMHS referral acceptance plot
  output$ref_accept_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_accept_camhs() |>
      filter(measure_name == 'Referrals acceptance status')
    
    create_line_graph(data, input$camhs_measure_type_accept, ref_acc_palette, quarters_in_data, 
                      label_name = "Acceptance status", label_title = "CAMHS Referrals")
    
  })
  
  #CAMHS referral rejection reason
  output$ref_rej_reason_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_rej_reason_camhs() |>
      filter(measure_name == 'Non-acceptance reason')
    
    # create_horz_bar_graph(data, input$camhs_measure_type_ref_rej, label_name = "Rejection Reason", 
    #                       label_title = "CAMHS Referral Rejection Reason", xaxis_name = "Rejection reason",
    #                       quarter = input$camhs_quarter_rej_reason) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of referrals', label_name = "Rejection Reason",
                              label_title = "CAMHS Referral Rejection Reason", xaxis_name = "Rejection reason",
                              quarter = input$camhs_quarter_rej_reason)
    
  })
  
  #CAMHS referral rejection actions
  output$ref_rej_action_plot_camhs <- renderPlotly({
    
    data <- filtered_ref_data_rej_action_camhs() |>
      filter(measure_name == 'Non-acceptance action')
    
    # create_horz_bar_graph(data, input$camhs_measure_type_ref_action, label_name = "Rejection Action", 
    #                       label_title = "CAMHS Referral Rejection Action", xaxis_name = "Rejection action",
    #                       quarter = input$camhs_quarter_rej_action) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of referrals', label_name = "Rejection Action",
                              label_title = "CAMHS Referral Rejection Action", xaxis_name = "Rejection action",
                              quarter = input$camhs_quarter_rej_action)
    
  })
  
  
  #PT appointments attendance plot
  output$appt_att_plot_pt <- renderPlotly({
    
    data <- filtered_appt_att_data_pt() |>
      filter(measure_name == 'Total appointment attendances')
    
    create_line_graph(data, input$pt_measure_type_att, appt_att_palette, quarters_in_data,
                      label_name = "Attendance status", label_title = "PT Appointment Attendance")
   
  })
  
  
  #CAMHS appointments attendance plot
  output$appt_att_plot_camhs <- renderPlotly({
    
    data <- filtered_appt_att_data_camhs() |>
      filter(measure_name == 'Total appointment attendances')
    
    create_line_graph(data, input$camhs_measure_type_att, appt_att_palette, quarters_in_data,
                      label_name = "Attendance status", label_title = "CAMHS Appointment Attendance")
    
  })
  
  #PT first contact attendance plot
  output$first_appt_plot_pt <- renderPlotly({
    
    data <- filtered_first_appt_data_pt() |>
      filter(measure_name == 'First contact attendances')
    
    create_line_graph(data, input$pt_measure_type_first_att, appt_att_palette, quarters_in_data,
                      label_name = "Attendance status", label_title = "PT First Contact Attendance")
    
  })
  
  #CAMHS first contact attendance plot
  output$first_appt_plot_camhs <- renderPlotly({
    
    data <- filtered_first_appt_data_camhs() |>
      filter(measure_name == 'First contact attendances')
    
    create_line_graph(data, input$camhs_measure_type_first_att, appt_att_palette, quarters_in_data,
                      label_name = "Attendance status", label_title = "CAMHS First Contact Attendance")
    
  })
  
  #PT appointment location
  output$appt_loc_plot_pt <- renderPlotly({
    
  data <- filtered_appt_loc_data_pt() |>
    filter(measure_name == 'Appointment care location')
  
  # create_horz_bar_graph(data, input$pt_measure_type_loc, label_name = "Appt location", 
  #                       label_title = "PT Care Contact Location", xaxis_name = "Care contact location",
  #                       quarter = input$pt_quarter_loc) 
  
  create_new_horz_bar_graph(data, measure_title = 'Number of appointments', label_name = "Appt location",
                            label_title = "PT Care Contact Location", xaxis_name = "Care contact location",
                            quarter = input$pt_quarter_loc)
  
  })
  
  #CAMHS appointment location
  output$appt_loc_plot_camhs <- renderPlotly({
    
    data <- filtered_appt_loc_data_camhs() |>
      filter(measure_name == 'Appointment care location')
    
    # create_horz_bar_graph(data, input$camhs_measure_type_loc, label_name = "Appt location", 
    #                       label_title = "CAMHS Care Contact Location", xaxis_name = "Care contact location",
    #                       quarter = input$camhs_quarter_loc) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of appointments', label_name = "Appt location",
                              label_title = "CAMHS Care Contact Location", xaxis_name = "Care contact location",
                              quarter = input$camhs_quarter_loc)
    
  })
  
  #PT professional group
  output$appt_prof_plot_pt <- renderPlotly({
    
    data <- filtered_appt_prof_data_pt() |>
      filter(measure_name == 'Care professional')
    
    # create_horz_bar_graph(data, input$pt_measure_type_prof, label_name = "Care prof.", 
    #                       label_title = "PT Care Professional", xaxis_name = "Care professional",
    #                       quarter = input$pt_quarter_prof) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of appointments', label_name = "Care prof.",
                              label_title = "PT Care Professional", xaxis_name = "Care professional",
                              quarter = input$pt_quarter_loc)
    
  })
  
  #CAMHS professional group
  output$appt_prof_plot_camhs <- renderPlotly({
    
    data <- filtered_appt_prof_data_camhs() |>
      filter(measure_name == 'Care professional')
    
    # create_horz_bar_graph(data, input$pt_measure_type_prof, label_name = "Care prof.", 
    #                       label_title = "CAMHS Care Professional", xaxis_name = "Care professional",
    #                       quarter = input$camhs_quarter_prof) 
    
    create_new_horz_bar_graph(data, measure_title = 'Number of appointments', label_name = "Care prof.",
                              label_title = "CAMHS Care Professional", xaxis_name = "Care professional",
                              quarter = input$camhs_quarter_loc)
    
  })
  
  #PT first contact DNAs
  output$first_appt_dna_pt <- renderPlotly({
    
    data <- filtered_first_appt_dnas_data_pt()
    
    create_bar_graph(data, input$pt_measure_type_first_appt_dna, demo_palette,
                     label_name = "SIMD Quintile", label_title = "PT first contact DNAs", 
                     quarter = input$pt_quarter_first_appt_dna)
    
  })
  
  #CAMHS first contact DNAs
  output$first_appt_dna_camhs <- renderPlotly({
    
    data <- filtered_first_appt_dnas_data_camhs()
    
    create_bar_graph(data, input$camhs_measure_type_first_appt_dna, demo_palette,
                     label_name = "SIMD Quintile", label_title = "PT first contact DNAs", 
                     quarter = input$camhs_quarter_first_appt_dna)
    
  })
  
  
  #PT prof group
  # output$appt_prof_plot_pt <- renderPlotly({
  #   
  #   data <- filtered_app_prof_data_pt() |>
  #     filter(measure_name == 'Care professional')
  #   
  #   label_order <- data$measure_breakdown
  #   ifelse(any(label_order == "Not known"),
  #          label_order <- c(label_order[-which(label_order == "Not known")], "Not known"), "") # put not known and missing to the bottom of the plot
  #   ifelse(any(label_order == "Missing data"),
  #          label_order <- c(label_order[-which(label_order == "Missing data")], "Missing data"), "")
  #   
  #   req(nrow(data) > 0)
  #   
  #   p <- data |>
  #     mutate(measure_breakdown = factor(measure_breakdown, levels = label_order)) |>
  #     ggplot(aes(
  #       x = fct_rev(measure_breakdown),
  #       y = count,
  #       text = paste(
  #         paste0("<b> PT Care Professional <b>"), #label_title
  #         "<br>Quarter ending: ", app_quarter_ending,
  #         paste0("<br> Care professional : "), measure_breakdown, #label_name
  #         paste0("<br>", input$pt_measure_type_prof, ": "), count)
  #     )) +
  #     geom_col(width = 0.7, fill = "#3F3685") +
  #     coord_flip()+
  #     labs(x = "Care contact location", y = input$pt_measure_type_prof) + 
  #     theme_captnd() +
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #     expand_limits(y = 0)
  #   
  #   ggplotly(p, tooltip = "text")
  #   
  # })
  
}

