##### UI #####
ui <- dashboardPage(
  dashboardHeader(title = "CAPTND Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Home", tabName = "home", icon = icon("home")),
                menuItem("Psychological Therapies", tabName = "pt", icon = icon("user-md"),
                         menuSubItem("PT Background", tabName = "pt_background"),
                         
                         menuItem("PT Referrals", icon = icon("clipboard-user"),
                                  menuSubItem("Referral Demographics", tabName = "pt_referrals"),
                                  menuSubItem("Referral Acceptance", tabName = "pt_referrals_accept")
                         ),
                         menuItem("PT Appointments", icon = icon("house-medical"),
                                  menuSubItem("Total Appointments", tabName = "pt_appts"),
                                  menuSubItem("First Contact Appointments", tabName = "pt_first_appts"))
                         
                ),
                menuItem("CAMHS", tabName = "camhs", icon = icon("child"),
                         menuSubItem("CAMHS Background", tabName = "camhs_background"),
                         
                         menuItem("CAMHS Referrals", icon = icon("clipboard-user"),
                                  menuSubItem("Referral Demographics", tabName = "camhs_referrals"),
                                  menuSubItem("Referral Acceptance", tabName = "camhs_referrals_accept")
                         ),
                         
                         menuItem("CAMHS Appointments", icon = icon("house-medical"),
                                  menuSubItem("Total Appointments", tabName = "camhs_appts"),
                                  menuSubItem("First Contact Appointments", tabName = "camhs_first_appts"))
                ),
                
                menuItem("Glossary", tabName = "glossary", icon = icon("book")),
                
                menuItem("Metadata", tabName = "metadata", icon = icon("database"),
                         menuSubItem("PT Appendices", tabName = "metadata_pt"),
                         menuSubItem("CAMHS Appendices", tabName = "metadata_camhs"))
                
    )
  ),
  
  dashboardBody(
    # Logo (fixed, top-right)
    tags$img(
      src = "phs-logo.png",  # must match file name in www/ ??
      id  = "phs-logo",
      alt = "Public Health Scotland"
    ),
    
    # Load custom CSS
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "custom.css")),
    
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              h2("Welcome to the Child, Adolescent, and Psychological Therapies National Dataset (CAPTND) Dashboard"),
              includeHTML("captnd_dashboard_intro.html")),
      
      # PT Referrals Demographic Tab
      tabItem(tabName = "pt_referrals",
        fluidRow(box(width = 12, status = "info", solidHeader = TRUE,
                     title = "PT Referrals",
            div(class = "intro-html",
                includeHTML("captnd_db_ref_text_pt.html")),
            
            #By sex
            div(class = "section-block",
                h3(class = "section-title", "PT referrals by sex"),
                uiOutput("pt_referrals_sex_text"),
                actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                selectInput("pt_health_board_sex", "Select health board:",
                            choices = unique(ref_master_df$hb_name),
                            selected = default_hb),
                radioButtons("pt_measure_type_sex", "Select measurement type:",
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_sex_plot_pt", height = "400px")),
            
            #By age group
            div(class = "section-block",
                h3(class = "section-title", "PT referrals by age group"),
                uiOutput("pt_referrals_age_text"),
                selectInput("pt_health_board_age", "Select health board:",
                            choices = unique(ref_master_df$hb_name),
                            selected = default_hb),
                radioButtons("pt_measure_type_age", "Select measurement type:",
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_age_plot_pt", height = "400px")),
            
            #By SIMD
            div(class = "section-block",
                h3(class = "section-title", "PT referrals by SIMD"),
                uiOutput("pt_referrals_simd_text"),
                selectInput("pt_health_board_simd", "Select health board:",
                            choices = unique(ref_master_df$hb_name),
                            selected = default_hb),
                selectInput("pt_quarter_simd", "Select quarter:",
                            choices = unique(ref_master_df$quarter_ending),
                            selected = default_quarter),
                radioButtons("pt_measure_type_simd", "Select measurement type:",
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_simd_plot_pt", height = "400px")),
            
            #Adult protection/Veteran status/PPMH
            div(class = "section-block",
                h3(class = "section-title", textOutput("pt_demo_status_header")),
                #uiOutput("pt_referrals_simd_text"),
                selectInput("pt_measure_demo_status", "Select variable:",
                            choices = NULL),
                selectInput("pt_health_board_demo_status", "Select health board:",
                            choices = NULL),
                selectInput("pt_quarter_demo_status", "Select quarter:",
                            choices = NULL),
                plotlyOutput("ref_demo_status_plot_pt", height = "400px"))
          )
        )
      ),
      
      #PT Referrals Acceptance tab
      tabItem(tabName = "pt_referrals_accept",
              
              fluidRow(
                box(title = "PT referral source", width = 12, 
                    status = "info", solidHeader = TRUE,
                    uiOutput("pt_referral_source_text"),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("pt_health_board_ref_source", "Select health board:", 
                                choices = unique(df_ref_source$hb_name),
                                selected = default_hb),
                    selectInput("pt_quarter_ref_source", "Select quarter:",
                                choice = unique(df_ref_source$quarter_ending),
                                selected = default_quarter),
                    # radioButtons("pt_measure_type_ref_source", "Select measurement type:", 
                    #              choices = unique(df_ref_source$measure_type),
                    #              selected = unique(df_ref_source$measure_type)[1]),
                    plotlyOutput("ref_source_plot_pt", height = "400px")),
              fluidRow(
                box(title = "PT referrals acceptance status", width = 12, 
                    status = "info", solidHeader = TRUE,
                    #uiOutput("pt_referrals_sex_text"),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("pt_health_board_accept", "Select health board:", 
                                choices = unique(ref_master_df$hb_name),
                                selected = default_hb),
                    radioButtons("pt_measure_type_accept", "Select measurement type:", 
                                 choices = unique(ref_accept_df$measure_type),
                                 selected = unique(ref_accept_df$measure_type)[1]),
                    plotlyOutput("ref_accept_plot_pt", height = "400px")),
                fluidRow(
                  box(title = "PT referral rejection reasons", width = 12, 
                      status = "info", solidHeader = TRUE,
                      uiOutput("pt_rej_reason_text"),
                      actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                      selectInput("pt_health_board_rej_reason", "Select health board:", 
                                  choices = unique(master_non_acceptance_df$hb_name),
                                  selected = default_hb),
                      selectInput("pt_quarter_rej_reason", "Select quarter:",
                                  choice = unique(master_non_acceptance_df$quarter_ending),
                                  selected = default_quarter),
                      # radioButtons("pt_measure_type_rej_reason", "Select measurement type:", 
                      #              choices = unique(master_non_acceptance_df$measure_type),
                      #              selected = unique(master_non_acceptance_df$measure_type)[1]),
                      plotlyOutput("ref_rej_reason_plot_pt", height = "400px")),
                  fluidRow(
                    box(title = "PT referral rejection actions", width = 12, 
                        status = "info", solidHeader = TRUE,
                        uiOutput("pt_rej_action_text"),
                        actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                        selectInput("pt_health_board_rej_action", "Select health board:", 
                                    choices = unique(master_non_acceptance_df$hb_name),
                                    selected = default_hb),
                        selectInput("pt_quarter_rej_action", "Select quarter:",
                                    choice = unique(master_non_acceptance_df$quarter_ending),
                                    selected = default_quarter),
                        # radioButtons("pt_measure_type_rej_action", "Select measurement type:", 
                        #              choices = unique(master_non_acceptance_df$measure_type),
                        #              selected = unique(master_non_acceptance_df$measure_type)[1]),
                        plotlyOutput("ref_rej_action_plot_pt", height = "400px"))
                  )
                ) 
              )
            )
      ), 
      
      
      #PT Appointment Tab
      tabItem(tabName = "pt_appts",
              
              fluidRow(
                box(title = "PT appointment attendance", width = 12, 
                    status = "info", solidHeader = TRUE,
                    p("This chart shows the attendance status of appointments for the selected health board."),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("pt_health_board_att", "Select health board:", 
                                choices = unique(master_appts_df$hb_name),
                                selected = default_hb),
                    radioButtons("pt_measure_type_att", "Select measurement type:", 
                                 choices = unique(master_appts_df$measure_type),
                                 selected = unique(master_appts_df$measure_type)[1]),
                    plotlyOutput("appt_att_plot_pt", height = "400px")),
                fluidRow(
                  box(title = "PT appointment by location", width = 12, 
                      status = "info", solidHeader = TRUE,
                      uiOutput("pt_appt_loc_text"),
                      actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                      selectInput("pt_health_board_loc", "Select health board:", 
                                  choices = unique(master_loc_prof_df$hb_name),
                                  selected = default_hb),
                      selectInput("pt_quarter_loc", "Select quarter:",
                                  choices = unique(master_loc_prof_df$app_quarter_ending),
                                  selected = default_quarter),
                      # radioButtons("pt_measure_type_loc", "Select measurement type:", 
                      #              choices = unique(master_loc_prof_df$measure_type),
                      #              selected = unique(master_loc_prof_df$measure_type)[1]),
                      plotlyOutput("appt_loc_plot_pt", height = "400px")),
                  fluidRow(
                    box(title = "PT appointment by professional group", width = 12, 
                        status = "info", solidHeader = TRUE,
                        uiOutput("pt_appt_prof_text"),
                        actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                        selectInput("pt_health_board_prof", "Select health board:", 
                                    choices = unique(master_loc_prof_df$hb_name),
                                    selected = default_hb),
                        selectInput("pt_quarter_prof", "Select quarter:",
                                    choices = unique(master_loc_prof_df$app_quarter_ending),
                                    selected = default_quarter),
                        # radioButtons("pt_measure_type_prof", "Select measurement type:", 
                        #              choices = unique(master_loc_prof_df$measure_type),
                        #              selected = unique(master_loc_prof_df$measure_type)[1]),
                        plotlyOutput("appt_prof_plot_pt", height = "400px"))
                  )
                )
              )
      ),
      #PT First Appointment Tab
      tabItem(tabName = "pt_first_appts",
              
              fluidRow(
                box(title = "PT first contact appointment attendance", width = 12, 
                    status = "info", solidHeader = TRUE,
                    p("This chart shows the attendance status of first contact appointments for the selected health board."),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("pt_health_board_first_appt", "Select health board:", 
                                choices = unique(master_appts_df$hb_name),
                                selected = default_hb),
                    radioButtons("pt_measure_type_first_appt", "Select measurement type:", 
                                 choices = unique(master_appts_df$measure_type),
                                 selected = unique(master_appts_df$measure_type)[1]),
                    plotlyOutput("first_appt_plot_pt", height = "400px")),
                fluidRow(
                  box(title = "PT First contact DNAs by SIMD", width = 12, 
                      status = "info", solidHeader = TRUE,
                      p("This chart shows the first contact DNA rate of the selected health board by SIMD quintile."),
                      actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                      selectInput("pt_health_board_first_appt_dna", "Select health board:", 
                                  choices = unique(first_con_dna_simd$hb_name),
                                  selected = default_hb),
                      selectInput("pt_quarter_first_appt_dna", "Select quarter:",
                                  choices = unique(first_con_dna_simd$app_quarter_ending),
                                  selected = default_quarter),
                      radioButtons("pt_measure_type_first_appt_dna", "Select measurement type:", 
                                   choices = unique(first_con_dna_simd$measure_type),
                                   selected = unique(first_con_dna_simd$measure_type)[1]),
                      plotlyOutput("first_appt_dna_pt", height = "400px"))
                )
              )
      ),
      
      # CAMHS Referrals Tab
      tabItem(tabName = "camhs_referrals",
        fluidRow(box(width = 12, status = "info", solidHeader = TRUE,
                     title = "CAMHS Referrals",
            
            div(class = "intro-html",
                includeHTML("captnd_db_ref_text_camhs.html")),
            
            #By sex
            div(class = "section-block",
                h3(class = "section-title", "CAMHS referrals by sex"),
                uiOutput("camhs_referrals_sex_text"),
                actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                selectInput("camhs_health_board_sex", "Select health board:",
                            choices = unique(ref_master_df$hb_name), selected = default_hb),
                radioButtons("camhs_measure_type_sex", "Select measurement type:",
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_sex_plot_camhs", height = "400px")),
            
            #By age
            div(class = "section-block",
                h3(class = "section-title", "CAMHS referrals by age group"),
                uiOutput("camhs_referrals_age_text"),
                actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                selectInput("camhs_health_board_age", "Select health board:",
                            choices = unique(ref_master_df$hb_name), selected = default_hb),
                radioButtons("camhs_measure_type_age", "Select measurement type:",
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_age_plot_camhs", height = "400px")),
            
            #By SIMD
            div(class = "section-block",
                h3(class = "section-title", "CAMHS referrals by SIMD"),
                uiOutput("camhs_referrals_simd_text"),
                actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                selectInput("camhs_health_board_simd", "Select health board:",
                            choices = unique(ref_master_df$hb_name), selected = default_hb),
                selectInput("camhs_quarter_simd", "Select quarter:",
                            choices = unique(ref_master_df$quarter_ending), selected = default_quarter),
                radioButtons("camhs_measure_type_simd", "Select measurement type:",
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_simd_plot_camhs", height = "400px")),
                
                #Child protection status/Looked after child status
                div(class = "section-block",
                    h3(class = "section-title", textOutput("camhs_demo_status_header")),
                    #uiOutput("pt_referrals_simd_text"),
                    selectInput("camhs_measure_demo_status", "Select variable:",
                                choices = NULL),
                    selectInput("camhs_health_board_demo_status", "Select health board:",
                                choices = NULL),
                    selectInput("camhs_quarter_demo_status", "Select quarter:",
                                choices = NULL),
                    plotlyOutput("ref_demo_status_plot_camhs", height = "400px"))
          )
        )
      ),
      
      #CAMHS Referrals Acceptance tab
      tabItem(tabName = "camhs_referrals_accept",
              
              fluidRow(
                box(title = "CAMHS referral source", width = 12, 
                    status = "info", solidHeader = TRUE,
                    uiOutput("camhs_referral_source_text"),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("camhs_health_board_ref_source", "Select health board:", 
                                choices = unique(df_ref_source$hb_name),
                                selected = default_hb),
                    selectInput("camhs_quarter_ref_source", "Select quarter:",
                                choice = unique(df_ref_source$quarter_ending),
                                selected = default_quarter),
                    # radioButtons("camhs_measure_type_ref_source", "Select measurement type:", 
                    #              choices = unique(df_ref_source$measure_type),
                    #              selected = unique(df_ref_source$measure_type)[1]),
                    plotlyOutput("ref_source_plot_camhs", height = "400px")),
              fluidRow(
                box(title = "CAMHS referrals acceptance status", width = 12, 
                    status = "info", solidHeader = TRUE,
                    #uiOutput("pt_referrals_sex_text"),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("camhs_health_board_accept", "Select health board:", 
                                choices = unique(ref_master_df$hb_name),
                                selected = default_hb),
                    radioButtons("camhs_measure_type_accept", "Select measurement type:", 
                                 choices = unique(ref_accept_df$measure_type),
                                 selected = unique(ref_accept_df$measure_type)[1]),
                    plotlyOutput("ref_accept_plot_camhs", height = "400px")),
                fluidRow(
                  box(title = "CAMHS referral rejection reasons", width = 12, 
                      status = "info", solidHeader = TRUE,
                      uiOutput("camhs_rej_reason_text"),
                      actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                      selectInput("camhs_health_board_rej_reason", "Select health board:", 
                                  choices = unique(master_non_acceptance_df$hb_name),
                                  selected = default_hb),
                      selectInput("camhs_quarter_rej_reason", "Select quarter:",
                                  choice = unique(master_non_acceptance_df$quarter_ending),
                                  selected = default_quarter),
                      # radioButtons("camhs_measure_type_rej_reason", "Select measurement type:", 
                      #              choices = unique(master_non_acceptance_df$measure_type),
                      #              selected = unique(master_non_acceptance_df$measure_type)[1]),
                      plotlyOutput("ref_rej_reason_plot_camhs", height = "400px")),
                  fluidRow(
                    box(title = "CAMHS referral rejection actions", width = 12, 
                        status = "info", solidHeader = TRUE,
                        uiOutput("camhs_rej_action_text"),
                        actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                        selectInput("camhs_health_board_rej_action", "Select health board:", 
                                    choices = unique(master_non_acceptance_df$hb_name),
                                    selected = default_hb),
                        selectInput("camhs_quarter_rej_action", "Select quarter:",
                                    choice = unique(master_non_acceptance_df$quarter_ending),
                                    selected = default_quarter),
                        # radioButtons("camhs_measure_type_rej_action", "Select measurement type:", 
                        #              choices = unique(master_non_acceptance_df$measure_type),
                        #              selected = unique(master_non_acceptance_df$measure_type)[1]),
                        plotlyOutput("ref_rej_action_plot_camhs", height = "400px"))
                  )
                ) 
              )
            )
      ),
      
      #CAMHS Appointment Tab
      tabItem(tabName = "camhs_appts",
              
              fluidRow(
                box(title = "CAMHS appointment attendance", width = 12, 
                    status = "info", solidHeader = TRUE,
                    p("This chart shows the attendance status of appointments for the selected health board."),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("camhs_health_board_att", "Select health board:", 
                                choices = unique(master_appts_df$hb_name),
                                selected = default_hb),
                    radioButtons("camhs_measure_type_att", "Select measurement type:", 
                                 choices = unique(master_appts_df$measure_type),
                                 selected = unique(master_appts_df$measure_type)[1]),
                    plotlyOutput("appt_att_plot_camhs", height = "400px")),
                fluidRow(
                  box(title = "CAMHS appointment by location", width = 12, 
                      status = "info", solidHeader = TRUE,
                      uiOutput("camhs_appt_loc_text"),
                      actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                      selectInput("camhs_health_board_loc", "Select health board:", 
                                  choices = unique(master_loc_prof_df$hb_name),
                                  selected = default_hb),
                      selectInput("camhs_quarter_loc", "Select quarter:",
                                  choices = unique(master_loc_prof_df$app_quarter_ending),
                                  selected = default_quarter),
                      # radioButtons("camhs_measure_type_loc", "Select measurement type:", 
                      #              choices = unique(master_loc_prof_df$measure_type),
                      #              selected = unique(master_loc_prof_df$measure_type)[1]),
                      plotlyOutput("appt_loc_plot_camhs", height = "400px")),
                  fluidRow(
                    box(title = "CAMHS appointment by professional group", width = 12, 
                        status = "info", solidHeader = TRUE,
                        uiOutput("camhs_appt_prof_text"),
                        actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                        selectInput("camhs_health_board_prof", "Select health board:", 
                                    choices = unique(master_loc_prof_df$hb_name),
                                    selected = default_hb),
                        selectInput("camhs_quarter_prof", "Select quarter:",
                                    choices = unique(master_loc_prof_df$app_quarter_ending),
                                    selected = default_quarter),
                        # radioButtons("camhs_measure_type_prof", "Select measurement type:", 
                        #              choices = unique(master_loc_prof_df$measure_type),
                        #              selected = unique(master_loc_prof_df$measure_type)[1]),
                        plotlyOutput("appt_prof_plot_camhs", height = "400px"))
                  )
                )
              )
      ),
      
      #CAMHS First Appointment Tab
      tabItem(tabName = "camhs_first_appts",
              
              fluidRow(
                box(title = "CAMHS first contact appointment attendance", width = 12, 
                    status = "info", solidHeader = TRUE,
                    p("This chart shows the attendance status of first contact appointments for the selected health board."),
                    actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                    selectInput("camhs_health_board_first_appt", "Select health board:", 
                                choices = unique(master_appts_df$hb_name),
                                selected = default_hb),
                    radioButtons("camhs_measure_type_first_appt", "Select measurement type:", 
                                 choices = unique(master_appts_df$measure_type),
                                 selected = unique(master_appts_df$measure_type)[1]),
                    plotlyOutput("first_appt_plot_camhs", height = "400px")),
                fluidRow(
                  box(title = "CAMHS First contact DNAs by SIMD", width = 12, 
                      status = "info", solidHeader = TRUE,
                      p("This chart shows the first contact DNA rate of the selected health board by SIMD quintile."),
                      actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                      selectInput("camhs_health_board_first_appt_dna", "Select health board:", 
                                  choices = unique(first_con_dna_simd$hb_name),
                                  selected = default_hb),
                      selectInput("camhs_quarter_first_appt_dna", "Select quarter:",
                                  choices = unique(first_con_dna_simd$app_quarter_ending),
                                  selected = default_quarter),
                      radioButtons("camhs_measure_type_first_appt_dna", "Select measurement type:", 
                                   choices = unique(first_con_dna_simd$measure_type),
                                   selected = unique(first_con_dna_simd$measure_type)[1]),
                      plotlyOutput("first_appt_dna_camhs", height = "400px"))
                )
              )
      ),
      
      #Glossary Tab
      tabItem(tabName = "glossary",
              h2("Glossary"),
              includeHTML("captnd_dashboard_glossary.html")),
      
      # Metadata Tab
      tabItem(tabName = "metadata_pt",
              h2("PT Appendices"),
              includeHTML("captnd_dashboard_pt_metadata.html")),
      
      tabItem(tabName = "metadata_camhs",
              h2("CAMHS Appendices"),
              includeHTML("captnd_dashboard_camhs_metadata.html")
              
      )
    )
  )
)
