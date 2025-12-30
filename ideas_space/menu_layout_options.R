
############# Tab option 1
tabItem(tabName = "camhs_referrals",
        
        fluidRow(
          
          box(
            width = 12, status = "info", solidHeader = TRUE,
            title = "CAMHS referrals",
            div(class = "intro-html", includeHTML("captnd_db_ref_text_camhs.html")),
            
            
            tabBox(width = 12, id = "camhs_tabbox",
                   
                   # Tab 1: By sex
                   tabPanel("CAMHS referrals by sex",
                            uiOutput("camhs_referrals_sex_text"),
                            actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
                            selectInput("camhs_health_board_sex", "Select health board:",
                                        choices = unique(ref_master_df$hb_name), selected = default_hb),
                            radioButtons("camhs_measure_type_sex", "Select measurement type:",
                                         choices = unique(ref_master_df$measure_type),
                                         selected = unique(ref_master_df$measure_type)[1]),
                            plotlyOutput("ref_sex_plot_camhs", height = "400px")),
                   
                   # Tab 2: By age
                   tabPanel("CAMHS referrals by age group",
                            p("This chart shows the trend of referrals broken down by age group for the selected health board."),
                            selectInput("camhs_health_board_age", "Select health board:",
                                        choices = unique(ref_master_df$hb_name), selected = default_hb),
                            radioButtons("camhs_measure_type_age", "Select measurement type:",
                                         choices = unique(ref_master_df$measure_type),
                                         selected = unique(ref_master_df$measure_type)[1]),
                            plotlyOutput("ref_age_plot_camhs", height = "400px")),
                   
                   # Tab 3: By SIMD
                   tabPanel("CAMHS referrals by SIMD",
                            p("This chart shows the trend of referrals broken down by SIMD for the selected health board."),
                            selectInput("camhs_health_board_simd", "Select health board:",
                                        choices = unique(ref_master_df$hb_name), selected = default_hb),
                            selectInput("camhs_quarter_simd", "Select quarter:",
                                        choices = unique(ref_master_df$quarter_ending), selected = default_quarter),
                            radioButtons("camhs_measure_type_simd", "Select measurement type:",
                                         choices = unique(ref_master_df$measure_type),
                                         selected = unique(ref_master_df$measure_type)[1]),
                            plotlyOutput("ref_simd_plot_camhs", height = "400px"))
            )
          )
        )
)

############# Tab option 2 (classic)
tabItem(tabName = "camhs_referrals",
        
        fluidRow(
          box(title = "CAMHS referrals by sex", width = 12, status = "info", solidHeader = TRUE,
              uiOutput("camhs_referrals_sex_text"),
              actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
              selectInput("camhs_health_board_sex", "Select health board:", 
                          choices = unique(ref_master_df$hb_name),
                          selected = default_hb),
              radioButtons("camhs_measure_type_sex", "Select measurement type:", 
                           choices = unique(ref_master_df$measure_type),
                           selected = unique(ref_master_df$measure_type)[1]),
              plotlyOutput("ref_sex_plot_camhs", height = "400px")),
          
          fluidRow(
            box(title = "CAMHS referrals by age group", width = 12, status = "info", solidHeader = TRUE,
                p("This chart shows the trend of referrals broken down by sex for the selected health board."),
                selectInput("camhs_health_board_age", "Select health board:", 
                            choices = unique(ref_master_df$hb_name),
                            selected = default_hb),
                radioButtons("camhs_measure_type_age", "Select measurement type:", 
                             choices = unique(ref_master_df$measure_type),
                             selected = unique(ref_master_df$measure_type)[1]),
                plotlyOutput("ref_age_plot_camhs", height = "400px")),
            
            fluidRow(
              box(title = "CAMHS referrals by SIMD", width = 12, status = "info", solidHeader = TRUE,
                  p("This chart shows the trend of referrals broken down by SIMD for the selected health board."),
                  selectInput("camhs_health_board_simd", "Select health board:",  
                              choices = unique(ref_master_df$hb_name),
                              selected = default_hb),
                  selectInput("camhs_quarter_simd", "Select quarter:",
                              choice = unique(ref_master_df$quarter_ending),
                              selected = default_quarter),
                  radioButtons("camhs_measure_type_simd", "Select measurement type:", 
                               choices = unique(ref_master_df$measure_type),
                               selected = unique(ref_master_df$measure_type)[1]),
                  plotlyOutput("ref_simd_plot_camhs", height = "400px"))
            )
          )
        )
)

############# Tab option 3
tabItem(
  tabName = "camhs_referrals",
  fluidRow(
    box(
      width = 12, status = "info", solidHeader = TRUE,
      title = "CAMHS referrals",
      
      # ---- Intro (HTML file) ----
      div(class = "intro-html",
          includeHTML("captnd_db_ref_text_camhs.html")
      ),
      
      # ---- Section: By sex ----
      div(class = "section-block",
          h3(class = "section-title", "CAMHS referrals by sex"),
          uiOutput("camhs_referrals_sex_text"),
          actionButton("info_btn", "Using the plot", icon = icon("arrow-pointer")),
          selectInput("camhs_health_board_sex", "Select health board:",
                      choices = unique(ref_master_df$hb_name), selected = default_hb),
          radioButtons("camhs_measure_type_sex", "Select measurement type:",
                       choices = unique(ref_master_df$measure_type),
                       selected = unique(ref_master_df$measure_type)[1]),
          plotlyOutput("ref_sex_plot_camhs", height = "400px")
      ),
      
      # ---- Section: By age ----
      div(class = "section-block",
          h3(class = "section-title", "CAMHS referrals by age group"),
          p("This chart shows the trend of referrals broken down by age group for the selected health board."),
          selectInput("camhs_health_board_age", "Select health board:",
                      choices = unique(ref_master_df$hb_name), selected = default_hb),
          radioButtons("camhs_measure_type_age", "Select measurement type:",
                       choices = unique(ref_master_df$measure_type),
                       selected = unique(ref_master_df$measure_type)[1]),
          plotlyOutput("ref_age_plot_camhs", height = "400px")
      ),
      
      # ---- Section: By SIMD ----
      div(class = "section-block",
          h3(class = "section-title", "CAMHS referrals by SIMD"),
          p("This chart shows the trend of referrals broken down by SIMD for the selected health board."),
          selectInput("camhs_health_board_simd", "Select health board:",
                      choices = unique(ref_master_df$hb_name), selected = default_hb),
          selectInput("camhs_quarter_simd", "Select quarter:",
                      choices = unique(ref_master_df$quarter_ending), selected = default_quarter),
          radioButtons("camhs_measure_type_simd", "Select measurement type:",
                       choices = unique(ref_master_df$measure_type),
                       selected = unique(ref_master_df$measure_type)[1]),
          plotlyOutput("ref_simd_plot_camhs", height = "400px")
      )
    )
  )
)






