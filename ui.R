##### UI #####
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel("CAPTND Dashboard"),
  
  tabsetPanel(
    id = "data_tab",
    
    #Panel for Referral Demographics
    tabPanel("Referrals by Demographic",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("dataset_type_master", "Select Dataset Type:",
                              choices = unique(ref_master_df$dataset_type),
                              selected = default_dataset),
                 selectInput("health_board_master", "Select Health Board:",
                             choices = unique(ref_master_df$hb_name),
                             selected = default_hb),
                 radioButtons("demographic_type_master", "Select Demographic:",
                              choices = unique(ref_master_df$measure_name),
                              selected = default_measure),
                 selectInput("demographic_options_master", "Select Variable:",
                             choices = default_choices,
                             selected = default_selected,
                             multiple = TRUE),
                 radioButtons("measure_type_master", "Select Measurement Type:",
                              choices = unique(ref_master_df$measure_type),
                              selected = unique(ref_master_df$measure_type)[1])
               ),
               mainPanel(
                 plotlyOutput("master_plot"),
                 br(),
                 fluidRow(
                   column(6, actionButton("show_table_master", "Show Table")),
                   column(6, downloadButton("download_master_data", "Download Data"))
                 ),
                 br(), br(),
                 DTOutput("table_master")
               )
             )
    ),
    
    #Panel for Referral Acceptance Status
    tabPanel("Referral Acceptance Status",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("dataset_type_accept", "Select Dataset Type:",
                              choices = unique(ref_accept_df$dataset_type),
                              selected = unique(ref_accept_df$dataset_type)[1]),
                 selectInput("health_board_accept", "Select Health Board:",
                             choices = unique(ref_accept_df$hb_name),
                             selected = unique(ref_accept_df$hb_name)[1]),
                 radioButtons("measure_type_accept", "Select Measurement Type:",
                              choices = unique(ref_accept_df$measure_type),
                              selected = unique(ref_accept_df$measure_type)[1])
               ),
               mainPanel(
                 plotlyOutput("accept_plot")
               )
             ),
             
    ),
    
    #Panel for Appt Attendance Status
    tabPanel("Appointment Attendance Status",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("dataset_type_appts", "Select Dataset Type:",
                              choices = unique(master_appts_df$dataset_type),
                              selected = unique(master_appts_df$dataset_type)[1]),
                 selectInput("health_board_appts", "Select Health Board:",
                             choices = unique(master_appts_df$hb_name),
                             selected = unique(master_appts_df$hb_name)[1]),
                 radioButtons("contact_type_appts", "Select Measure:",
                              choices = unique(master_appts_df$measure_name),
                              selected = unique(master_appts_df$measure_name)[1]),
                 selectInput("attendance_status_appts", "Attendance Status:",
                             choices = unique(master_appts_df$measure_breakdown),
                             selected = unique(master_appts_df$measure_breakdown)[1],
                             multiple = TRUE),
                 radioButtons("measure_type_appts", "Select Measurement Type:",
                              choices = unique(master_appts_df$measure_type),
                              selected = unique(master_appts_df$measure_type)[1])
               ),
               mainPanel(
                 plotlyOutput("appt_plot")
               )
             ),
             
    )
  )
)

