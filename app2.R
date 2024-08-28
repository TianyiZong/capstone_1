# Define the UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .title { 
        text-align: center;
      }
      .well { 
        text-align: left;
      }
      .stats-panel {
        padding: 10px;
        background-color: #f5f5f5;
        border-radius: 5px;
        margin-bottom: 15px;
      }
      .checkbox-inline, .radio-inline {
        margin-left: 0px;
        margin-right: 10px;
      }
      .centered-heading {
        text-align: center;
      }
    "))
  ),
  titlePanel(
    div(class = "title", "Patients with Hypertension")
  ),
  fluidRow(
    column(12,
           wellPanel(
             h3(class = "centered-heading", "Filter Patients"),
             fluidRow(
               column(2, h4("Gender:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("gender", NULL, choices = unique(filtered_patients_info$gender), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Race:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("race", NULL, choices = unique(filtered_patients_info$race), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Marital Status:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("marital_status", NULL, choices = unique(filtered_patients_info$marital_status), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Insurance:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("insurance", NULL, choices = unique(filtered_patients_info$insurance), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Admission Type:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("admission_type", NULL, choices = unique(filtered_patients_info$admission_type), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Admission Location:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("admission_location", NULL, choices = unique(filtered_patients_info$admission_location), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Discharge Location:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("discharge_location", NULL, choices = unique(filtered_patients_info$discharge_location), inline = TRUE)))
             ),
             fluidRow(
               column(2, h4("Mortality:")),
               column(10, div(style="display: inline-block; text-align: left;", checkboxGroupInput("is_dead", NULL, choices = unique(filtered_patients_info$is_dead), inline = TRUE)))
             )
           )
    )
  ),
  fluidRow(
    column(3,
           div(class = "stats-panel",
               h4("Statistics"),
               textOutput("avgAge")
           )
    ),
    column(9,
           plotOutput("bpPlot")
    )
  ),
  fluidRow(
    column(12,
           plotOutput("hospitalMap")
    )
  ),
  titlePanel("Admission and Discharge Location Percentages"),
  sidebarLayout(
    sidebarPanel(
      h3("Admission Location Summary"),
      uiOutput("admissionSummary"),
      h3("Discharge Location Summary"),
      uiOutput("dischargeSummary"),
      h3("Select Admission Location"),
      selectInput("selected_admission_location", "Admission Location:", choices = unique(filtered_patients_info$admission_location), selected = NULL)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Admission Percentages", plotOutput("admissionPlot")),
        tabPanel("Discharge Percentages", plotOutput("dischargePlot")),
        tabPanel("Long Title Frequency", plotOutput("longTitlePlot"))
      )
    )
  )
)
# Define the server logic
server <- function(input, output, session) {
  
  # Reactive function to filter patients based on input criteria
  filtered_patients <- reactive({
    patients <- filtered_patients_info
    
    if (!is.null(input$gender) && length(input$gender) > 0) {
      patients <- patients %>% filter(gender %in% input$gender)
    }
    
    if (!is.null(input$race) && length(input$race) > 0) {
      patients <- patients %>% filter(race %in% input$race)
    }
    
    if (!is.null(input$marital_status) && length(input$marital_status) > 0) {
      patients <- patients %>% filter(marital_status %in% input$marital_status)
    }
    
    if (!is.null(input$insurance) && length(input$insurance) > 0) {
      patients <- patients %>% filter(insurance %in% input$insurance)
    }
    
    if (!is.null(input$admission_type) && length(input$admission_type) > 0) {
      patients <- patients %>% filter(admission_type %in% input$admission_type)
    }
    
    if (!is.null(input$admission_location) && length(input$admission_location) > 0) {
      patients <- patients %>% filter(admission_location %in% input$admission_location)
    }
    
    if (!is.null(input$discharge_location) && length(input$discharge_location) > 0) {
      patients <- patients %>% filter(discharge_location %in% input$discharge_location)
    }
    
    if (!is.null(input$is_dead) && length(input$is_dead) > 0) {
      patients <- patients %>% filter(is_dead %in% input$is_dead)
    }
    
    patients
  })
  
  # Reactive function to filter blood pressure records based on filtered patients
  filtered_bp_records <- reactive({
    filtered_subject_ids <- filtered_patients()$subject_id
    bp_records %>% filter(subject_id %in% filtered_subject_ids)
  })
  
  # Reactive function to calculate average age of filtered patients
  average_age <- reactive({
    patients <- filtered_patients()
    if(nrow(patients) > 0) {
      avg_age <- mean(patients$anchor_age, na.rm = TRUE)
      return(round(avg_age, 2))
    } else {
      return(NA)
    }
  })
  
  # Output the average age
  output$avgAge <- renderText({
    avg_age <- average_age()
    if(is.na(avg_age)) {
      "Average Age: N/A"
    } else {
      paste("Average Age:", avg_age)
    }
  })
  
  # Output the blood pressure plot
  output$bpPlot <- renderPlot({
    patient_data <- filtered_bp_records()
    
    data <- patient_data %>%
      arrange(subject_id, days_of_measurement) %>%
      mutate(next_days_of_measurement = lead(days_of_measurement),
             next_control = lead(hypertension_control)) %>%
      filter(!is.na(next_days_of_measurement))
    
    ggplot() +
      geom_point(data = patient_data, aes(x = days_of_measurement, y = as.factor(subject_id), color = hypertension_control), size = 2) +
      geom_segment(data = data,
                   aes(x = days_of_measurement, xend = next_days_of_measurement, y = as.factor(subject_id), yend = as.factor(subject_id), color = hypertension_control),
                   size = 1) +  
      scale_color_manual(values = c("controlled" = "blue", "not controlled" = "red")) +
      labs(title = "Blood Pressure Control Trajectories",
           x = "Days of Measurement",
           y = "Patient ID",
           color = "Control Status") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),  # Remove Y axis text to avoid overcrowding
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank())  # Remove Y axis title as well
  })
  
  
  # Reactive function to calculate percentages for the map
  location_percentages <- reactive({
    patients <- filtered_patients()
    
    admission_percent <- patients %>%
      group_by(admission_location) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100)
    
    discharge_percent <- patients %>%
      group_by(discharge_location) %>%
      summarise(count = n()) %>%
      mutate(percentage = (count / sum(count)) * 100)
    
    list(admission = admission_percent, discharge = discharge_percent)
  })
  
  output$admissionPlot <- renderPlot({
    admission_data <- location_percentages()$admission
    ggplot(admission_data, aes(x = admission_location, y = percentage)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme_minimal() +
      labs(title = "Admission Location Percentages",
           x = "Admission Location",
           y = "Percentage")
  })
  
  output$dischargePlot <- renderPlot({
    discharge_data <- location_percentages()$discharge
    ggplot(discharge_data, aes(x = discharge_location, y = percentage)) +
      geom_bar(stat = "identity", fill = "darkorange") +
      theme_minimal() +
      labs(title = "Discharge Location Percentages",
           x = "Discharge Location",
           y = "Percentage")
  })
  
  output$admissionSummary <- renderUI({
    admission_data <- location_percentages()$admission
    summary_text <- paste(
      "Total Admissions: ", sum(admission_data$count), "<br>",
      paste(admission_data$admission_location, ": ", admission_data$count, " (", round(admission_data$percentage, 2), "%)", sep = "", collapse = "<br>")
    )
    HTML(summary_text)
  })
  
  output$dischargeSummary <- renderUI({
    discharge_data <- location_percentages()$discharge
    summary_text <- paste(
      "Total Discharges: ", sum(discharge_data$count), "<br>",
      paste(discharge_data$discharge_location, ": ", discharge_data$count, " (", round(discharge_data$percentage, 2), "%)", sep = "", collapse = "<br>")
    )
    HTML(summary_text)
  })
  
  # Output the long title frequency plot for selected admission location
  output$longTitlePlot <- renderPlot({
    req(input$selected_admission_location)  # Ensure a location is selected
    
    selected_patients <- filtered_patients() %>%
      filter(admission_location == input$selected_admission_location)
    
    if(nrow(selected_patients) == 0) return(NULL)  # If no patients match, return nothing
    
   merged_data <- merge(selected_patients,hypertension_data, by = c("subject_id", "hadm_id"))

    long_title_freq <- merged_data %>%
      group_by(long_title) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      top_n(10, count)  # Show only top 10
    
    ggplot(long_title_freq, aes(x = reorder(long_title, -count), y = count)) +
      geom_bar(stat = "identity", fill = "purple") +
      coord_flip() +
      labs(title = paste("Top 10 Reasons for Admission at", input$selected_admission_location),
           x = "ICD Procedure (long_title)",
           y = "Count") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

