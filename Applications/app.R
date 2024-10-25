# Install necessary packages (uncomment if not already installed)
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("DT")
# install.packages("shinyjs")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("lubridate")

library(shiny)
library(shinythemes)
library(DT)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(lubridate)

# UI Section
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    useShinyjs(),
    
    # Title Panel
    titlePanel("PhD Application Tracker"),
    
    # Sidebar layout for data input
    sidebarLayout(
        sidebarPanel(
            textInput("university", "University Name", placeholder = "Enter the university name"),
            textInput("program", "Program Name", placeholder = "Enter the program name"),
            dateInput("deadline", "Application Deadline", value = Sys.Date(), min = Sys.Date()),
            numericInput("fee", "Application Fee", value = 0, min = 0),
            textInput("link", "Application Link", placeholder = "Enter the application link"),
            selectInput("status", "Application Status", 
                        choices = c("Not Started", "In Progress", "Submitted", "Accepted", "Rejected")),
            checkboxGroupInput("components", "Application Components",
                               choices = c("Transcript", "GRE Scores", "Letters of Recommendation", "Statement of Purpose")),
            numericInput("sop_word_limit", "SOP Word Limit", value = NA, min = 0),
            numericInput("gre_score_min", "Minimum GRE Score", value = NA, min = 0),
            numericInput("ps_word_limit", "Personal Statement Word Limit", value = NA, min = 0),
            textAreaInput("extra_requirements", "Extra Requirements", placeholder = "Enter any extra requirements"),
            textAreaInput("notes", "Notes", placeholder = "Enter any notes"),
            
            actionButton("add", "Add/Update Application", icon = icon("plus")),
            actionButton("clear", "Clear Fields", icon = icon("eraser")),
            actionButton("delete", "Delete Selected", icon = icon("trash")),
            
            br(), br(),
            downloadButton("downloadData", "Download Data", icon = icon("download")),
            width = 3  # Adjust sidebar width
        ),
        
        # Main Panel for displaying tables and plots
        mainPanel(
            tabsetPanel(
                tabPanel("Applications", dataTableOutput("appTable")),
                tabPanel("Summary", plotOutput("statusPlot")),
                tabPanel("Deadlines", plotOutput("deadlinePlot"))
            ),
            width = 9  # Adjust main panel width
        )
    )
)

# Server Section
server <- function(input, output, session) {
    
    # Initialize reactive values
    values <- reactiveValues()
    values$data <- data.frame(
        University = character(),
        Program = character(),
        Deadline = as.Date(character()),
        Fee = numeric(),
        Link = character(),
        Status = character(),
        Components = character(),
        SOP_Word_Limit = numeric(),
        GRE_Min = numeric(),
        PS_Word_Limit = numeric(),
        Extra_Requirements = character(),
        Notes = character(),
        stringsAsFactors = FALSE
    )
    
    # Add or update applications
    observeEvent(input$add, {
        # Ensure mandatory fields are filled
        if (input$university == "" || input$program == "" || is.null(input$deadline)) {
            showModal(modalDialog(
                title = "Input Error",
                "Please ensure all required fields (University, Program, and Deadline) are filled out.",
                easyClose = TRUE,
                footer = NULL
            ))
            return()
        }
        
        # Ensuring non-empty inputs are provided for all fields
        newRow <- data.frame(
            University = input$university,
            Program = input$program,
            Deadline = as.Date(input$deadline),
            Fee = input$fee,
            Link = input$link,
            Status = input$status,
            Components = ifelse(length(input$components) > 0, paste(input$components, collapse = ", "), NA),
            SOP_Word_Limit = input$sop_word_limit,
            GRE_Min = input$gre_score_min,
            PS_Word_Limit = input$ps_word_limit,
            Extra_Requirements = input$extra_requirements,
            Notes = input$notes,
            stringsAsFactors = FALSE
        )
        
        # Update or add new row logic
        if (!is.null(input$appTable_rows_selected)) {
            # Update existing row
            values$data[input$appTable_rows_selected, ] <- newRow
        } else {
            # Add new row
            values$data <- rbind(values$data, newRow)
        }
        
        # Clear input fields after submission
        clearInputs()
    })
    
    # Function to clear inputs
    clearInputs <- function() {
        updateTextInput(session, "university", value = "")
        updateTextInput(session, "program", value = "")
        updateDateInput(session, "deadline", value = Sys.Date())
        updateNumericInput(session, "fee", value = 0)
        updateTextInput(session, "link", value = "")
        updateSelectInput(session, "status", selected = "Not Started")
        updateCheckboxGroupInput(session, "components", selected = character(0))
        updateNumericInput(session, "sop_word_limit", value = NA)
        updateNumericInput(session, "gre_score_min", value = NA)
        updateNumericInput(session, "ps_word_limit", value = NA)
        updateTextAreaInput(session, "extra_requirements", value = "")
        updateTextAreaInput(session, "notes", value = "")
    }
    
    # Clear button functionality
    observeEvent(input$clear, {
        clearInputs()
    })
    
    # Delete selected application
    observeEvent(input$delete, {
        if (!is.null(input$appTable_rows_selected)) {
            values$data <- values$data[-as.numeric(input$appTable_rows_selected),]
        }
    })
    
    # Populate inputs with selected row data for editing
    observeEvent(input$appTable_rows_selected, {
        selectedRow <- values$data[input$appTable_rows_selected, ]
        updateTextInput(session, "university", value = selectedRow$University)
        updateTextInput(session, "program", value = selectedRow$Program)
        updateDateInput(session, "deadline", value = selectedRow$Deadline)
        updateNumericInput(session, "fee", value = selectedRow$Fee)
        updateTextInput(session, "link", value = selectedRow$Link)
        updateSelectInput(session, "status", selected = selectedRow$Status)
        updateCheckboxGroupInput(session, "components", selected = unlist(strsplit(selectedRow$Components, ", ")))
        updateNumericInput(session, "sop_word_limit", value = selectedRow$SOP_Word_Limit)
        updateNumericInput(session, "gre_score_min", value = selectedRow$GRE_Min)
        updateNumericInput(session, "ps_word_limit", value = selectedRow$PS_Word_Limit)
        updateTextAreaInput(session, "extra_requirements", value = selectedRow$Extra_Requirements)
        updateTextAreaInput(session, "notes", value = selectedRow$Notes)
    })
    
    # Display applications table
    output$appTable <- renderDataTable({
        datatable(values$data, selection = "single", options = list(pageLength = 5, autoWidth = TRUE))
    })
    
    # Download data as CSV
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("phd_applications", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(values$data, file, row.names = FALSE)
        }
    )
    
    # Plot for application status summary using ggplot2
    output$statusPlot <- renderPlot({
        if (nrow(values$data) > 0) {
            status_counts <- values$data %>%
                group_by(Status) %>%
                summarize(Count = n())
            
            ggplot(status_counts, aes(x = Status, y = Count, fill = Status)) +
                geom_bar(stat = "identity") +
                theme_minimal() +
                labs(title = "Application Status Summary", x = "Status", y = "Number of Applications") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
    })
    
    # Plot for application deadlines using ggplot2
    output$deadlinePlot <- renderPlot({
        if (nrow(values$data) > 0) {
            deadlines <- values$data %>%
                mutate(DaysLeft = as.numeric(difftime(Deadline, Sys.Date(), units = "days"))) %>%
                arrange(DaysLeft)
            
            ggplot(deadlines, aes(x = reorder(University, DaysLeft), y = DaysLeft, color = Program, size = Fee)) +
                geom_point() +
                coord_flip() +
                theme_minimal() +
                labs(title = "Application Deadlines", x = "University", y = "Days Left to Deadline") +
                theme(axis.text.y = element_text(size = 8))
        }
    })
}

shinyApp(ui = ui, server = server)
