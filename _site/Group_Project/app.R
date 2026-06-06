library(shiny)
library(shinydashboard)

ui <- navbarPage(
  title = "Group Project - Visual Analytics",

  # Proposal Tab
  tabPanel("Proposal",
           fluidPage(
             titlePanel("Project Proposal"),

             fluidRow(
               column(12,
                      h3("Project Overview"),
                      p("Add your project overview here..."),

                      h3("Objectives"),
                      tags$ul(
                        tags$li("Objective 1"),
                        tags$li("Objective 2"),
                        tags$li("Objective 3")
                      ),

                      h3("Data Source"),
                      p("Describe your data source here..."),

                      h3("Methodology"),
                      p("Describe your methodology here..."),

                      h3("Expected Outcomes"),
                      p("Describe expected outcomes here...")
               )
             )
           )
  ),

  # Data Exploration Tab
  tabPanel("Data Exploration",
           fluidPage(
             titlePanel("Data Exploration"),
             sidebarLayout(
               sidebarPanel(
                 h4("Upload Data"),
                 fileInput("file1", "Choose CSV File",
                          accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 hr(),
                 p("Data exploration tools will appear here...")
               ),
               mainPanel(
                 h4("Data Preview"),
                 tableOutput("contents")
               )
             )
           )
  ),

  # Visualization Tab
  tabPanel("Visualization",
           fluidPage(
             titlePanel("Data Visualization"),
             p("Add your visualizations here...")
           )
  ),

  # Analysis Tab
  tabPanel("Analysis",
           fluidPage(
             titlePanel("Statistical Analysis"),
             p("Add your analysis here...")
           )
  ),

  # About Tab
  tabPanel("About",
           fluidPage(
             titlePanel("About This Project"),
             fluidRow(
               column(12,
                      h3("Team Members"),
                      tags$ul(
                        tags$li("Member 1"),
                        tags$li("Member 2"),
                        tags$li("Member 3")
                      ),

                      h3("Contact Information"),
                      p("Add contact information here...")
               )
             )
           )
  )
)

server <- function(input, output, session) {

  # Data upload handling
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    head(df, 10)
  })

}

shinyApp(ui = ui, server = server)
