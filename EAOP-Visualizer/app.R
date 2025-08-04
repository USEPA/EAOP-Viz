library(shiny)
library(shinyjs)  # Add shinyjs for enabling/disabling UI elements

# Source module files
source("modules/extractLoadWikiXML.R")
source("modules/processAOPWikiXML.R")

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  titlePanel("Modularized Shiny App"),
  sidebarLayout(
    sidebarPanel(
      extractLoadWikiXMLUI("extractLoadWikiXML"),
      br(),  # Add a line break for spacing
      processAOPWikiXMLUI("processAOPWikiXML")  # Move to its own line
    ),
    mainPanel(
      verbatimTextOutput("extracted_file_name"),
      textOutput("file_result"),
      textOutput("analysis_result"),
      verbatimTextOutput("ke_summary"),
      verbatimTextOutput("ker_summary"),
      verbatimTextOutput("aop_summary")
    )
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('disableElement', function(id) {
      document.getElementById(id).disabled = true;
    });
    Shiny.addCustomMessageHandler('enableElement', function(id) {
      document.getElementById(id).disabled = false;
    });
  "))
)

# Define server logic
server <- function(input, output, session) {
  extracted_file <- callModule(extractLoadWikiXML, "extractLoadWikiXML", output)
  callModule(processAOPWikiXML, "processAOPWikiXML", extracted_file, output)
}

# Run the application
shinyApp(ui = ui, server = server)
