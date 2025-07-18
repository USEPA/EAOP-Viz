#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(httr)
library(jsonlite)
# Define the GitHub repository and directory
owner <- "npollesch"
repo <- "EAOP-Viz"
directory <- "AOP-Wiki-XML/"

# Function to get file names from GitHub
get_github_files <- function(owner, repo, directory) {
  url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", directory)
  response <- GET(url)
  content <- content(response, "text")
  file_info <- fromJSON(content)

  # Extract file names
  file_names <- file_info$name
  return(file_names)
}

# Get the list of files
file_list <- get_github_files(owner, repo, directory)

# Define UI for the application
ui <- fluidPage(
  titlePanel("GitHub File Selector"),
  sidebarLayout(
    sidebarPanel(
      selectInput("file", "Choose a file:", choices = file_list)
    ),
    mainPanel(
      textOutput("selected_file")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$selected_file <- renderText({
    paste("You have selected:", input$file)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
