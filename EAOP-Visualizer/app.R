#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## packages for Shiny app
library(shiny)
library(httr)
library(jsonlite)
library(R.utils)

## packages from EAOP analysis
library(xml2)
library(igraph)
library(prodlim)
library(RColorBrewer)
library(plotrix)
library(autoimage)
library(dplyr)
library(openxlsx)
library(readxl)
library(data.table)
library(ggplot2)
library(latex2exp)
library(ggrepel)

#Symbol assignment
`%!in%`<-Negate(`%in%`) #"not in" command created

library(shiny)
library(httr)
library(jsonlite)
library(R.utils)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Emergent AOP Visualizer and Quality Evaluation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("file", "Choose AOP-Wiki XML:", choices = NULL, selectize = FALSE),
      actionButton("process", "Extract + Load XML", id = "processBtn"),
      actionButton("refresh", label = NULL, icon = icon("refresh"))
    ),
    mainPanel(
      textOutput("selected_file"),
      verbatimTextOutput("extracted_file_name"),
      textOutput("file_result"),
      textOutput("analysis_result")
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

server <- function(input, output, session) {

  # Define the GitHub repository and directory
  owner <- "npollesch"
  repo <- "EAOP-Viz"
  base_directory <- "AOP-Wiki-XML"
  script_path <- "Source/AOP-Net-Functions.R"

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

  # Function to download and extract a .gz file from GitHub
  download_and_extract_gz <- function(owner, repo, directory, file_name) {
    file_url <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/main/", directory, "/", file_name)
    temp_file <- tempfile(fileext = ".gz")
    download.file(file_url, temp_file, mode = "wb")

    # Extract the .gz file to a temporary location
    extracted_file <- gunzip(temp_file, remove = FALSE, temporary = TRUE)
    return(extracted_file)
  }

  # Function to download and source an R script from GitHub
  source_github_script <- function(owner, repo, script_path) {
    script_url <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/main/", script_path)
    temp_script <- tempfile(fileext = ".R")
    download.file(script_url, temp_script, mode = "wb")
    source(temp_script)
  }

  # Source the R script
  source_github_script(owner, repo, script_path)

  # Get the list of files
  file_list <- get_github_files(owner, repo, base_directory)

  # Update the select input with file list
  updateSelectInput(session, "file", choices = file_list)

  # Reactive value to store the path of the extracted file
  extracted_file_path <- reactiveVal(NULL)

  output$selected_file <- renderText({
    paste("You have selected:", input$file)
  })

  observeEvent(input$process, {
    req(input$file)  # Ensure a file is selected
    if (grepl("\\.gz$", input$file)) {
      extracted_file <- download_and_extract_gz(owner, repo, base_directory, input$file)
      extracted_file_path(extracted_file)  # Store the path in the reactive value
      output$file_result <- renderText({
        paste("The file", input$file, "has been extracted.")
      })

      # Display the name of the original .gz file
      output$extracted_file_name <- renderText({
        paste("Original .gz File Name:", input$file)
      })

      # Example analysis: Count lines in the extracted file
      file_content <- readLines(extracted_file)
      num_lines <- length(file_content)
      output$analysis_result <- renderText({
        paste("The extracted file has", num_lines, "lines.")
      })

      # Disable the dropdown and process button after processing
      session$sendCustomMessage(type = 'disableElement', message = 'file')
      session$sendCustomMessage(type = 'disableElement', message = 'processBtn')

      # Clean up the extracted file at

      session$onSessionEnded(function() {
        if (file.exists(extracted_file)) {
          file.remove(extracted_file)
        }
      })
    } else {
      output$file_result <- renderText({
        "Please select a .gz file."
      })
    }
  })

  observeEvent(input$refresh, {
    # Enable the dropdown and process button when refresh button is clicked
    session$sendCustomMessage(type = 'enableElement', message = 'file')
    session$sendCustomMessage(type = 'enableElement', message = 'processBtn')

    # Clear the UI outputs
    output$selected_file <- renderText({ "" })
    output$extracted_file_name <- renderText({ "" })
    output$file_result <- renderText({ "" })
    output$analysis_result <- renderText({ "" })
  })
}

# Run the application
shinyApp(ui = ui, server = server)













# ##Adapting EAOP extraction code
# #fName<-paste(sourceDir,"/Data/AOP-wiki-data.xml",sep="") ##This assumes that there is a subfolder 'Data/AOPWikiSnapshots' within the working directory where the XML file is located
#
# xData<-read_xml(input$file)
# xData<-xml_ns_strip(xData)
#
# ## Ref ID to AOPwiki ID
#
# keID<-data.frame(
#   ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"id"),
#   ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"),"aop-wiki-id"),
#   stringsAsFactors=FALSE
# )
#
# kerID<-data.frame(
#   ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"id"),
#   ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"),"aop-wiki-id"),
#   stringsAsFactors=FALSE
# )
#
# aopID<-data.frame(
#   ref=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"id"),
#   ID=xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"),"aop-wiki-id"),
#   stringsAsFactors=FALSE
# )
#
#
# ## Key event (KE) Data
#
# keData<-data.frame(
#   ID=keID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event"), "id"),keID$ref)],
#   title=xml_text(xml_find_all(xData, "/data/key-event/title")),
#   LOBO=xml_text(xml_find_all(xData, "/data/key-event/biological-organization-level")),
#   stringsAsFactors=FALSE
# )
#
#
# ## Key event relationship (KER) Data
#
# kerData<-data.frame(
#   ID=kerID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event-relationship"), "id"),kerID$ref)],
#   KEup=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/upstream-id")),keID$ref)],
#   KEdown=keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/downstream-id")),keID$ref)],
#   stringsAsFactors=FALSE
# )
#
#
# ## AOP data
#
# ## OECD status: not all aops have an "oecd-status" xml tag, so must us "if" to return NA when missing
# oecdStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
#   if("oecd-status"%in%xml_name(xml_children(x))){
#     return(xml_text(xml_find_all(x,"oecd-status")))
#   }else{
#     return("not specified")
#   }
# })
#
# ## SAAOP status: not all aops have an "saaop-status" xml tag, so must us "if" to return NA when missing
# saaopStatus<-sapply(xml_find_all(xData, "/data/aop/status"),FUN=function(x){
#   if("saaop-status"%in%xml_name(xml_children(x))){
#     return(xml_text(xml_find_all(x,"saaop-status")))
#   }else{
#     return("not specified")
#   }
# })
#
# ## MIEs: more than one MIE possible per aop, so must return list
# mies<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
#   if("molecular-initiating-event"%in%xml_name(xml_children(x))){
#     return(keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"),"key-event-id"),keID$ref)])
#   }else{
#     return(NULL)
#   }
# })
#
# ## AOs: more than one AO possible per aop, so must return list
# aos<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
#   if("adverse-outcome"%in%xml_name(xml_children(x))){
#     return(keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)])
#   }else{
#     return(NULL)
#   }
# })
#
#
# ## KEs: more than one KE possible per aop, so must return list
# kes<-lapply(xml_find_all(xData, "/data/aop/key-events"),FUN=function(x){
#   if("key-event"%in%xml_name(xml_children(x))){
#     return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"id"),keID$ref)])
#   }else{
#     return(NULL)
#   }
# })
#
# ## KERs: more than one KER per aop, each with aop-specific "adjaceny", "quantitative understanding", and "WoE"
# ## So must return data frame of KERs
# kers<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
#   if("relationship"%in%xml_name(xml_children(x))){
#     return(data.frame(
#       ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
#       adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
#       quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
#       woe=xml_text(xml_find_all(x, "relationship/evidence")),
#       stringsAsFactors=FALSE
#     ))
#   }else{
#     return(NULL)
#   }
# })
#
# ## add kes and MIE/AO designation (which is AOP-specific) for each KER in kers data.frame
# for(i in 1:length(kers)){
#   if(length(kers[[i]])>0){
#     KEup<-kerData$KEup[match(kers[[i]]$ID,kerData$ID)]
#     KEDup<-sapply(KEup, FUN=function(x){
#       if(x%in%mies[[i]]){
#         return("MIE")
#       }else{
#         if(x%in%aos[[i]]){
#           return("AO")
#         }else{
#           return("KE")
#         }
#       }
#     })
#
#     KEdown<-kerData$KEdown[match(kers[[i]]$ID,kerData$ID)]
#     KEDdown<-sapply(KEdown, FUN=function(x){
#       if(x%in%mies[[i]]){
#         return("MIE")
#       }else{
#         if(x%in%aos[[i]]){
#           return("AO")
#         }else{
#           return("KE")
#         }
#       }
#     })
#
#     kers[[i]]<-data.frame(
#       ID=kers[[i]]$ID,
#       KEup=KEup,
#       KEDup=KEDup,
#       KEdown=KEdown,
#       KEDdown=KEDdown,
#       adjacency=kers[[i]]$adjacency,
#       quant=kers[[i]]$quant,
#       woe=kers[[i]]$woe,
#       row.names=NULL,
#       stringsAsFactors = FALSE
#     )
#   }
# }
#
#
# aopData<-data.frame(
#   ID=aopID$ID[match(xml_attr(xml_find_all(xData, "/data/aop"), "id"),aopID$ref)],
#   oecdStatus=oecdStatus,
#   saaopStatus=saaopStatus,
#   mies=I(mies),
#   aos=I(aos),
#   kes=I(kes),
#   kers=I(kers),
#   stringsAsFactors=FALSE
# )
#

