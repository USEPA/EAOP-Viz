library(httr)
library(jsonlite)
library(R.utils)

extractLoadWikiXMLUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("file"), "Choose AOP-Wiki XML:", choices = NULL, selectize = FALSE),
    actionButton(ns("process"), "Extract + Load XML"),
    actionButton(ns("refresh"), label = NULL, icon = icon("refresh"))
  )
}

extractLoadWikiXML <- function(input, output, session, output_main) {
  owner <- "npollesch"
  repo <- "EAOP-Viz"
  base_directory <- "AOP-Wiki-XML"

  get_github_files <- function(owner, repo, directory) {
    url <- paste0("https://api.github.com/repos/", owner, "/", repo, "/contents/", directory)
    response <- tryCatch({
      GET(url)
    }, error = function(e) {
      return(NULL)
    })

    if (is.null(response) || http_status(response)$category != "Success") {
      return(character(0))
    }

    content <- content(response, "text")
    file_info <- fromJSON(content)
    file_names <- file_info$name
    return(file_names)
  }

  download_and_extract_gz <- function(owner, repo, directory, file_name) {
    file_url <- paste0("https://raw.githubusercontent.com/", owner, "/", repo, "/main/", directory, "/", file_name)
    temp_file <- tempfile(fileext = ".gz")
    tryCatch({
      download.file(file_url, temp_file, mode = "wb")
      extracted_file <- gunzip(temp_file, remove = FALSE, temporary = TRUE)
      return(extracted_file)
    }, error = function(e) {
      return(NULL)
    })
  }

  file_list <- get_github_files(owner, repo, base_directory)
  updateSelectInput(session, "file", choices = file_list)

  extracted_file_path <- reactiveVal(NULL)

  observeEvent(input$process, {
    req(input$file)
    withProgress(message = 'Extracting...', value = 0, {
      if (grepl("\\.gz$", input$file)) {
        incProgress(0.3, detail = "Downloading file...")
        extracted_file <- download_and_extract_gz(owner, repo, base_directory, input$file)
        if (!is.null(extracted_file)) {
          extracted_file_path(extracted_file)
          output_main$file_result <- renderText({
            paste("The file", input$file, "has been extracted.")
          })

          output_main$extracted_file_name <- renderText({
            paste("Original .gz File Name:", input$file)
          })

          incProgress(0.6, detail = "Reading file content...")
          file_content <- tryCatch({
            readLines(extracted_file)
          }, error = function(e) {
            return(NULL)
          })

          if (!is.null(file_content)) {
            num_lines <- length(file_content)
            output_main$analysis_result <- renderText({
              paste("The extracted file has", num_lines, "lines.")
            })
          } else {
            output_main$analysis_result <- renderText({
              "Error processing the extracted file."
            })
          }

          incProgress(1, detail = "Done.")

          # Disable the dropdown and process button after processing
          session$sendCustomMessage(type = 'disableElement', message = session$ns('file'))
          session$sendCustomMessage(type = 'disableElement', message = session$ns('process'))

          session$onSessionEnded(function() {
            if (file.exists(extracted_file)) {
              file.remove(extracted_file)
            }
          })
        } else {
          output_main$file_result <- renderText({
            "Please select a .gz file."
          })
        }
      }
    })
  })

  observeEvent(input$refresh, {
    # Enable the dropdown and process button when refresh button is clicked
    session$sendCustomMessage(type = 'enableElement', message = session$ns('file'))
    session$sendCustomMessage(type = 'enableElement', message = session$ns('process'))

    # Clear all UI outputs
    output_main$extracted_file_name <- renderText({ "" })
    output_main$file_result <- renderText({ "" })
      output_main$analysis_result <- renderText({ "" })
      output_main$ke_summary <- renderText({ "" })
      output_main$ker_summary <- renderText({ "" })
      output_main$aop_summary <- renderText({ "" })

      # Remove the extracted file from memory
      if (!is.null(extracted_file_path()) && file.exists(extracted_file_path())) {
        file.remove(extracted_file_path())
      }
      extracted_file_path(NULL)  # Clear the reactive value
    })

    return(extracted_file_path)
  }
