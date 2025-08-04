library(xml2)
library(shiny)
library(shinyjs)
library(dplyr)

# Source the AOP-Net-Functions.R script from the source folder within modules
source("modules/source/AOP-Net-Functions.R")

processAOPWikiXMLUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("processXML"), "Process XML"),
    textOutput(ns("error_message")),
    textOutput(ns("success_message")),
    verbatimTextOutput(ns("kes_full_contents")),
    verbatimTextOutput(ns("aopswithkes_contents"))
  )
}

processAOPWikiXML <- function(input, output, session, extracted_file, output_main) {
  observe({
    # Disable the process button if no file is extracted
    shinyjs::disable("processXML")
    if (!is.null(extracted_file()) && file.exists(extracted_file())) {
      shinyjs::enable("processXML")
    }
  })

  observeEvent(input$processXML, {
    req(extracted_file())
    withProgress(message = 'Processing XML...', value = 0, {
      tryCatch({
        # Read and process the XML file
        incProgress(0.3, detail = "Reading XML file...")
        xData <- read_xml(extracted_file())
        xData <- xml_ns_strip(xData)
        output$success_message <- renderText("XML file read successfully.")

        # Ref ID to AOPwiki ID
        keID <- tryCatch({
          data.frame(
            ref = xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"), "id"),
            ID = xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-reference"), "aop-wiki-id"),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing keID:", e$message) })
          stop("Error processing keID")
        })
        output$success_message <- renderText("keID processed successfully.")

        kerID <- tryCatch({
          data.frame(
            ref = xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"), "id"),
            ID = xml_attr(xml_find_all(xData, "/data/vendor-specific/key-event-relationship-reference"), "aop-wiki-id"),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing kerID:", e$message) })
          stop("Error processing kerID")
        })
        output$success_message <- renderText("kerID processed successfully.")

        aopID <- tryCatch({
          data.frame(
            ref = xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"), "id"),
            ID = xml_attr(xml_find_all(xData, "/data/vendor-specific/aop-reference"), "aop-wiki-id"),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing aopID:", e$message) })
          stop("Error processing aopID")
        })
        output$success_message <- renderText("aopID processed successfully.")

        # Key Event (KE) Data Processing
        incProgress(0.5, detail = "Extracting Key Events...")
        keData <- tryCatch({
          data.frame(
            ID = keID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event"), "id"), keID$ref)],
            title = xml_text(xml_find_all(xData, "/data/key-event/title")),
            LOBO = xml_text(xml_find_all(xData, "/data/key-event/biological-organization-level")),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing keData:", e$message) })
          stop("Error processing keData")
        })
        output$success_message <- renderText("Key Events extracted successfully.")

        # Key Event Relationship (KER) Data Processing
        incProgress(0.7, detail = "Extracting Key Event Relationships...")
        kerData <- tryCatch({
          data.frame(
            ID = kerID$ID[match(xml_attr(xml_find_all(xData, "/data/key-event-relationship"), "id"), kerID$ref)],
            KEup = keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/upstream-id")), keID$ref)],
            KEdown = keID$ID[match(xml_text(xml_find_all(xData, "/data/key-event-relationship/title/downstream-id")), keID$ref)],
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing kerData:", e$message) })
          stop("Error processing kerData")
        })
        output$success_message <- renderText("Key Event Relationships extracted successfully.")

        # AOP Data Processing
        oecdStatus <- tryCatch({
          sapply(xml_find_all(xData, "/data/aop/status"), FUN = function(x) {
            if ("oecd-status" %in% xml_name(xml_children(x))) {
              return(xml_text(xml_find_all(x, "oecd-status")))
            } else {
              return("not specified")
            }
          })
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing oecdStatus:", e$message) })
          stop("Error processing oecdStatus")
        })
        output$success_message <- renderText("OECD Status extracted successfully.")

        saaopStatus <- tryCatch({
          sapply(xml_find_all(xData, "/data/aop/status"), FUN = function(x) {
            if ("saaop-status" %in% xml_name(xml_children(x))) {
              return(xml_text(xml_find_all(x, "saaop-status")))
            } else {
              return("not specified")
            }
          })
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing saaopStatus:", e$message) })
          stop("Error processing saaopStatus")
        })
        output$success_message <- renderText("SAAOP Status extracted successfully.")

        ## MIEs: more than one MIE possible per aop, so must return list
        mies<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
          if("molecular-initiating-event"%in%xml_name(xml_children(x))){
            return(keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"),"key-event-id"),keID$ref)])
          }else{
            return(NULL)
          }
        })

        mies_full <- tryCatch({
          lapply(xml_find_all(xData, "/data/aop"), FUN = function(x) {
            if ("molecular-initiating-event" %in% xml_name(xml_children(x))) {
              mies_out <- keID$ID[match(xml_attr(xml_find_all(x, "molecular-initiating-event"), "key-event-id"), keID$ref)]
              return(list(ref = xml_attr(x, "id"),
                          ID = aopID[match(xml_attr(x, "id"), aopID$ref), "ID"],
                          df = mies_out))
            } else {
              return(NULL)
            }
          })
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing mies_full:", e$message) })
          stop("Error processing mies_full")
        })
        output$success_message <- renderText("Molecular Initiating Events processed successfully.")

        mies_full_noNULL <- Filter(Negate(is.null), mies_full)
        aopswithmies <- data.frame(ref = sapply(mies_full_noNULL, function(x) x$ref), ID = sapply(mies_full_noNULL, function(x) x$ID))

        ## AOs: more than one AO possible per aop, so must return list
        aos<-lapply(xml_find_all(xData, "/data/aop"),FUN=function(x){
          if("adverse-outcome"%in%xml_name(xml_children(x))){
            return(keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"),"key-event-id"),keID$ref)])
          }else{
            return(NULL)
          }
        })

        aos_full <- tryCatch({
          lapply(xml_find_all(xData, "/data/aop"), FUN = function(x) {
            if ("adverse-outcome" %in% xml_name(xml_children(x))) {
              aos_out <- keID$ID[match(xml_attr(xml_find_all(x, "adverse-outcome"), "key-event-id"), keID$ref)]
              return(list(ref = xml_attr(x, "id"),
                          ID = aopID[match(xml_attr(x, "id"), aopID$ref), "ID"],
                          df = aos_out))
            } else {
              return(NULL)
            }
          })
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error processing aos_full:", e$message) })
          stop("Error processing aos_full")
        })
        output$success_message <- renderText("Adverse Outcomes processed successfully.")

        aos_full_noNULL <- Filter(Negate(is.null), aos_full)
        aopswithaos <- data.frame(ref = sapply(aos_full_noNULL, function(x) x$ref), ID = sapply(aos_full_noNULL, function(x) x$ID))

        kes<-lapply(xml_find_all(xData,"/data/aop/key-events"),FUN=function(x){
          if("key-event"%in%xml_name(xml_children(x))){
            keidName<-unique(names(unlist(xml_attrs(xml_children(x)))))
            if(keidName=="id"){
              return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"id"),keID$ref)])}
            else if(keidName=="key-event-id"){return(keID$ID[match(xml_attr(xml_find_all(x, "key-event"),"key-event-id"),keID$ref)])}
            else (return(print("Please use an xml file with key-event attibutes named either 'id' or 'key-event-id'")))
          }else{
            return(NULL)
          }
        })


        kes_full <- lapply(xml_find_all(xData, "/data/aop/key-events"), FUN = function(x) {
          if ("key-event" %in% xml_name(xml_children(x))) {
            keidName <- unique(names(unlist(xml_attrs(xml_children(x)))))
            if (keidName == "id") {
              ke.out <- keID$ID[match(xml_attr(xml_find_all(x, "key-event"), "id"), keID$ref)]
            } else if (keidName == "key-event-id") {
              ke.out <- keID$ID[match(xml_attr(xml_find_all(x, "key-event"), "key-event-id"), keID$ref)]
            } else {
              print("Please use an xml file with key-event attributes named either 'id' or 'key-event-id'")
              return(NULL)
            }
            return(list(ref = xml_attr(xml_parent(x), "id"),
                        ID = aopID[match(xml_attr(xml_parent(x), "id"), aopID$ref), "ID"],
                        df = ke.out))
          } else {
            return(NULL)
          }
        })

        # Filter out NULL entries
        kes_full_noNULL <- Filter(Negate(is.null), kes_full)

        # Create a dataframe that contains the aopID and refs for
        aopswithkes <- data.frame(
          ref = sapply(kes_full_noNULL, function(x) x$ref),
          ID = sapply(kes_full_noNULL, function(x) x$ID)
        )

        # Debugging: print contents of aopswithkes
        output$success_message <- renderText("Key Events processed successfully.")


        ###
        ## KERs: more than one KER per aop, each with aop-specific "adjaceny", "quantitative understanding", and "WoE"
        ## So must return data frame of KERs
        kers_full<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
          if("relationship"%in%xml_name(xml_children(x))){
            df.out<-data.frame(
              ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
              adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
              quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
              woe=xml_text(xml_find_all(x, "relationship/evidence")),
              stringsAsFactors=FALSE
            )
            return(list(ref=xml_attr(xml_parent(x),"id"),
                        ID=aopID[match(xml_attr(xml_parent(x),"id"),aopID$ref),"ID"],df=df.out))
          }else{
            return(NULL)
          }
        })

        kers<-lapply(xml_find_all(xData, "/data/aop/key-event-relationships"),FUN=function(x){
          if("relationship"%in%xml_name(xml_children(x))){
            return(data.frame(
              ID=kerID$ID[match(xml_attr(xml_find_all(x, "relationship"),"id"),kerID$ref)],
              adjacency=xml_text(xml_find_all(x, "relationship/adjacency")),
              quant=xml_text(xml_find_all(x, "relationship/quantitative-understanding-value")),
              woe=xml_text(xml_find_all(x, "relationship/evidence")),
              stringsAsFactors=FALSE
            ))
          }else{
            return(NULL)
          }
        })

        ## add kes and MIE/AO designation (which is AOP-specific) for each KER in kers data.frame
        for(i in 1:length(kers)){
          if(length(kers[[i]])>0){
            KEup<-kerData$KEup[match(kers[[i]]$ID,kerData$ID)]
            KEDup<-sapply(KEup, FUN=function(x){
              if(x%in%mies[[i]]){
                return("MIE")
              }else{
                if(x%in%aos[[i]]){
                  return("AO")
                }else{
                  return("KE")
                }
              }
            })

            KEdown<-kerData$KEdown[match(kers[[i]]$ID,kerData$ID)]
            KEDdown<-sapply(KEdown, FUN=function(x){
              if(x%in%mies[[i]]){
                return("MIE")
              }else{
                if(x%in%aos[[i]]){
                  return("AO")
                }else{
                  return("KE")
                }
              }
            })

            kers[[i]]<-data.frame(
              ID=kers[[i]]$ID,
              KEup=KEup,
              KEDup=KEDup,
              KEdown=KEdown,
              KEDdown=KEDdown,
              adjacency=kers[[i]]$adjacency,
              quant=kers[[i]]$quant,
              woe=kers[[i]]$woe,
              row.names=NULL,
              stringsAsFactors = FALSE
            )
          }
        }

        kers_full_noNULL <- Filter(Negate(is.null), kers_full)

        aopswithkers<-data.frame(ref=sapply(kers_full_noNULL, function(x) x$ref),ID=sapply(kers_full_noNULL, function(x) x$ID))

        # Find Complete AOPs
        aopComplete <- tryCatch({
          aopID %>%
            inner_join(aopswithkers, by = c("ref", "ID")) %>%
            inner_join(aopswithkes, by = c("ref", "ID")) %>%
            inner_join(aopswithmies, by = c("ref", "ID")) %>%
            inner_join(aopswithaos, by = c("ref", "ID"))
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error finding complete AOPs:", e$message) })
          stop("Error finding complete AOPs")
        })
        output$success_message <- renderText("Complete AOPs identified successfully.")

        # Filter and Create aopDataComplete
        matched_ids <- aopID$ID[match(xml_attr(xml_find_all(xData, "/data/aop"), "id"), aopID$ref)]
        filtered_ids <- matched_ids[matched_ids %in% aopComplete$ID]

        # # Check lengths before creating the data frame
        # print(length(filtered_ids))
        # print(length(oecdStatus[matched_ids %in% aopComplete$ID]))
        # print(length(saaopStatus[matched_ids %in% aopComplete$ID]))
        # print(length(mies[matched_ids %in% aopComplete$ID]))
        # print(length(aos[matched_ids %in% aopComplete$ID]))
        # print(length(kes[matched_ids %in% aopComplete$ID]))
        # print(length(kers[matched_ids %in% aopComplete$ID]))

        aopDataComplete <- tryCatch({
          data.frame(
            ID = filtered_ids,
            oecdStatus = oecdStatus[matched_ids %in% aopComplete$ID],
            saaopStatus = saaopStatus[matched_ids %in% aopComplete$ID],
            mies = I(mies[matched_ids %in% aopComplete$ID]),
            aos = I(aos[matched_ids %in% aopComplete$ID]),
            kes = I(kes[matched_ids %in% aopComplete$ID]),
            kers = I(kers[            matched_ids %in% aopComplete$ID]),
            stringsAsFactors = FALSE
          )
        }, error = function(e) {
          output$error_message <- renderText({ paste("Error creating aopDataComplete:", e$message) })
          stop("Error creating aopDataComplete")
        })
        output$success_message <- renderText("aopDataComplete created successfully.")

        incProgress(1, detail = "Finalizing...")

        # Output summaries of the processed data to the UI
        output_main$ke_summary <- renderPrint({
          paste("Total Key Events:", nrow(keData))
        })

        output_main$ker_summary <- renderPrint({
          paste("Total Key Event Relationships:", nrow(kerData))
        })

        output_main$aop_summary <- renderPrint({
          paste("Total Complete AOPs:", nrow(aopDataComplete))
        })

        output$success_message <- renderText("XML processing completed successfully.")

      }, error = function(e) {
        # Log the error message
        output$error_message <- renderText({ paste("General error during XML processing:", e$message) })
      })
    })
  })
}
