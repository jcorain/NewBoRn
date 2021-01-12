newborn_server <- function(input, output, session) {

  #--------------Varaible binding---------------

  X <- NULL

  #-----------------data handling-------------------

  # create the new data

  new_data <- shiny::eventReactive(input$new_data, {
    shiny::req(input$firstname)
    shiny::req(input$surname)
    create_new_data(firstname = input$firstname, surname = input$surname)
  })

  # define firstname and surname

  firstname <- shiny::reactive({
    if(is.null(input$firstname) || input$firstname == ""){
      if(!is.null(input$data_load$datapath)){
        strsplit(basename(input$data_load$name), "_")[[1]][1]
      } else{
        NULL
      }
    } else{
      input$firstname
    }
  })

  surname <- shiny::reactive({
    if(is.null(input$surname) || input$surname == ""){
      if(!is.null(input$data_load$datapath)){
        strsplit(basename(input$data_load$name), "_")[[1]][2]
      } else{
        NULL
      }
    } else{
      input$surname
    }
  })

  # get the good data

  data_first <- shiny::reactive({
    if(!is.null(input$data_load)){
      utils::read.csv(input$data_load$datapath, stringsAsFactors = FALSE)
    } else if(!is.null(new_data())){
      new_data()
    } else{
      NULL
    }
  })


  # save the data

  output$save_data <- shiny::downloadHandler(filename = function() {
    paste0(firstname(), "_", surname(),"_data.csv")
  },
  content = function(file){
    utils::write.csv(data$curval,file)
  },
  contentType = "csv")

  #-----------------data tabset---------------

  # get reactive data and update it while adding new row

  data <- shiny::reactiveValues(lstval = NULL, curval = NULL, fantom = NULL)

  shiny::observeEvent(list(input$Add_row, input$new_data, input$data_load),
                      {
                        data$lstval <- if(input$Add_row == 0){
                          if(is.null(data_first()$X)){
                            shiny::req(data_first())
                          } else{
                            dplyr::select(.data = shiny::req(data_first()), -X)
                          }
                        } else{
                          data$curval
                        }

                        data$curval <- if(input$Add_row == 0){
                          if(is.null(data_first()$X)){
                            shiny::req(data_first())
                          } else{
                            dplyr::select(.data = shiny::req(data_first()), -X)
                          }
                        } else{
                          append_to_data(dataframe = data$lstval,
                                         Date = as.character(format(input$Date, "%d-%m-%Y")),
                                         Hour = input$Hour,
                                         Weight = ifelse(input$Weight == 0, NA, input$Weight),
                                         Temperature = ifelse(input$Temperature == 0, NA, input$Temperature),
                                         Lactation_Right = ifelse(input$Lactation_Right, as.logical(1), NA),
                                         Lactation_Left = ifelse(input$Lactation_Left, as.logical(1), NA),
                                         Mother_Milk = ifelse(input$Mother_Milk == 0, NA, input$Mother_Milk),
                                         Powder_Milk = ifelse(input$Powder_Milk == 0, NA, input$Powder_Milk),
                                         Urin = ifelse(input$Urin == 0, NA, input$Urin),
                                         Vomit = ifelse(input$Vomit == 0, NA, input$Vomit),
                                         Poop = ifelse(input$Poop == 0, NA, input$Poop))
                        }
                      })

  # print the data

  output$data_full <- DT::renderDT(shiny::req(utils::tail(data$curval)))

  #deleting rows

  output$row_to_del <- shiny::renderUI(
    shiny::selectInput(inputId = "del_row_sel",
                       label = "Row numbers to delete",
                       choices = 1:nrow(shiny::req(data$curval)),
                       selected = nrow(shiny::req(data$curval)))
  )

  shiny::observeEvent(input$del_row,
                      {
                        data$fantom <- data$curval
                        data$curval <- data$fantom[-as.numeric(input$del_row_sel),]
                      })


  #---------------plotting-----------------

  output$temperature <- plotly::renderPlotly(temperature_graph(dataframe = shiny::req(data$curval)))

  output$weight <- plotly::renderPlotly(weight_graph(dataframe = shiny::req(data$curval)))

  output$lactation <- plotly::renderPlotly(lactation_graph(dataframe = shiny::req(data$curval),
                                                           granularity = shiny::req(input$granularity)))

  output$milk_feeding <- plotly::renderPlotly(milk_feeding_graph(dataframe = shiny::req(data$curval),
                                                                 granularity = shiny::req(input$granularity)))

  output$dejection <- plotly::renderPlotly(dejection_graph(dataframe = shiny::req(data$curval),
                                                           granularity = shiny::req(input$granularity)))

  #-------------- test------------------------

  output$test <- shiny::renderPrint(input$del_row_sel)
}

