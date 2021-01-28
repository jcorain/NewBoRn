newborn_ui <- shiny::fluidPage(
  shiny::titlePanel("NewBoRn"),
  shiny::fluidRow(

    # data selection panel


    # bug report button

    shiny::actionButton(
      inputId = "bug_report",
      label = "Report a bug/comment.",
      onclick = "window.open('https://github.com/jcorain/NewBoRn', '_blank_')"
    ),

    shiny::tabsetPanel(
      id = "Tab",
      selected = "Data",

      shiny::tabPanel(
        title = "Data",

        shiny::column(2,

                      # create new data

                      shiny::h4("Create new data"),

                      shiny::textInput(inputId = "firstname",
                                       label = "First Name"),

                      shiny::textInput(inputId = "surname",
                                       label = "Surname"),

                      shiny::actionButton(inputId = "new_data",
                                          label = "Create new file"),

                      # load existing data

                      shiny::h4("Load existing data"),

                      shiny::fileInput(inputId = "data_load",
                                       label = "",
                                       multiple = FALSE,
                                       accept = "csv"
                      ),

                      # Save the data

                      shiny::h4("Save the data"),

                      shiny::downloadButton(outputId = "save_data",
                                            label = "Save the data")

        ), # end of data loading column

        shiny::column(10,

                      shiny::h4("Already loaded data"),

                      DT::DTOutput(outputId = "data_full"),

                      shiny::h4("Add new row"),

                      shiny::h5("Date time selection"),

                      shiny::column(6,

                                    shiny::dateInput(inputId = "Date",
                                                     label = "Date",
                                                     format = "dd-mm-yyyy")
                      ), # end of datetime column

                      shiny::column(6,
                                    shiny::textInput(inputId = "Hour",
                                                     label = "Hour (format = HH:MM)",
                                                     value = substr(Sys.time(),12,16))
                      ), #end of hour column

                      shiny::h5("Physics charcateristics"),

                      shiny::column(6,

                                    shiny::numericInput(inputId = "Weight",
                                                        label = "Weight (g)",
                                                        min = 0,
                                                        value = 0)
                      ),

                      shiny::column(6,

                                    shiny::numericInput(inputId = "Temperature",
                                                        label = "Temperature (\u00B0C)",
                                                        min = 0,
                                                        value = 0)
                      ),

                      shiny::h5("Feeding"),

                      shiny::column(6,

                                    shiny::checkboxInput(inputId = "Lactation_Left",
                                                         label = "Lactation Left",
                                                         value = FALSE)
                      ),


                      shiny::column(6,
                                    shiny::checkboxInput(inputId = "Lactation_Right",
                                                         label = "Lactation Right",
                                                         value = FALSE)
                      ),

                      shiny::h6("or"),

                      shiny::column(6,

                                    shiny::numericInput(inputId = "Mother_Milk",
                                                        label = "Mother Milk (mL)",
                                                        min = 0,
                                                        value = 0)
                      ),

                      shiny::column(6,
                                    shiny::numericInput(inputId = "Powder_Milk",
                                                        label = "Powder Milk (mL)",
                                                        min = 0,
                                                        value = 0)
                      ),

                      shiny::h5("Dejection"),

                      shiny::column(4,

                                    shiny::numericInput(inputId = "Urin",
                                                        label = "Urin",
                                                        min = 0,
                                                        step = 1,
                                                        value = 0)
                      ),

                      shiny::column(4,
                                    shiny::numericInput(inputId = "Poop",
                                                        label = "Poop",
                                                        min = 0,
                                                        step = 1,
                                                        value = 0)
                      ),

                      shiny::column(4,
                                    shiny::numericInput(inputId = "Vomit",
                                                        label = "Vomit",
                                                        min = 0,
                                                        step = 1,
                                                        value = 0),

                      ),

                      shiny::actionButton(inputId = "Add_row",
                                          label = "Add row to data"),

                      shiny::h4("Delete rows"),

                      shiny::uiOutput(outputId = "row_to_del"),

                      shiny::actionButton(inputId = "del_row",
                                          label = "Delete the row")
        ) #end of add data column
      ), # end of data tab

      shiny::tabPanel(
        title = "Graph",

        shiny::column(2,

                      shiny::h3("Graph inputs"),

                      shiny::selectInput(inputId = "granularity",
                                         label = "Time granularity",
                                         choices = granularity_val[-length(granularity_val)], #granularity_val are defined in graph.R
                                         selected = "Day"),

                      shiny::checkboxInput(inputId = "mean_plot",
                                           label = "Plot mean values",
                                           value = TRUE),

                      shiny::h4("Graph selection"),

                      shiny::h5("Physical Parameters"),

                      shiny::column(6,

                                    shiny::checkboxInput(inputId = "temperature_bool",
                                                         label = "Temperature",
                                                         value = TRUE
                                    )
                      ),

                      shiny::column(6,

                                    shiny::checkboxInput(inputId = "weight_bool",
                                                         label = "Weight",
                                                         value = TRUE
                                    )
                      ),

                      shiny::h5("Feeding"),

                      shiny::column(6,
                                    shiny::checkboxInput(inputId = "lactation_bool",
                                                         label = "Lactation",
                                                         value = TRUE
                                    )
                      ),

                      shiny::column(6,
                                    shiny::checkboxInput(inputId = "milk_feeding_bool",
                                                         label = "Milk Feeding",
                                                         value = TRUE
                                    )
                      ),

                      shiny::h5("Dejection"),

                      shiny::column(4,

                                    shiny::checkboxInput(inputId = "urin_bool",
                                                         label = "Urin",
                                                         value = TRUE
                                    )
                      ),

                      shiny::column(4,
                                    shiny::checkboxInput(inputId = "poop_bool",
                                                         label = "Poop",
                                                         value = TRUE
                                    )
                      ),

                      shiny::column(4,
                                    shiny::checkboxInput(inputId = "vomit_bool",
                                                         label = "Vomit",
                                                         value = TRUE
                                    )
                      ) ,

                      shiny::verbatimTextOutput(outputId = "test")


        ), #end of graph inputs

        shiny::column(10,

                      shiny::column(
                        4,
                        shiny::h3("Physical parameters"),

                        shiny::uiOutput(outputId = "temperature_graph"),

                        shiny::uiOutput(outputId = "weight_graph")

                      ),

                      shiny::column(
                        4,
                        shiny::h3("Feeding"),

                        shiny::uiOutput(outputId = "lactation_graph"),

                        shiny::uiOutput(outputId = "milk_feeding_graph")

                      ),

                      shiny::column(
                        4,
                        shiny::h3("Dejection"),

                        shiny::uiOutput(outputId = "dejection_graph")
                      )
        )# end of graph display pannel
      ) #end of graph tab
    ) #end of tabpannel
  ) #end of fluidpage
)

