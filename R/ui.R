newborn_ui <- shiny::fluidPage(
  shiny::titlePanel("NewBoRn"),
  shiny::fluidRow(

    # data selection panel

    shiny::column(2,

                  # bug report button

                  shiny::actionButton(
                    inputId = "bug_report",
                    label = "Report a bug/comment."
                  ),

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
                                        label = "Save the data"),

                  # print for test purpose

                  shiny::verbatimTextOutput(outputId = "test")

    ),

    shiny::column(10,

                  shiny::tabsetPanel(
                    id = "Tab",
                    selected = "Data",

                    shiny::tabPanel(
                      title = "Data",

                      shiny::h4("Already loaded data"),

                      DT::DTOutput(outputId = "data_full"),

                      shiny::h4("Add new row"),

                      shiny::h5("Date time selection"),

                      shiny::column(6,

                                    shiny::dateInput(inputId = "Date",
                                                     label = "Date",
                                                     format = "dd-mm-yyyy")
                      ),

                      shiny::column(6,
                                    shiny::textInput(inputId = "Hour",
                                                     label = "Hour (format = HH:MM)",
                                                     value = substr(Sys.time(),12,16))
                      ),

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
                    ),

                    shiny::tabPanel(
                      title = "Graph",

                      shiny::selectInput(inputId = "granularity",
                                         label = "Time granularity",
                                         choices = c("Hour","Day"),
                                         selected = "Day"),

                      shiny::column(
                        4,
                        shiny::h3("Physical parameters"),

                        plotly::plotlyOutput(outputId = "temperature"),

                        plotly::plotlyOutput(outputId = "weight")
                      ),

                      shiny::column(
                        4,
                        shiny::h3("Feeding"),

                        plotly::plotlyOutput(outputId = "lactation"),

                        plotly::plotlyOutput(outputId = "milk_feeding")
                      ),

                      shiny::column(
                        4,
                        shiny::h3("Dejection"),
                        plotly::plotlyOutput(outputId = "dejection")
                      )

                    )

                  )
    )

  )
)
