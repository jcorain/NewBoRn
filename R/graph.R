#' @name weight_graph
#' @title weight_graph
#'
#' @description function to plot the weight vs date
#'
#' @param dataframe The dataframe contaning the dat you want to use
#' @param birthdate The birthdate of the child. If NULL we assume that this is the first day of the dataframe. Defaut is NULL
#'
#' @return graphic plotly object
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' weight_graph(dataframe = dummy_data)
#'
#' @export

weight_graph <- function(dataframe = NULL, birthdate = NULL){

  #---------------Variable binding-----------------

  Date <- character(0)
  Weight <- numeric(0)

  #---------------Checks-----------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Weight))) stop("There is no weight in the data frame.")
  if(all(is.na(dataframe$Date))) stop("There is no date in the data frame.")

  #-----------change the data--------------

  # get the birthdate

  if(is.null(birthdate)) birthdate <- min(unique(as.Date(dataframe$Date, format = "%d-%m-%Y")))

  # We assume that the weight is only relevant for different date

  df <- dplyr::select(.data = dataframe, c(Date, Weight))
  df <- tidyr::drop_na(data = df)
  df <- dplyr::mutate(.data = df, Date = lubridate::as_date(Date, format = "%d-%m-%Y"))

  #-------------plot the graph-------------

  gr <- plotly::plot_ly()
  gr <- plotly::add_trace(gr,
                          x = df$Date,
                          y = df$Weight,
                          type = "scatter",
                          mode = "lines+markers")
  gr <- plotly::layout(gr,
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Weight (g)"))

  return(gr)

} #end of weight_graph function

#' @name dejection_graph
#' @title dejection_graph
#'
#' @description function to plot the number of dejection versus date
#'
#' @param dataframe The dataframe containing the data you want to use
#' @param granularity The granularity you want to use. For the moment it is hour or day.
#' @param dejection_type Dejection type. Choose between "Urin", "Poop" and "Vomit". Defaut is all of them.
#' @param mean_plot Boolean to plot mean with granularity one step higher than the asked one.
#' @param birthdate The birthdate of the child. If NULL we assume that this is the first day of the dataframe. Defaut is NULL
#'
#' @return graphic plotly object
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' dejection_graph(dataframe = dummy_data)
#' dejection_graph(dataframe = dummy_data, granularity = "Day", dejection_type = "Urin")
#'
#' @export

dejection_graph <- function(dataframe = NULL, granularity = "Hour", dejection_type = c("Urin", "Poop", "Vomit"), mean_plot = FALSE, birthdate = NULL){

  #--------------global variables binding---------------

  Date <- character(0)
  Hour <- character(0)
  Urin <- numeric(0)
  Poop <- numeric(0)
  Vomit <- numeric(0)
  Time <- NULL

  #-------------data checks-----------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Vomit)) && all(is.na(dataframe$Urin)) && all(is.na(dataframe$Poop))) stop("There is no dejection in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  if(is.null(dejection_type)) stop("No dejection type. Do not know how to proceed")
  for (dej in dejection_type){
    if(!dej %in% c("Poop","Vomit","Urin")) stop(paste0("The dejection type, ", dej, ", is not in the list Poop, Vomit and Urin"))
  }

  stopifnot(granularity %in% granularity_val[-length(granularity_val)])

  #-------------data shaping-------------------

  # get the birthdate

  if(is.null(birthdate)) birthdate <- min(unique(as.Date(dataframe$Date, format = "%d-%m-%Y")))

  # select the proper columns and rows

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Urin, Poop, Vomit))
  row_num_to_del <- unlist(sapply(1:nrow(df), function(row_num) {
    if(is.na(df$Urin[row_num]) && is.na(df$Vomit[row_num]) && is.na(df$Poop[row_num])){
      row_num
    }
  }, simplify = TRUE))

  if(length(row_num_to_del) != 0) df <- df[-row_num_to_del,]

  # aggregate the data according to the granularity

  if(granularity == "Hour"){
    df <- dplyr::mutate(.data = df, Date = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
    df <- dplyr::select(.data = df, -Hour)
  } else if(granularity == "Day") {
    df <- dplyr::mutate(.data = df, Date = lubridate::as_date(paste0(Date), format = "%d-%m-%Y"))
    df <- dplyr::group_by(.data = df, Date)
    df <- dplyr::summarise(.data = df, Urin = sum(Urin, na.rm = TRUE), Poop = sum(Poop, na.rm = TRUE), Vomit = sum(Vomit, na.rm = TRUE))
  }

  # clean the data

  df <- tidyr::replace_na(data = df, list(Urin = 0, Poop = 0, Vomit = 0))

  # calculate the mean if needed

  if(mean_plot){
    granularity_mean <- granularity_val[(which(granularity_val == granularity) + 1)]
    df_mean <- mean_df(dataframe = df, granularity_mean = granularity_mean, birthdate = birthdate)
  }

  #--------------graph plotting-------------------

  gr <- plotly::plot_ly()

  if("Urin" %in% dejection_type){
    if(mean_plot){
      gr <- plotly::add_trace(gr,
                              x = df$Date,
                              y = df$Urin,
                              type = "scatter",
                              mode = "markers",
                              name = "Urin",
                              marker = list(color = "blue",
                                            opacity = 0.5),
                              showlegend = FALSE)

      gr <- plotly::add_trace(gr,
                              x = df_mean$Date,
                              y = df_mean$Urin,
                              type = "scatter",
                              mode = "lines",
                              name = "Urin",
                              line = list(color = "blue",
                                          width = 5))

    } else{
      gr <- plotly::add_trace(gr,
                              x = df$Date,
                              y = df$Urin,
                              type = "scatter",
                              mode = "lines+markers",
                              name = "Urin",
                              marker = list(color = "blue"),
                              line = list(color = "blue"))
    }
  }

  if("Poop" %in% dejection_type){
    if(mean_plot){
      gr <- plotly::add_trace(gr,
                              x = df$Date,
                              y = df$Poop,
                              type = "scatter",
                              mode = "markers",
                              name = "Poop",
                              marker = list(color = "orange",
                                            opacity = 0.5),
                              showlegend = FALSE)

      gr <- plotly::add_trace(gr,
                              x = df_mean$Date,
                              y = df_mean$Poop,
                              type = "scatter",
                              mode = "lines",
                              name = "Poop",
                              line = list(color = "orange",
                                          width = 5))

    } else{

    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Poop,
                            type = "scatter",
                            mode = "lines+markers",
                            name = "Poop",
                            marker = list(color = "orange"),
                            line = list(color = "orange"))
    }
  }

  if("Vomit" %in% dejection_type){
    if(mean_plot){
      gr <- plotly::add_trace(gr,
                              x = df$Date,
                              y = df$Vomit,
                              type = "scatter",
                              mode = "markers",
                              name = "Vomit",
                              marker = list(color = "green",
                                            opacity = 0.5),
                              showlegend = FALSE)

      gr <- plotly::add_trace(gr,
                              x = df_mean$Date,
                              y = df_mean$Vomit,
                              type = "scatter",
                              mode = "lines",
                              name = "Vomit",
                              line = list(color = "green",
                                          width = 5))

    } else{

    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Vomit,
                            type = "scatter",
                            mode = "lines+markers",
                            name = "Vomit",
                            marker = list(color = "green"),
                            line = list(color = "green"))
  }
}
  gr <- plotly::layout(gr,
                       xaxis = list(title = "Time"),
                       yaxis = list(title = "Dejection (unit)"))
  return(gr)

} #end of dejection_graph function

#' @name temperature_graph
#' @title temperature_graph
#'
#' @description Function to plot the temperature versus time
#'
#' @param dataframe The dataframe you want to analyze
#' @param birthdate The birthdate of the child. If NULL we assume that this is the first day of the dataframe. Defaut is NULL
#'
#' @return plotly graphic object
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' temperature_graph(dataframe = dummy_data)
#'
#' @export

temperature_graph <- function(dataframe = NULL, birthdate = NULL){

  #-------------Variable binding--------------------

  Date <- character(0)
  Hour <- character(0)
  Temperature <- numeric(0)

  #----------------checks----------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Temperature))) stop("There is no temperature in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  #-----------------reshape the data-------------------

  # get the birthdate

  if(is.null(birthdate)) birthdate <- min(unique(as.Date(dataframe$Date, format = "%d-%m-%Y")))

  # We assume that looking at the temperature only make sense at a certain datetime so we avoid any aggregation

  # select the relevant data and change the datetime format

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Temperature))
  df <- tidyr::drop_na(data = df)
  df <- dplyr::mutate(.data = df, Date = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
  df <- dplyr::select(.data = df, -Hour)

  #-------------plot the graph-----------------------

  gr <- plotly::plot_ly()
  gr <- plotly::add_trace(gr,
                          x = df$Date,
                          y = df$Temperature,
                          type = "scatter",
                          mode = "lines+markers")
  gr <- plotly::layout(gr,
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Temperature (\u00B0C)"),
                       shapes = list(list(type = "line", #add line at 37,5 degC
                                          x0 = 0,
                                          x1= 1,
                                          xref = "paper",
                                          y0 = 37.5,
                                          y1 = 37.5,
                                          line = list(color = "red",
                                                      dash = "dash")),
                                     list(type = "line", # add line at 36.5 degC
                                          x0 = 0,
                                          x1= 1,
                                          xref = "paper",
                                          y0 = 36.5,
                                          y1 = 36.5,
                                          line = list(color = "red",
                                                      dash = "dash"))))

  return(gr)

} #end of temperature_graph

#' @name lactation_graph
#' @title lactation_graph
#'
#' @description Function to plot lactation vs time
#'
#' @param dataframe The dataframe containing the data you want to use
#' @param granularity The granularity you want to use. For the moment it is hour or day.
#' @param birthdate The birthdate of the child. If NULL we assume that this is the first day of the dataframe. Defaut is NULL
#'
#' @return graphic plotly object
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' lactation_graph(dataframe = dummy_data)
#'
#' @export

lactation_graph <- function(dataframe = NULL, granularity = "Hour", birthdate = NULL){

  #-------Variable bindings---------------

  Date <- character(0)
  Hour <- character(0)
  Lactation_Left <- logical(0)
  Lactation_Right <- logical(0)

  #---------------checks-----------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Lactation_Right)) && all(is.na(dataframe$Lactation_Left))) stop("There is no lactation in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  stopifnot(granularity %in% granularity_val[-length(granularity_val)])

  #-----------------reshape the data-----------------------

  # get the birthdate

  if(is.null(birthdate)) birthdate <- min(unique(as.Date(dataframe$Date, format = "%d-%m-%Y")))

  # select only lactation columns and relevant rows

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Lactation_Left, Lactation_Right))
  row_num_to_del <- unlist(sapply(1:nrow(df), function(row_num) {
    if(is.na(df$Lactation_Left[row_num]) && is.na(df$Lactation_Right[row_num])){
      row_num
    }
  }, simplify = TRUE))

  if(length(row_num_to_del) != 0) df <- df[-row_num_to_del,]

  # change the boolean to numeric and set lactation left to negative values

  df <- dplyr::mutate(.data = df, dplyr::across(tidyselect::starts_with("Lact"), as.numeric),
                      Lactation_Left = -Lactation_Left)
  df <- tidyr::replace_na(data = df, list(Lactation_Left = 0, Lactation_Right = 0))

  # aggregate the data according to the granularity

  if(granularity == "Hour"){
    df <- dplyr::mutate(.data = df, Date = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
    df <- dplyr::select(.data = df, -Hour)
  } else if(granularity == "Day") {
    df <- dplyr::mutate(.data = df, Date = lubridate::as_date(paste0(Date), format = "%d-%m-%Y"))
    df <- dplyr::group_by(.data = df, Date)
    df <- dplyr::summarise(.data = df, Lactation_Left = sum(Lactation_Left), Lactation_Right = sum(Lactation_Right))
  }

  #--------------graph plotting-------------------

  gr <- plotly::plot_ly()

  gr <- plotly::add_trace(gr,
                          x = df$Date,
                          y = df$Lactation_Right,
                          type = "bar",
                          name = "Lactation Right")

  gr <- plotly::add_trace(gr,
                          x = df$Date,
                          y = df$Lactation_Left,
                          type = "bar",
                          name = "Lactation Left")

  gr <- plotly::layout(gr,
                       xaxis = list(title = "Date"),
                       yaxis = list(title = "Lactation (unit)"),
                       barmode = "relative")

  return(gr)

} # end of lactation_graph

#' @name milk_feeding_graph
#' @title milk_feeding_graph
#'
#' @description Graph to survey milk feeding versus time
#'
#' @param dataframe The dataframe containing the data you want to use
#' @param granularity The granularity you want to use. For the moment it is hour or day.
#' @param mean_plot Boolean to plot mean with granularity one step higher than the asked one.
#' @param birthdate The birthdate of the child. If NULL we assume that this is the first day of the dataframe. Defaut is NULL
#'
#' @return graphic plotly object
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' milk_feeding_graph(dataframe = dummy_data)
#'
#' @export

milk_feeding_graph <- function(dataframe = NULL, granularity = "Hour", mean_plot = FALSE, birthdate = NULL){

  #-------------Variable binding------------------------

  Date <- character(0)
  Hour <- character(0)
  Mother_Milk <- numeric(0)
  Powder_Milk <- numeric(0)
  Time <- NULL
  granularity_mean <- character(0)

  #--------------Checks---------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Mother_Milk)) && all(is.na(dataframe$Powder_Milk))) stop("There is no given milk in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  stopifnot(granularity %in% granularity_val[-length(granularity_val)])

  #-----------------reshape the data-----------------------

  # get the birthdate

  if(is.null(birthdate)) birthdate <- min(unique(as.Date(dataframe$Date, format = "%d-%m-%Y")))

  # select the wanted columns and rows

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Mother_Milk, Powder_Milk))
  row_num_to_del <- unlist(sapply(1:nrow(df), function(row_num) {
    if(is.na(df$Mother_Milk[row_num]) && is.na(df$Powder_Milk[row_num])){
      row_num
    }
  }, simplify = TRUE))

  if(length(row_num_to_del) != 0) df <- df[-row_num_to_del,]

  # change the date and summarize the data according to granularity

  if(granularity == "Hour"){
    df <- dplyr::mutate(.data = df, Date = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
    df <- dplyr::select(.data = df, -c(Hour))
  } else if(granularity == "Day") {
    df <- dplyr::mutate(.data = df, Date = lubridate::as_date(paste0(Date), format = "%d-%m-%Y"))
    df <- dplyr::group_by(.data = df, Date)
    df <- dplyr::summarise(.data = df, Mother_Milk = sum(Mother_Milk, na.rm = TRUE), Powder_Milk = sum(Powder_Milk, na.rm = TRUE))
  }

  # clean the Na values and get the total milk value

  df <- tidyr::replace_na(data = df, list(Mother_Milk = 0, Powder_Milk = 0))
  df <- dplyr::mutate(.data = df, Total = Mother_Milk + Powder_Milk)

  # calculate the mean if needed

  if(mean_plot){
    granularity_mean <- granularity_val[(which(granularity_val == granularity) + 1)]
    df_mean <- mean_df(dataframe = df, granularity_mean = granularity_mean, birthdate = birthdate)
  }

  #--------------plotting-----------------

  gr <- plotly::plot_ly()
  if(mean_plot){
    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Mother_Milk,
                            type = "scatter",
                            mode = "markers",
                            name = "Mother milk",
                            marker = list(color = "blue",
                                          opacity = 0.5),
                            showlegend = FALSE)

    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Powder_Milk,
                            type = "scatter",
                            mode = "markers",
                            name = "Powder milk",
                            marker = list(color = "orange",
                                          opacity = 0.5),
                            showlegend = FALSE)

    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Total,
                            type = "scatter",
                            mode = "markers",
                            name = "Total",
                            marker = list(color = "green",
                                          opacity = 0.5),
                            showlegend = FALSE)

    gr <- plotly::add_trace(gr,
                            x = df_mean$Date,
                            y = df_mean$Mother_Milk,
                            type = "scatter",
                            mode = "lines",
                            name = "Mother milk",
                            line = list(color = "blue",
                                        width =  5))

    gr <- plotly::add_trace(gr,
                            x = df_mean$Date,
                            y = df_mean$Powder_Milk,
                            type = "scatter",
                            mode = "lines",
                            name = "Powder Milk",
                            line = list(color = "orange",
                                        width = 5))

    gr <- plotly::add_trace(gr,
                            x = df_mean$Date,
                            y = df_mean$Total,
                            type = "scatter",
                            mode = "lines",
                            name = "Total",
                            line = list(color = "green",
                                        width = 5))

  } else{
    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Mother_Milk,
                            type = "scatter",
                            mode = "lines+markers",
                            name = "Mother milk",
                            marker = list(color = "blue"),
                            line = list(color = "blue"))

    gr <- plotly::add_trace(gr,
                            x = df$Date,
                            y = df$Powder_Milk,
                            type = "scatter",
                            mode = "lines+markers",
                            name = "Powder milk",
                            marker = list(color = "orange"),
                            line = list(color = "orange"))

    gr <- plotly::add_trace(gr, x = df$Date,
                            y = df$Total,
                            type = "scatter",
                            mode = "lines+markers",
                            name = "Total",
                            marker = list(color = "green"),
                            line = list(color = "green"))
  }

  gr <- plotly::layout(gr, xaxis = list(title = "Date"), yaxis = list(title = "Milk given (ml)"))

  return(gr)

} # end of milk_feeding_graph

#' @name mean_df
#' @title mean_df
#'
#' @description function returning the mean value of a data frame
#'
#' @param dataframe The dataframe from which you want to calculate the mean. It must have a Time column and the numeric column from which you want to have the mean.
#' @param granularity_mean The granularity at which you want to calculate the mean. Must be Day or Week. Defaut is Day
#' @param birthdate The birthdate of the child. If NULL we assume that this is the first day of the dataframe. Defaut is NULL
#'
#' @return mean_df the new dataframe containing the mean values
#'
#' @export

mean_df <- function(dataframe = NULL, granularity_mean =  "Day", birthdate = NULL){
  #----------------Varaible binding-------------

  Date <- character(0)
  day_diff <- numeric(0)
  #--------------Checks---------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")
  if(is.null(dataframe$Date) || all(is.na(dataframe$Date))) stop("There is no Date in the dataframe. Do not know how to proceed.")

  if(!granularity_mean %in% granularity_val[-1]) stop("The granularity mean is not in the possible values.")

  #------------------Make the computation---------------------

  # define birthdate

  if(is.null(birthdate)) birthdate <- min(unique(as.Date(dataframe$Date, format = "%d-%m-%Y")))

  # define column with new granularity

  if(granularity_mean == "Day"){
    df <- dplyr::mutate(.data = dataframe, Date = as.Date(dataframe$Date, format = "%d-%m-%Y"))
  } else if(granularity_mean == "Week"){
    df <- dplyr::mutate(.data = dataframe, day_diff = as.numeric(as.Date(dataframe$Date, format = "%d-%m-%Y") - birthdate),
                        Date = (trunc(day_diff/7)*7 + 3 + birthdate))
    df <- dplyr::select(.data = df, -day_diff)
  }

  # group_by the new granularity and apply mean to all other columns

  df <- dplyr::group_by(.data = df, Date)
  df <- dplyr::summarise(.data = df, dplyr::across(setdiff(colnames(df), "Date"), ~mean(.x, na.rm = TRUE)))

  return(df)
}
#--------------granularity definition-------------------

granularity_val <- c("Hour","Day","Week")
