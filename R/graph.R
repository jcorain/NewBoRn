#' @name weight_graph
#' @title weight_graph
#'
#' @description function to plot the weight vs date
#'
#' @param dataframe The dataframe contaning the dat you want to use
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

weight_graph <- function(dataframe = NULL){

   #---------------Variable binding-----------------

  Date <- character(0)
  Weight <- numeric(0)

  #---------------Checks-----------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Weight))) stop("There is no weight in the data frame.")
  if(all(is.na(dataframe$Date))) stop("There is no date in the data frame.")

  #-----------change the data--------------

  df <- dplyr::select(.data = dataframe, c(Date, Weight))
  df <- tidyr::drop_na(data = df)
  df <- dplyr::mutate(.data = df, Date = lubridate::as_date(Date, format = "%d-%m-%Y"))

  #-------------plot the graph-------------

  gr <- plotly::plot_ly()
  gr <- plotly::add_trace(gr, x = df$Date, y = df$Weight, type = "scatter", mode = "lines+markers")
  gr <- plotly::layout(gr, xaxis = list(title = "Date"), yaxis = list(title = "Weight (g)"))
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

dejection_graph <- function(dataframe = NULL, granularity = "Hour", dejection_type = c("Urin", "Poop", "Vomit")){

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

  stopifnot(granularity %in% c("Hour","Day"))

  #-------------data shaping-------------------

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Urin, Poop, Vomit))
  row_num_to_del <- unlist(sapply(1:nrow(df), function(row_num) {
    if(is.na(df$Urin[row_num]) && is.na(df$Vomit[row_num]) && is.na(df$Poop[row_num])){
      row_num
    }
  }, simplify = TRUE))

  if(length(row_num_to_del) != 0) df <- df[-row_num_to_del,]

  if(granularity == "Hour"){
    df <- dplyr::mutate(.data = df, Time = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
    df <- dplyr::select(.data = df, -c(Date, Hour))
  } else if(granularity == "Day") {
    df <- dplyr::mutate(.data = df, Time = lubridate::as_date(paste0(Date), format = "%d-%m-%Y"))
    df <- dplyr::group_by(.data = df, Time)
    df <- dplyr::summarise(.data = df, Urin = sum(Urin, na.rm = TRUE), Poop = sum(Poop, na.rm = TRUE), Vomit = sum(Vomit, na.rm = TRUE))
  }

  df <- tidyr::replace_na(data = df, list(Urin = 0, Poop = 0, Vomit = 0))

  #--------------graph plotting-------------------

  gr <- plotly::plot_ly()

  if("Urin" %in% dejection_type){
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Urin, type = "scatter", mode = "lines+markers", name = "Urin")
  }

  if("Poop" %in% dejection_type){
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Poop, type = "scatter", mode = "lines+markers", name = "Poop")
  }

  if("Vomit" %in% dejection_type){
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Vomit, type = "scatter", mode = "lines+markers", name = "Vomit")
  }

  gr <- plotly::layout(gr, xaxis = list(title = "Time"), yaxis = list(title = "Dejection (unit)"))
  return(gr)

} #end of dejection_graph function

#' @name temperature_graph
#' @title temperature_graph
#'
#' @description Function to plot the temperature versus time
#'
#' @param dataframe The dataframe you want to analyze
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

temperature_graph <- function(dataframe = NULL){

  #-------------Variable binding--------------------

  Date <- character(0)
  Hour <- character(0)
  Temperature <- numeric(0)

  #----------------checks----------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Temperature))) stop("There is no temperature in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  #-----------------reshape the data-------------------

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Temperature))
  df <- tidyr::drop_na(data = df)
  df <- dplyr::mutate(.data = df, Time = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
  df <- dplyr::select(.data = df, -c(Date, Hour))

  #-------------plot the graph-----------------------

  gr <- plotly::plot_ly()
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Temperature, type = "scatter", mode = "lines+markers")
  gr <- plotly::layout(gr, xaxis = list(title = "Date"), yaxis = list(title = "Temperature (\u00B0C)"),
                       shapes = list(list(type = "line", x0 = 0, x1= 1, xref = "paper", y0 = 37.5, y1 = 37.5, line = list(color = "red", dash = "dash")),
                                     list(type = "line", x0 = 0, x1= 1, xref = "paper", y0 = 36.5, y1 = 36.5, line = list(color = "red", dash = "dash"))))

  return(gr)

} #end of temperature_graph

#' @name lactation_graph
#' @title lactation_graph
#'
#' @description Function to plot lactation vs time
#'
#' @param dataframe The dataframe containing the data you want to use
#' @param granularity The granularity you want to use. For the moment it is hour or day.
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

lactation_graph <- function(dataframe = NULL, granularity = "Hour"){

  #-------Variable bindings---------------

  Date <- character(0)
  Hour <- character(0)
  Lactation_Left <- logical(0)
  Lactation_Right <- logical(0)
  Time <- NULL

  #---------------checks-----------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Lactation_Right)) && all(is.na(dataframe$Lactation_Left))) stop("There is no lactation in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  stopifnot(granularity %in% c("Hour","Day"))

  #-----------------reshape the data-----------------------

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Lactation_Left, Lactation_Right))
  row_num_to_del <- unlist(sapply(1:nrow(df), function(row_num) {
    if(is.na(df$Lactation_Left[row_num]) && is.na(df$Lactation_Right[row_num])){
      row_num
    }
  }, simplify = TRUE))

  if(length(row_num_to_del) != 0) df <- df[-row_num_to_del,]
  df <- dplyr::mutate(.data = df, dplyr::across(tidyselect::starts_with("Lact"), as.numeric),
                      Lactation_Left = -Lactation_Left)
  df <- tidyr::replace_na(data = df, list(Lactation_Left = 0, Lactation_Right = 0))

  if(granularity == "Hour"){
    df <- dplyr::mutate(.data = df, Time = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
    df <- dplyr::select(.data = df, -c(Date, Hour))
  } else if(granularity == "Day") {
    df <- dplyr::mutate(.data = df, Time = lubridate::as_date(paste0(Date), format = "%d-%m-%Y"))
    df <- dplyr::group_by(.data = df, Time)
    df <- dplyr::summarise(.data = df, Lactation_Left = sum(Lactation_Left), Lactation_Right = sum(Lactation_Right))
  }

  #--------------graph plotting-------------------

  gr <- plotly::plot_ly()
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Lactation_Right, type = "bar", name = "Lactation Right")
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Lactation_Left, type = "bar", name = "Lactation Left")
  gr <- plotly::layout(gr, xaxis = list(title = "Time"), yaxis = list(title = "Lactation (unit)"), barmode = "relative")
  return(gr)

} # end of lactation_graph

#' @name milk_feeding_graph
#' @title milk_feeding_graph
#'
#' @description Graph to survey milk feeding versus time
#'
#' @param dataframe The dataframe containing the data you want to use
#' @param granularity The granularity you want to use. For the moment it is hour or day.
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

milk_feeding_graph <- function(dataframe = NULL, granularity = "Hour"){

  #-------------Variable binding------------------------

  Date <- character(0)
  Hour <- character(0)
  Mother_Milk <- numeric(0)
  Powder_Milk <- numeric(0)
  Time <- NULL

  #--------------Checks---------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(all(is.na(dataframe$Mother_Milk)) && all(is.na(dataframe$Powder_Milk))) stop("There is no given milk in the data frame.")
  if(all(is.na(dataframe$Date)) && all(is.na(dataframe$Hour))) stop("There is no date or time in the data frame.")

  stopifnot(granularity %in% c("Hour","Day"))

  #-----------------reshape the data-----------------------

  df <- dplyr::select(.data = dataframe, c(Date, Hour, Mother_Milk, Powder_Milk))
  row_num_to_del <- unlist(sapply(1:nrow(df), function(row_num) {
    if(is.na(df$Mother_Milk[row_num]) && is.na(df$Powder_Milk[row_num])){
      row_num
    }
  }, simplify = TRUE))

  if(length(row_num_to_del) != 0) df <- df[-row_num_to_del,]

  if(granularity == "Hour"){
    df <- dplyr::mutate(.data = df, Time = lubridate::as_datetime(paste0(Date," ", Hour), format = "%d-%m-%Y %H:%M"))
    df <- dplyr::select(.data = df, -c(Date, Hour))
  } else if(granularity == "Day") {
    df <- dplyr::mutate(.data = df, Time = lubridate::as_date(paste0(Date), format = "%d-%m-%Y"))
    df <- dplyr::group_by(.data = df, Time)
    df <- dplyr::summarise(.data = df, Mother_Milk = sum(Mother_Milk, na.rm = TRUE), Powder_Milk = sum(Powder_Milk, na.rm = TRUE))
  }

  df <- tidyr::replace_na(data = df, list(Mother_Milk = 0, Powder_Milk = 0))
  df <- dplyr::mutate(.data = df, Total = Mother_Milk + Powder_Milk)

  gr <- plotly::plot_ly()
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Mother_Milk, type = "scatter", mode = "lines+markers", name = "Mother milk")
  gr <- plotly::add_trace(gr, x = df$Time, y = df$Powder_Milk, type = "scatter", mode = "lines+markers", name = "Powder milk")
  if(granularity == "Day"){
    gr <- plotly::add_trace(gr, x = df$Time, y = df$Total, type = "scatter", mode = "lines+markers", name = "Total")
  }
  gr <- plotly::layout(gr, xaxis = list(title = "Time"), yaxis = list(title = "Milk given (ml)"))
  return(gr)

} # end of milk_feeding_graph
