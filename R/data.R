#' @name create_new_data
#' @title create_new_data
#'
#' @param firstname First name of your kid.
#' Defaut is NULL leading to John/Jane
#' @param surname Surname of your kid.
#' Defaut is NULLL leading to Doh.
#' @param previous_data Prexisting data. Must be NULL or a csv leading to a dataframe
#' Defaut is NULL leading to an empty data frame
#'
#' @examples
#' create_new_data(firstname = "Test", surname = "Retest")
#'
#' @return dataframe A dataframe with names as attributes and containing several columns :
#'    - Weight (numeric)
#'    - Temperature (numeric)
#'    - Date (datetime)
#'    - Hour (datetime)
#'    - Mother milk (numeric)
#'    - Powder milk (numeric)
#'    - Lactation left (boolean)
#'    - Lactation right (boolean)
#'    - Vomit (integer)
#'    - Urin (integer)
#'    - Poop (integer)
#'
#' @export

create_new_data <- function(firstname = NULL,
                            surname =  NULL,
                            previous_data = NULL){
  #------------------Variable binding-----------------

  X <- character(0)

  #-------------check the data----------------

  if(is.null(firstname)) firstname <- "John/Jane"
  if(is.null(surname)) surname <- "Doh"

  if(!is.character(firstname)) stop("The first name is not a character. Do not know how to proceed.")
  if(!is.character(surname)) stop("The surname is not a character. Do not know how to proceed.")

  #--------------load the data-------------

  if(!is.null(previous_data)){
    if(length(grep(pattern = ".csv$", previous_data)) != 0){
      df <- utils::read.csv(previous_data, stringsAsFactors = FALSE)
      if(!is.null(df$X)){
        df <- dplyr::select(.data = df, -X)
      }
    } else {
      stop("The data you are trying to load is not a csv file. Do not know how to proceed.")
    }
    if(any(sort(names(df)) != sort(names(col_empty_val)))) stop("The column names of the preexisting data are not the expected one. Please review your data.")
  } else{
    df <- data.frame(col_empty_val, stringsAsFactors = FALSE)
  }

  attr(df, "firstname") <- firstname
  attr(df, "surname") <- surname
  return(df)
} #end of create_new_data


#' @name append_to_data
#' @title append_to_data
#'
#' @param dataframe The dataframe to append data to
#' @param Date The date at which the event happened. Format dd-mm-yyy
#' @param Hour The hour at which the event happened. Format hh:mm
#' @param ... the data to append. It has to be named and to be related to the different names
#'
#' @return new data frame with new rows
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' append_to_data(dataframe = dummy_data, Lactation_Left = TRUE, Temperature = 37.2)
#'
#' @export

append_to_data <- function(dataframe, Date = as.character(format(Sys.Date(),"%d-%m-%Y")), Hour = substr(Sys.time(),12,16), ...){

  #----------------checks------------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  args <- list(...)
  for(name in names(args)){
    if(!name %in% colnames) stop("The name of the data you are trying to append is not in the predifined colnames.")
  }

  # check Date regexpr

  if(length(grep("^[0-3][0-9]-[01][0-9]-[0-9][0-9][0-9][0-9]$",Date)) == 0) stop("The date is not in the format (dd-mm-yyyy)")

  # check Hour regexpr

  if(length(grep("^[0-2][0-9]:[0-6][0-9]$",Hour)) == 0) stop("The Hour is not in the format (HH:MM)")

  #------------------append new col----------------------

  new_col <- data.frame(list(Date = Date, Hour = Hour, args), stringsAsFactors = FALSE)
  new_df <- dplyr::bind_rows(dataframe, new_col)
  return(new_df)

}# end of append_to_data

#' @name save_data
#' @title save_data
#'
#' @description function to save the data
#'
#' @param dataframe The dataframe to be saved
#' @param filename The filename to save the file in
#' @param path The path to save the file in
#'
#' @return csv file
#'
#' @export
#'
#' @examples
#' dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),
#' "dummy_data.csv"))
#' dummy_data <- dplyr::select(.data = dummy_data, -X)
#' save_data(dataframe = dummy_data, filename = "test")


save_data <- function(dataframe = NULL, filename = NULL, path = NULL){

  #------------------checks-----------------

  if(is.null(dataframe)) stop("There is no dataframe. Do not know how to proceed.")

  if(is.null(filename)) filename <- paste0(attr(dataframe, "firstname"), "_", attr(dataframe, "surname"), "_data")

  stopifnot(is.character(filename))

  if(is.null(path)) path <-  file.path(system.file("extdata",package = "NewBoRn"))
  stopifnot(is.character(path))

  #-----------------Save the data-----------------------

  filename <- paste0(filename,".csv")
  utils::write.csv(dataframe, file.path(path, filename), row.names = FALSE)

  return(NULL)
} #end of save_data

# shared vectors for column names

col_empty_val <- list(Date = character(), Hour = character(), Weight = numeric(), Temperature = numeric(),  Mother_Milk = integer(), Powder_Milk = integer(),
                      Lactation_Left = logical(), Lactation_Right = logical (), Vomit = integer(), Urin = integer(), Poop = integer())

colnames <- names(col_empty_val)
