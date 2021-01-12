# test file for graph.R
library(testthat)

dummy_data <- utils::read.csv(file.path(system.file("extdata", package = "NewBoRn"),"dummy_data.csv"))
dummy_data <- dplyr::select(.data = dummy_data, -X)

context("Test weight_graph function")

test_that("Good inputs are giving the good outputs",
          {
            expect_equal(class(weight_graph(dataframe = dummy_data))[1], "plotly")
          })

test_that("Wrong inputs are raising errors",
          {
            expect_error(weight_graph())
            expect_error(weight_graph(dataframe = data.frame()))
            dummy_data_NA_date <- dplyr::mutate(.data = dummy_data, Date = NA)
            expect_error(weight_graph(dataframe = dummy_data_NA_date))
            dummy_data_NA_weight <- dplyr::mutate(.data = dummy_data, Weight = NA)
            expect_error(weight_graph(dataframe = dummy_data_NA_weight))
          })

context("Test temperature_graph function")

test_that("Good inputs are giving the good outputs",
          {
            expect_equal(class(temperature_graph(dataframe = dummy_data))[1], "plotly")
          })

test_that("Wrong inputs are raising errors",
          {
            expect_error(temperature_graph())
            expect_error(temperature_graph(dataframe = data.frame()))
            dummy_data_NA_date_hour <- dplyr::mutate(.data = dummy_data, Date = NA, Hour = NA)
            expect_error(temperature_graph(dataframe = dummy_data_NA_date_hour))
            dummy_data_NA_temperature <- dplyr::mutate(.data = dummy_data, Temperature = NA)
            expect_error(temperature_graph(dataframe = dummy_data_NA_temperature))
          })

context("Test lactation_graph function")

test_that("Good inputs are giving the good outputs",
          {
            expect_equal(class(lactation_graph(dataframe = dummy_data))[1], "plotly")
            expect_equal(class(lactation_graph(dataframe = dummy_data, granularity = "Day"))[1], "plotly")
          })

test_that("Wrong inputs are raising errors",
          {
            expect_error(lactation_graph())
            expect_error(lactation_graph(dataframe = data.frame()))
            expect_error(lactation_graph(dataframe = dummy_data, granularity = "test"))
            dummy_data_NA_date_hour <- dplyr::mutate(.data = dummy_data, Date = NA, Hour = NA)
            expect_error(lactation_graph(dataframe = dummy_data_NA_date_hour))
            dummy_data_NA_lactation <- dplyr::mutate(.data = dummy_data, Lactation_Right = NA, Lactation_Left = NA)
            expect_error(lactation_graph(dataframe = dummy_data_NA_lactation))
          })


context("Test milk_feeding_graph function")

test_that("Good inputs are giving the good outputs",
          {
            expect_equal(class(milk_feeding_graph(dataframe = dummy_data))[1], "plotly")
            expect_equal(class(milk_feeding_graph(dataframe = dummy_data, granularity = "Day"))[1], "plotly")
          })

test_that("Wrong inputs are raising errors",
          {
            expect_error(milk_feeding_graph())
            expect_error(milk_feeding_graph(dataframe = data.frame()))
            expect_error(milk_feeding_graph(dataframe = dummy_data, granularity = "test"))
            dummy_data_NA_date_hour <- dplyr::mutate(.data = dummy_data, Date = NA, Hour = NA)
            expect_error(milk_feeding_graph(dataframe = dummy_data_NA_date_hour))
            dummy_data_NA_milk_feeding <- dplyr::mutate(.data = dummy_data, Mother_Milk = NA, Powder_Milk = NA)
            expect_error(milk_feeding_graph(dataframe = dummy_data_NA_milk_feeding))
          })

context("Test dejection_graph function")

test_that("Good inputs are giving the good outputs",
          {
            expect_equal(class(dejection_graph(dataframe = dummy_data))[1], "plotly")
            expect_equal(class(dejection_graph(dataframe = dummy_data, granularity = "Day"))[1], "plotly")
          })

test_that("Wrong inputs are raising errors",
          {
            expect_error(dejection_graph())
            expect_error(dejection_graph(dataframe = data.frame()))
            expect_error(dejection_graph(dataframe = dummy_data, granularity = "test"))
            dummy_data_NA_date_hour <- dplyr::mutate(.data = dummy_data, Date = NA, Hour = NA)
            expect_error(dejection_graph(dataframe = dummy_data_NA_date_hour))
            dummy_data_NA_dejection <- dplyr::mutate(.data = dummy_data, Vomit = NA, Urin = NA, Poop = NA)
            expect_error(dejection_graph(dataframe = dummy_data_NA_dejection))
          })
