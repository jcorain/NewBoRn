# test file for data.R
library(testthat)

col_empty_val <- list(Date = character(), Hour = character(), Weight = numeric(), Temperature = numeric(),  Mother_Milk = integer(), Powder_Milk = integer(),
                      Lactation_Left = logical(), Lactation_Right = logical (), Vomit = integer(), Urin = integer(), Poop = integer())

dummy_data_path <- file.path(system.file("extdata", package = "NewBoRn"),"dummy_data.csv")
dummy_data_wrong_path <- file.path(system.file("extdata", package = "NewBoRn"),"dummy_data_wrong.csv")

dummy_data <- utils::read.csv(dummy_data_path)
dummy_data <- dplyr::select(.data = dummy_data, -X)

context("Test create_new_data function")

test_that("Wrong inputs is given errors",
          {
            expect_error(create_new_data(firstname = 3))
            expect_error(create_new_data(firstname = TRUE))
            expect_error(create_new_data(surname = 3))
            expect_error(create_new_data(surname = TRUE))
            expect_error(create_new_data(previous_data = "test.jpg"))
            expect_error(create_new_data(previous_data = dummy_data_wrong_path))
          })

test_that("Defaut input give the expected values",
          {
            expect_equal(attr(create_new_data(),"firstname"), "John/Jane")
            expect_equal(attr(create_new_data(),"surname"), "Doh")
            expect_equal(colnames(create_new_data()),names(col_empty_val))
            expect_equal(nrow(create_new_data()), 0)
          })

test_that("Non defaut input give the expected values",
          {
            expect_equal(attr(create_new_data(firstname = "test"),"firstname"), "test")
            expect_equal(attr(create_new_data(surname = "test"),"surname"), "test")
            expect_equal(colnames(create_new_data(previous_data = dummy_data_path)), names(col_empty_val))
            expect_equal(nrow(create_new_data(previous_data = dummy_data_path)), 4)
          })

context("Test append_to_data function")

test_that("Wrong inputs are giving error",
          {
            expect_error(append_to_data())
            expect_error(append_to_data(dataframe = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "test" = 3))
          })

test_that("Good inputs are giving goout output",
          {
            expect_equal(nrow(append_to_data(dataframe = create_new_data(), "Weight" = 3700)), 1)
            expect_equal(sort(colnames(append_to_data(dataframe = create_new_data(), "Weight" = 3700))), sort(names(col_empty_val)))
            expect_equal(nrow(append_to_data(dataframe = dummy_data, "Weight" = 3700)), 5)
            expect_equal(sort(colnames(append_to_data(dataframe = dummy_data, "Weight" = 3700))), sort(names(col_empty_val)))
          })

test_that("Wrong format raises errors",
          {
            expect_error(append_to_data(dataframe = dummy_data, Date = 3, "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Date = NULL, "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Date = FALSE, "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Date = "test", "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Hour = 3, "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Hour = NULL, "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Hour = FALSE, "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, Hour = "test", "Weight" = 3700))
            expect_error(append_to_data(dataframe = dummy_data, "Weight" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Weight" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Temperature" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Temperature" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Mother_Milk" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Mother_Milk" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Powder_Milk" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Powder_Milk" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Lactation_Left" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Lactation_Left" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Lactation_Right" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Lactation_Right" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Vomit" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Vomit" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Poop" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Poop" = NULL))
            expect_error(append_to_data(dataframe = dummy_data, "Urin" = "test"))
            expect_error(append_to_data(dataframe = dummy_data, "Urin" = NULL))
           })

context("Test save_data function")

test_that("Wrong inputs are giving error",
          {
            expect_error(save_data())
            expect_error(save_data(dataframe = dummy_data, filename = 3))
            expect_error(save_data(dataframe = dummy_data, filename = TRUE))
            expect_error(save_data(dataframe = dummy_data, path = 3))
            expect_error(save_data(dataframe = dummy_data, path = TRUE))
          })

test_that("Good inputs are doing the expected outcome",
          {
            expect_equal(save_data(dataframe = dummy_data, filename = "test"), NULL)
            file.remove(file.path(system.file("extdata", package = "NewBoRn"),"test.csv"))
          })
