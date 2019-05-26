# ============================================================================= #
# Unit testing for storage container                                            #
# Sean Browning                                                                 #
# ============================================================================= #
library(testthat)

# Remove existing test files if they exist
test_files <- c("tests/test.xlsx", "tests/test.sqlite3", "tests/test_2.xlsx")
unlink(test_files[file.exists(test_files)])


# NOTE: should be called from root dir for that to work
source("surv_store.R")
options("stringsAsFactors" = FALSE)

# === Basic Initialization =================================================== #
test_that("Initialization with invalid storage type", {
  # CSV is not used and should fail
  expect_error(
    surv_store("tests/test.csv", elements = c("a", "b", "c"), debug = FALSE)
  )
})

test_that("Initialization of Excel backend with no prior data", {
  # Should not throw error
  a <- surv_store("tests/test.xlsx", elements = c("a", "b", "c"), debug = FALSE)

  # Should indicate not appended
  expect_false(a$appended)

  # Should be writing data to the first line
  expect_equal(a$idx, 1L)
})

test_that("Initalization of SQLite backend with no prior data", {
  # Should not throw error
  a <- surv_store("tests/test.sqlite3", table = "test", elements = c("a", "b", "c"), debug = FALSE)

  # Should indicate not appended
  expect_false(a$appended)
  a$save()
})


unlink(test_files[file.exists(test_files)])

# === Handling prior data ==================================================== #
test_that("Initialization of Excel backend with prior data", {
  a <- surv_store("tests/test.xlsx", elements = c("a", "b", "c"), debug = FALSE)
  random_dat <- data.frame(a = 1, b = 2, c = 3)
  a$write(random_dat)
  a$save()

  b <- surv_store("tests/test.xlsx", elements = c("a", "b", "c"), debug = FALSE)

  # Should indicate data appended
  expect_true(b$appended)

  # Should be pointing to row 3
  expect_equal(b$idx, 3L)

  b$save()
})

test_that("Initalization of SQLite backend with prior data", {
  a <- surv_store("tests/test.sqlite3", table = "test", elements = c("a", "b", "c"), debug = FALSE)
  random_dat <- data.frame(a = 1, b = 2, c = 3)
  a$write(random_dat)
  a$save()

  b <- surv_store("tests/test.sqlite3", table = "test", elements = c("a", "b", "c"), debug = FALSE)

  # Should indicate data appended
  expect_true(b$appended)
  b$save()
})

# === Excel-specific ========================================================= #
test_that("Excel backend thread-locking", {
  a <- surv_store("tests/test_2.xlsx", elements = c("a", "b", "c"), debug = FALSE)
  a$write(data.frame(a = 1:2, b = c("a", "b"), c = c(TRUE, FALSE)))
  a$write(data.frame(a = 3, b = "c", c = FALSE))
  a$write(data.frame(a = 4, b = "d", c = TRUE))

  a$locked <- TRUE

  # Write action should return 1 exit status (failed)
  expect_equal(a$write(data.frame(a = 4, b = "d", c = TRUE)), 1)

  # Saving data should throw an error
  expect_error(a$save())

  # Should expect that the idx == 6L
  expect_equal(a$idx, 6L)

})

test_that("Initialization of Excel backend with incompatible data elements", {
  # Initialization should fail with no common data elements
  expect_error(
    surv_store("tests/test.xlsx", elements = c("e", "f", "g"), debug = FALSE)
  )
})

# === SQLite specific ======================================================== #
test_that("Initialization of SQLite backend with no table name", {
  # Initialization should fail without a table name
  expect_error(
    surv_store("tests/test.sqlite3", elements = c("a", "b", "c"), debug = FALSE)
  )
})

test_that("Initialization of SQLite backend with incompatible data elements", {
  # Initialization should fail with no common data elements
  expect_error(
    surv_store("tests/test.sqlite3", table =  "test", elements = c("e", "f", "g"), debug = FALSE)
  )
})

message("All tests passed.")
unlink(test_files[file.exists(test_files)])
