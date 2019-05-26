# ============================================================================ #
# Survey storage object                                                        #
# Sean Browning (oet5)                                                         #
# ============================================================================ #

library(R6)
library(assertthat)
library(RQLite)
library(pool)

# NOTE:
# An R6 wrapper to cache responses and safely save data to a persistent file.
# There are two subclasses:
# - An Excel-based DB storage (useful for local setups, quick analyses)
# - A SQLite-based DB storage (more robust)
# The object should be initialized upon the opening of the shiny app.
# In the case of the Excel-variant, records stored in memory will save to disk
# on app close which will occur if the webserver is inactive for some time (shiny-server),
# or if the app is closed manually (interactive or shiny-server).

# R function to spawn our storage in a "natural" way
#' @title Survey storage generator
#'
#' @description
#' Spawns a survey storage R6 Class object that handles writing survey data to
#' either an excel spreadsheet or SQLite database.
#'
#' @param path Path to the output excel or SQLite DB to write to
#' @param elements A character vector of data elements that will be used for the survey
#' @param debug Should all DB actions be logged?
#' @param ... additional options, currently only `table` is recognized

#' @section SQLite backend
#' For the SQLite backend to work, one must pass a full path to a `.sqlite3` file
#' in addition to specifying the table to write to in a `table` argument.
#' Because dispatch is based on file extension, in-memory SQLite DBs are not
#' currently allowed (though they probably will be in the future).
#'
#' @import R6
#' @import assertthat
#' @import pool
#' @import RSQLite
#' @importFrom tools file_ext
#' @export
surv_store <- function(path, elements, debug, ...) {
  assert_that(is.character(path), is.character(elements), is.logical(debug))
  # Dispatch based on file extension of the path

  out <- switch(
    tools::file_ext(path),
    xlsx = surv_store_excel$new(
      file = path, data_elements = elements, debug = debug
    ),
    sqlite3 = surv_store_sqlite$new(
      file = path, table = list(...)$table, data_elements = elements, debug = debug
    ),
    stop("Unknown storage type.\nPossible containers: '.xlsx' or '.sqlite3'")
  )

  # Return the data container
  return(out)
}

surv_store_gen <- R6Class("survey_store",
  # Virtual class
  active = list(
    uid = function() {
      # This returns the current time in seconds from the epoch, coerced to character
      # TODO : Use UUID?
      return(paste(as.numeric(Sys.time()), paste(sample(letters, 4, replace = TRUE), collapse = ""), sep = ""))
    }
  ),

  public = list(
    con = NULL, # An Excel workbook or SQLite connection
    out_file = character(0), # Character string containing path to file
    locked = FALSE, # Whether the workbook/db is currently locked for writing
    names = character(0), # Data element names
    debug = logical(0), # Should all DB actions be logged?
    appended = logical(0), # Was the database created for the first time?

    initialize = function(...) {
      self$debug <- list(...)$debug
      self$out_file <- normalizePath(list(...)$file, "/", FALSE)
      self$appended <- file.exists(self$out_file)
      self$names <- list(...)$data_elements
    },

    write = function(...) {
      # NOTE: virtual for writeAsync
      NULL
    },

    writeAsync = function(data) {
      # NOTE: This is a UNIX-like locale-specific method and will fail on windows
      # Because we want lazy evaluation, we will return just the child PID
      # and let another method check for status later on in the code.
      # TODO: Not really implemented, worth the effort?
      job <- mcparallel(self$write(data))

      if (self$debug) {
        writeLines(sprintf("PID %s: Asynchronous write action -- %s", job$pid, Sys.time()))
      }

      return(invisible(job$pid))
    },

    resolvePromise = function(pid) {
      # NOTE: This is a UNIX-like locale-specific method and will fail on windows
      # Pull the results of the forked process
      return(unlist(mccollect(pid, wait = TRUE)))
    }
  ),
  private = list(
    requestLock = function(timeout = 10L, ...) {
      cnt <- 0
      while (self$locked) {
        # CASE : DB currently being accessed by another thread,
        # withold access and either grant access when block is lifted
        # or fail if timeout is reached.
        Sys.sleep(0.05)
        cnt <- cnt + 0.05

        if (cnt > timeout) {
          # CASE: Break early due to lock request timeout
          if (self$debug) {
            writeLines(sprintf("%s: Access request timed out! -- %s", list(...)$trace, Sys.time()))
          }

          return(invisible(1))
        }
      }

      # CASE : DB inactive, grant access
      if (self$debug) {
        if (!cnt) {
          # CASE : The process waited for some amount of time
          writeLines(sprintf("%s: Waited %0.3f seconds -- %s", list(...)$trace, cnt, Sys.time()))
        }
        writeLines(sprintf("%s: DB locked -- %s", list(...)$trace, Sys.time()))
      }
      # TODO: lockBinding() and unlockBinding() to actually lock the object?
      self$locked <- TRUE
      return(invisible(0))
    },

    unLock = function(...) {
      if (!self$locked) {
        # CASE : DB was somehow not locked, throw warning and exit.
        if (self$debug) {
          writeLines(sprintf("%s: DB was not locked, request ignored -- %s", list(...)$trace, Sys.time()))
        }
        return(invisible(1))
      }

      # CASE : Everything nominal, lift the thread lock.
      if (self$debug) {
        writeLines(sprintf("%s: DB unlocked -- %s", list(...)$trace, Sys.time()))
      }

      self$locked <- FALSE
      return(invisible(0))
    }
  )
)

surv_store_excel <- R6Class("excel_survey_store",
  inherit = surv_store_gen,
  active = list(
    idx = function() {
      # Return the row number where the data should be written
      if (!is.null(suppressWarnings(dim(openxlsx::readWorkbook(self$con))))) {
        # NOTE: 1L to offset for the colnames, 1L to point to the next row
        return(dim(openxlsx::readWorkbook(self$con))[1L] + 2L)
      }
      return(1L)
    }
  ),
  public = list(
    initialize = function(file, data_elements, debug = TRUE) {
      # initialize virtual class before excel-specific things
      super$initialize(file = file, data_elements = data_elements, debug = debug)

      if (!self$appended) {
        # CASE: No existing file
        # make a new workbook
        self$con <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(self$con, "Sheet 1")

        if (self$debug) {
          writeLines(sprintf("Storage object initialized with no existing data -- %s", Sys.time()))
        }
      } else {
        # CASE: Existing file, load and append
        self$con <- openxlsx::loadWorkbook(self$out_file)
        # Test that we aren't trying to join conflicting data by
        # testing the data elements from the file against the ones that
        # were passed.
        assert_that(
          all(names(openxlsx::readWorkbook(self$con, 1)) == data_elements),
          msg = "Data elements provided do not match those in the existing 'xlsx' document"
        )

        if (self$debug) {
          writeLines(sprintf("Storage object initialized, existing data loaded -- %s", Sys.time()))
        }
      }
      # Fully initialized excel data store
    },

    write = function(data) {
      # Main write operation that requests access to the cache, writes data
      # and relinquishes access upon completion.

      # TODO: Add additional assertions to check if structure of data is identical
      # to format requested during init (which will also need to be implemented).

      obs_id <- self$uid
      initial_state <- self$locked

      on.exit({
        if (self$locked & !initial_state) {
          # Prevents the case where writing fails due to timeout and we
          # were never given access
          private$unLock(trace = obs_id)
        }
      })


      if (self$debug) {
        writeLines(sprintf("%s: Attempting DB write action -- %s", obs_id, Sys.time()))
      }

      if (private$requestLock(trace = obs_id)) {
        # CASE: Non-0 exit status indicates we don't have write access

        # NOTE: if-closures have to be evaluated, so this is a way to evaluate
        # and check error status without taking time to save anything

        # TODO : Signal server timeout to UI to prompt user to retry submission
        # This could be accomplished similar to a closure like this one.

        return(invisible(1))
      }

      # Write to the sheet
      openxlsx::writeData(self$con,
        sheet = 1,
        data,
        startCol = 1,
        startRow = self$idx,
        colNames = self$idx == 1L
      )

      return(invisible(0))
    },

    save = function() {
      # Function which tabulates observations in cache and writes out to XLSX file
      # would be called at the end of webserver operation.
      initial_state <- self$locked

      # Relinquishes access on exit
      on.exit({
        if (self$locked & !initial_state) {
          # Prevents the case where writing fails due to timeout and we
          # were never given access
          private$unLock(trace = "App close")
        }
      })

      if (self$debug) {
        writeLines(sprintf("App close: starting save to disk -- %s", Sys.time()))
      }

      if (private$requestLock(trace = "App close")) {
        # CASE : DB still being accessed (unlikely)
        stop(sprintf("App close: Data not at rest, cannot write to disk"))
      }
      # CASE: Data at rest, write to disk

      openxlsx::saveWorkbook(wb = self$con, file = self$out_file, overwrite = TRUE)

      if (self$debug) {
        writeLines(sprintf("App close: data written successfully! -- %s", Sys.time()))
      }

      return(invisible(0))
    }
  )
)

surv_store_sqlite <- R6Class("SQLite_survey_store",
  inherit = surv_store_gen,
  public = list(
    table_name = character(0), # Name of the SQLite table that stores the data

    initialize = function(file, table, data_elements, debug = TRUE) {
      # At least a bit of defensive programming
      assert_that(!is.null(table), msg = "Table name must be provided")
      self$table_name <- as.character(table)

      # Initialize virtual class before subclass
      super$initialize(file = file, data_elements = data_elements, debug = debug)

      # dbPool will either create the file or append onto an existing one without
      # us having to be explicit about it. We also don't have to worry about
      # loading data into memory
      # A pool will allow multiple concurrent connections to the DB at once, but
      # technically we are strictly synchronous for the time being

      self$con <- pool::dbPool(
        drv = RSQLite::SQLite(),
        synchronous = "off",
        dbname = self$out_file
      )

      if (self$appended) {
        # Assert that we are not writing incompatible data to the DB
        # NOTE:
        # This is not that robust, since we'd ideally be checking types as well
        # We are also only checking that the variables exist, not that The
        # structure matches exactly. I wasn't sure if it really mattered TBH.
        # There might be cases where you only want to update certain columns, so
        # I figured it would probably be fine as is.
        if (!all(self$names %in% DBI::dbListFields(self$con, self$table_name))) {
          pool::poolClose(self$con)
          stop("Data elements passed were not found in existing SQLite database")
        }

        if (self$debug) {
          writeLines(sprintf("SQLite storage object initialized, existing data loaded -- %s", Sys.time()))
        }
      } else {
        # We can actually wait to define our table until the first write action
        if (self$debug) {
          writeLines(sprintf("SQLite storage object initialized with no existing data -- %s", Sys.time()))
        }
      }
    },

    write = function(data) {
      # NOTE: There is no reason to re-use the "thread-locking" code for SQLite
      # since it handles concurrency on the backend already. We are also not
      # using a "cache" here, rather we are writing to the DB each time.

      # Grab unique ID for write action
      obs_id <- self$uid
      ret_code <- 0

      if (self$debug) {
        writeLines(sprintf("%s: Attempting DB write action -- %s", obs_id, Sys.time()))
      }

      # Write the data
      # If the table doesn't yet exist, it will be created
      # We can have the data append each time regardless.
      tryCatch(
        DBI::dbWriteTable(self$con, self$table_name, data, append = TRUE),
        error = function(e) {
          # CASE: Write fails for some reason
          if (self$debug) {
            writeLines(sprintf("%s: Write to DB failed -- %s:", obs_id, Sys.time()))
            writeLines(as.character(e))
          }
          # Set return code to 1, which we might test for
          ret_code <<- 1
          return(invisible(1))
        }
      )

      if (self$debug & !ret_code) {
        writeLines(sprintf("%s: DB write action successful -- %s", obs_id, Sys.time()))
      }

      return(invisible(ret_code))
    },

    save = function(...) {
      # NOTE: With SQlite, we are actually saving the data each time, so the only
      # thing we ought to do here is close the connection and return
      # If we were making promises and using threading, we could include some
      # logic here to check that all promises were fulfilled, else re-try writing
      # before we finally close.

      if (self$debug) {
        writeLines(sprintf("App close: Closing pool -- %s", Sys.time()))
      }

      pool::poolClose(self$con)
    }
  )
)
