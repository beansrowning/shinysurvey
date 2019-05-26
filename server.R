# ===================================================================== #
# Shiny server definition                                               #
# Sean Browning, Sara Khan                                              #
# ===================================================================== #

shinyServer(function(input, output, session) {
  # Allow for client to reconnect to the server if the connection is lost
  session$allowReconnect(TRUE)

  # === Data management section ===============================================
  query <- parseQueryString(isolate(session$clientData$url_search))

  # Check if data_manager was passed as a query string, and if so serve that instead
  # NOTE: This is hacky and not at all protected. In the future, there should be
  # some kind of authentication here, and probably an entire re-work.
  if (!is.null(query$action) && query$action == "data_manager") {
    hideElement("next_button")

    # Render the UI shell to fill with the DT
    output$main_panel <- renderUI({
      list(
      tags$div(
        align = "center",
        tags$div(
          class = "alert alert-info",
          h3("Current Survey Data")
        )
      ),
      DT::dataTableOutput("survey_records"))
    })

    # Pull in data
    survey_data <- reactive({
      # We have to pull in our data differently depending on how it's being stored
      return(switch(class(db_store)[1],
        excel_survey_store = readxl::read_xlsx(db_store$out_file),
        SQLite_survey_store = dbGetQuery(
          db_store$con,
          sqlInterpolate(db_store$con, "SELECT * FROM ?table", table = db_store$table_name)
        ),
        warning("No data", immediate. = TRUE)
      ))
    }, label = "pull_survey_data")

    # Render datatable
    output$survey_records <- DT::renderDT({
      datatable(
        survey_data(),
        style = "bootstrap",
        class = c("compact", "cell-border stripe"),
        rownames = FALSE,
        width = "100%",
        extensions = "Buttons",
        options = list(
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            )
          )
        )
      )
    })
  } else {

  # === User Agent String testing ============================================
  # BUG:
  # Test user agent string to see if it is an edge browser
  # for some reason this won't work, so we don't serve the user if it returns TRUE
  isEdge <- reactive({
    val <- get0("HTTP_USER_AGENT", session$request, ifnotfound = "")

    return(grepl("edge", val, ignore.case = TRUE))
  })

  observeEvent(isEdge(), {
    # DEBUG
    # warning("Checking User agent string", immediate. = TRUE)

    if (isEdge()) {
      on.exit(hideElement("next_button"))

      output$main_panel <- renderUI({
        h2("Sorry! Please open this page in Internet Explorer, Mozilla Firefox, or Google Chrome. Microsot Edge does not work.")
      })
    }
  }, label = "check_UAS")

  # === Main app ============================================================
  # Initialize page counter
  page <- reactiveValues(count = 0L)

  # Results stores the temporary results for this session.
  # Upon completion of the survey, the results will be written to
  # the DB
  # NOTE: This stores the data in browser, so it should be isolated from
  # other sessions, which should prevent issues with concurrent connections.
  session$userData$results <- vector(mode = "list", length = length(response_elem))
  names(session$userData$results) <- response_elem

  # Create a progress bar
  progress <- Progress$new()
  progress$set(message = "Survey Progress", value = 0)


  # Populate the main_panel ui element from ui.R
  # with the current ui elements from dynamic_ui()
  output$main_panel <- renderUI({
    dynamic_ui()
  })


  # NOTE: The only reason we make this dynamic is so we can change the
  # label of the button on the last page from "next" to "submit"...
  # which may seem silly in hindsight
  output$next_button <- renderUI({
    actionButton("press",
      label = label(),
      icon("arrow-circle-right"),
      style = "color: #32273f; background-color: #fff; border-color: #6c9fcb"
    )
  })

  # Upon pressing the above button...
  observeEvent(input$press, {
    if (page$count) {
      # If we are not on the title page, save responses
      save_progress()
    }

    if (page$count == n_pages) {
      # If we are on the final page, hide the button and save the results

      # TODO : rework to check for db_store$write() success
      # if(!db_store$write()) then allow resubmission and
      # don't hide the button yet. Else, proceed as planned

      # Hide the buttons when the survey is over
      hideElement("next_button")
      hideElement("back_button")

      # Also save our results

      db_store$write(as.data.frame(session$userData$results))

      # Close progress bar
      progress$close()

      # Also pop up a shiny alert saying they have finished the survey
      return(shinyalert(
        title = "Thanks again!",
        text = "We appreciate your feedback.",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "success",
        showConfirmButton = FALSE,
        showCancelButton = FALSE,
        timer = 0,
        imageUrl = "",
        animation = TRUE
      ))
      # TODO: Add new splash screen?
    }

    # Keep track of the number of clicks
    isolate({
      # Increment the page count
      page$count <- page$count + 1
    })

    # Increment the progress bar
    progress$inc(1 / n_pages, detail = sprintf("%i / %i", isolate(page$count), n_pages))
  }, label = "next_button_internals")

  # Back button internals
  observeEvent(input$back_button, {
    # We need to change the page, but first we should save survey progress first
    save_progress()

    isolate({
      page$count <- page$count - 1
    })

    # Decrement the survey progress
    progress$inc(-(1 / n_pages), detail = sprintf("%i / %i", isolate(page$count), n_pages))

    if (!page$count) {
      # If we are now on the first page, hide the back button
      hideElement("back_button")
    }
  },
  label = "back_button_internals",
  ignoreInit = TRUE
  )

  # Save intermediate survey progress on click
  save_progress <- reactive({
    # For each active input field on the screen, save the current state
    for (id in active_input()) {
      # Coerce to character and collapse to a single element in the case of
      # checkboxes
      session$userData$results[[id]] <<- paste(input[[id]], collapse = ", ")
    }
  })


  # Update UI state
  # NOTE: If we input some data and then hit back or next, we will save the data
  # but when we return to the page, the inputs themselves will still appear
  # as if it wasn't saved. Thus, we need to both save the data and the UI state.
  update_state <- reactive({
    element_types <- survey_raw$input_type[survey_raw$element_id %in% active_input()]
    i <- 1

    # DEBUG
    # warning(
    #   sprintf("updating UI elements: %s", paste(element_types, collapse = ", ")),
    #   immediate. = TRUE
    # )

    for (elem in active_input()) {
      switch(
        element_types[i],

        # radio buttons
        radio = updateRadioButtons(
          session,
          inputId = elem,
          selected = session$userData$results[[elem]]
        ),

        # checkboxes
        check = updateCheckboxGroupInput(
          session,
          inputId = elem,
          selected = maybe_split(session$userData$results[[elem]])
        ),

        # textbox
        text = updateTextInput(
          session,
          inputId = elem,
          value = session$userData$results[[elem]]
        ),

        # likert
        likert = ,

        # Alternative likert
        likert_alt = updateSliderTextInput(
          session,
          inputId = elem,
          selected = session$userData$results[[elem]]
        ),

        # drop-down
        `drop-down` = ,

        # selectize
        selectize = updateSelectizeInput(
          session,
          inputId = elem,
          selected = maybe_split(session$userData$results[[elem]])
        ),

        # Else
        warning(
          sprintf(
            "%s Failed to update unknown type: %s",
            as.character(Sys.time()),
            element_types[i]
          )
        )
      )

      i <- i + 1L
    }
  })

  # Next button label reactive
  label <- reactive({
    if (!is.null(input$press)) {
      if (page$count == n_pages) {
        # If we are on the last question, we should
        # change the button text to "Submit"
        return("Submit")
      }
      # Else, the button should read "Next"
      return("Next")
    }
  })

  # Adjust UI presentation based on current page
  dynamic_ui <- eventReactive(page$count, {
    # Dynamically change the main ui as the survey progresses
    if (!page$count) {
      # CASE : Survey Start
      return(
        list(
          h2(survey_title)
        )
      )
    }

    if (page$count > 0 & page$count < n_pages + 1L) {
      # CASE : Taking the survey

      # Update UI state after load
      on.exit(update_state())

      showElement("back_button")

      return(
        active_ui()
      )
    }
  }, label = "ui_generation")

  # Input IDs of those elements currently visible on page
  active_input <- reactive({
    # This returns a character vector of only the element ids for input ui

    # NOTE: Some of the element_ids will be titles, section headers
    # so we will filter only those which are inputs in the return statement
    ids <- survey_raw$element_id[which(survey_raw$page_num == page$count)]

    return(ids[ids %in% response_elem])
  })

  # This subsets the list of all UI elements for the whole survey and returns
  # only those which are supposed to be rendered on this page (both input and otherwise)
  active_ui <- reactive({
    # TODO: We would have some kind of NSE occurring here to allow for
    # hiding questions on the page based on the response to others
    return(survey_raw$ui[which(survey_raw$page_num == page$count)])
  })

  }
})
