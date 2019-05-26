# A helper function which takes in a raw survey and
# returns a new `ui` column with the corresponding UI elements
# ready for rendering by shiny

generate_ui <- function(data) {
  # Test that the data is in the correct format
  # TODO: This is hacky and should be flexible
  stopifnot(
    all(
      c("element_id", "choices", "label", "input_type", "page_num") %in% names(data)
    )
  )

  raw_choices <- strsplit(data$choices, ";")

  ui <- vector("list", length = dim(data)[1L])

  for (i in seq_along(data$ui)) {

    ## recode the likert type into either likert or as a likert scale with alternative (likert_alt)
    # option choices
    data$input_type[i] <- ifelse(
      data$input_type[i] == "likert" & length(raw_choices[[i]]) > 0,
      "likert_alt",
      data$input_type[i]
    )

    ui[[i]] <- switch(data$input_type[i],

      # Radio buttons
      radio = radioButtons(
        data$element_id[i],
        data$label[i],
        raw_choices[[i]],
        #  inline = TRUE,
        selected = character(0)
      ),

      # Checkboxes
      check = checkboxGroupInput(
        data$element_id[i],
        paste0(data$label[i], "(Mark all that apply:)"),
        raw_choices[[i]]
      ),

      # Selectize
      # Drop-down with multi-select and creation
      selectize = selectizeInput(
        data$element_id[i],
        label = data$label[i],
        choices = raw_choices[[i]],
        options = list(
          create = TRUE,
          placeholder = "(Select multiple and/or write in answers)"
        ),
        multiple = TRUE
      ),

      # Textbox
      # A true textbox
      text = textInput(
        data$element_id[i],
        label = data$label[i]
      ),

      # Drop-down (i.e. No new input)
      `drop-down` = selectizeInput(
        data$element_id[i],
        label = data$label[i],
        choices = raw_choices[[i]],
        multiple = TRUE,
        options = list(
          placeholder = "(Select one)",
          maxItems = 1
        )
      ),

      # 1-5 Likert scale
      likert = sliderTextInput(
        inputId = data$element_id[i],
        label = data$label[i],
        grid = TRUE,
        force_edges = TRUE,
        choices = c(
          "Strongly disagree",
          "Disagree",
          "Neither agree nor disagree",
          "Agree",
          "Strongly agree"
        ),
        hide_min_max = TRUE,
        selected = "Neither agree nor disagree"
      ),

      # 1-5 Likert scale - alternative choices
      likert_alt = sliderTextInput(
        inputId = data$element_id[i],
        label = data$label[i],
        grid = F,
        force_edges = TRUE,
        hide_min_max = TRUE,
        choices = raw_choices[[i]],
        selected = NULL # raw_choices[[i]][3] # pick the middle most value
      ),


      # Page title
      # NOTE: Not really an input, but we can overload
      # this function to return output as well
      title = tags$div(
        align = "center",
        tags$div(
          class = "alert alert-info",
          h3(data$label[i])
        )
      ),

      # Section header
      section = h3(data$label[i]),

      # Else, it's an undefined input we can't render
      stop(sprintf("UI not implemented: %s", data$input_type[i]))
    )
  }

  return(ui)
}

# Helper function to return NULL if value is NULL, else perform a strsplit
# NOTE: This is used when updating the UI state before changes have been made
# i.e. the stored value is NULL, which will halt strsplit
maybe_split <- function(string, split = ", ") {
  if (is.null(string)) {
    return(NULL)
  }

  return(unlist(strsplit(string, split)))
}
