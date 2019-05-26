# ===================================================================== #
# Survey UI                                                             #
# Sean Browning, Sara Khan                                              #
# ===================================================================== #

fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$script(HTML(jscode))),
  chooseSliderSkin(skin = "Modern", color = "#6c9fcb"),
  includeCSS("www/custom.css"),

  fluidRow(
    column(
      10,
      offset = 2,
      tags$head(
        tags$style(
          HTML('
           label, input, {
           font-family: "Calibri";
           background-color: #FFFFFF;
           font-size: 20px;}
           }')
        )
      ),
      useShinyalert(),
      useShinyjs(),

      # Main Window where each question will appear
      uiOutput("main_panel"),

      # Back button
      div(
        style = "display:inline-block",
        hidden(
          actionButton(
            "back_button",
            "Back",
            icon("arrow-circle-left"),
            style = "color: #32273f; background-color: #fff; border-color: #6c9fcb"
          )
        )
      ),

      # Next button
      div(
        style = "display:inline-block",
        uiOutput("next_button")
      )

    )
  )
)
