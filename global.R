# ==================================================================== #
# Global variable and library definitions                              #
# Sean Browning (oet5)                                                 #
# ==================================================================== #

library(R6)
library(openxlsx)
library(readxl)
library(shiny)
library(dplyr)
library(DT)
library(pool)
library(RSQLite)
library(shinyWidgets)
library(shinyalert)
library(shinyjs)
library(assertthat)
library(shinythemes)

source("surv_store.R")
source("util.R")

survey_title <- "My Title"

# Read in questions from the excel workbook
survey_raw <- readxl::read_xlsx(file.path("Data", "Qs.xlsx"))

# Generate the shiny ui inputs and text values
survey_raw$ui <- vector("list", length = dim(survey_raw)[1L])
survey_raw$ui <- generate_ui(survey_raw)

# Pull the element_ids for the user inputs
response_elem <- survey_raw$element_id[!grepl("title|section", survey_raw$input_type)]

# Calculate total number of pages
n_pages <- max(survey_raw$page_num)

# Initialize a data store on app start
db_store <- surv_store(
  path = file.path("Data", "out.xlsx"),
  elements = response_elem,
  debug = TRUE
)

# Save the data-store on session end
onStop(function(){
  db_store$save()
})

# Playing with pressing 'Enter' on the keyboard to advance to the next Page
# From: https://github.com/daattali/advanced-shiny/blob/master/proxy-click/app.R
jscode <- '
  $(function() {
    var $els = $("[data-proxy-click]");
    $.each(
      $els,
      function(idx, el) {
        var $el = $(el);
        var $proxy = $("#" + $el.data("proxyClick"));
        $el.keydown(function (e) {
          if (e.keyCode == 13) {
            $proxy.click();
          }
        });
      }
    );
  });
'
