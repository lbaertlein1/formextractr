# app.R
#
# formExtractR — entry point.

library(shiny)

# Shiny's default upload cap is 5 MB — easily exceeded by a real
# multi-page scanned tally-sheet PDF (a facility scanning several
# completed forms into one file, per expand_submission_uploads() in
# utils_extraction.R). Raised, not removed: the whole file still has
# to land on the server before any of our code can even look at how
# many pages it has, let alone split or resize them — the per-page
# downscaling for the API (EXTRACTION_HOLISTIC_MAX_DIM) happens
# strictly AFTER upload, so it doesn't relax this limit, only what
# happens once a file has already arrived. 100 MB comfortably covers
# a many-page scan at real resolution while still bounding how much a
# single bad upload (wrong file, corrupted scan, etc.) can consume.
options(shiny.maxRequestSize = 100 * 1024^2)

# Load ANTHROPIC_API_KEY (and any other local config) from .env if
# present. Safe to skip in production if you set real environment
# variables another way (systemd EnvironmentFile=, Docker env, etc) —
# this just won't find a .env and silently does nothing.
if (file.exists(".env")) readRenviron(".env")

source("R/config.R")
source("R/utils_image.R")
source("R/utils_template_io.R")
source("R/utils_extraction.R")
source("R/mod_template_designer.R")
source("R/mod_extraction.R")

addResourcePath("template_images", TEMPLATE_IMAGE_DIR)

ui <- navbarPage(
  title = "formExtractR",
  tabPanel("Template Designer", mod_template_designer_ui("template_designer")),
  tabPanel("Extract Data", mod_extraction_ui("extraction"))
  # tabPanel("Review Queue", ...)       # Phase 3
)

server <- function(input, output, session) {
  mod_template_designer_server("template_designer")
  mod_extraction_server("extraction")
}

shinyApp(ui, server)