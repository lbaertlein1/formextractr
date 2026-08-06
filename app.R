# app.R
#
# formExtractR — entry point.

library(shiny)

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
