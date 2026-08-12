# mod_template_designer.R
#
# Shiny module: Template Designer
#
# Lets a user upload a blank (or, if unavailable, filled-in) form image
# or PDF, draw bounding boxes over it to mark fields, tag each field
# with a name + data type (+ options, for circled-text fields), and
# save/load the result as a template. Fields can be selected (click the
# box) and resized via corner drag-handles.
#
# There used to be a second kind of box here — "reference points",
# small regions of printed text used at extraction time to align a
# submission photo to this template before cropping fields — removed
# along with the rest of the alignment pipeline (extraction now sends
# each submission's whole image in one API call and lets Claude locate
# every field itself by reading the form; see utils_extraction.R's
# module header). A field's x/y/w/h is still recorded here and still
# saved on the template, but only as a coarse "look roughly here first"
# hint at extraction time (describe_field_position() in
# utils_extraction.R) — not as a crop or alignment target, so it no
# longer needs to be pixel-precise the way it used to.
#
# One template = one form layout. (An earlier revision supported
# multiple layout "versions" per template; removed for complexity —
# create a separate template if a form's layout changes.)
#
# Public API:
#   mod_template_designer_ui(id)
#   mod_template_designer_server(id)
#
# All storage goes through utils_template_io.R — this module never
# touches the filesystem/JSON directly.

library(shiny)
library(DT)

mod_template_designer_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      tags$script(src = "js/bbox_canvas.js"),
      tags$link(rel = "stylesheet", href = "css/styles.css")
    ),
    titlePanel("Form Template Designer"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        h4("Template"),
        textInput(ns("template_name"), "Template name", placeholder = "e.g. RI Tally Sheet"),
        checkboxInput(ns("upload_is_filled"),
                      "This is a filled-in example, not a blank form (no blank was available)",
                      value = FALSE),
        fileInput(ns("blank_form_upload"), "Upload form image or PDF",
                  accept = c(".png", ".jpg", ".jpeg", ".tif", ".tiff", ".pdf")),
        actionButton(ns("save_template_btn"), "Save Template", class = "btn-primary", width = "100%"),
        tags$hr(),
        h4("Load Existing Template"),
        selectInput(ns("template_select"), NULL, choices = NULL),
        actionButton(ns("load_template_btn"), "Load", width = "100%"),
        actionButton(ns("delete_template_btn"), "Delete", width = "100%", class = "btn-danger"),
        tags$hr(),
        actionButton(ns("new_template_btn"), "Start New / Clear", width = "100%"),
        tags$hr(),
        helpText("Draw a box on the image to mark a field. Click an existing box ",
                 "to select it and drag a corner handle to resize. Click 'Edit' or ",
                 "'Delete' in the table below to modify an existing field.")
      ),
      mainPanel(
        width = 9,
        uiOutput(ns("image_canvas_ui")),
        tags$br(),
        h4("Fields"),
        DTOutput(ns("field_table"))
      )
    )
  )
}

mod_template_designer_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- reactive state ----------------------------------------------
    empty_fields_df <- function() {
      data.frame(
        field_id = character(), name = character(), type = character(),
        x = numeric(), y = numeric(), w = numeric(), h = numeric(),
        options = character(),
        stringsAsFactors = FALSE
      )
    }
    
    current_template_id <- reactiveVal(NULL)   # NULL = unsaved/new template
    image_file           <- reactiveVal(NULL)  # relative path under data/template_images/
    image_dims            <- reactiveVal(NULL) # list(width=, height=)
    is_filled_reference   <- reactiveVal(FALSE)
    fields_rv             <- reactiveVal(empty_fields_df())
    pending_box           <- reactiveVal(NULL) # box coords awaiting name/type via modal
    edit_field_id         <- reactiveVal(NULL) # non-NULL => field modal is editing this field
    templates_refresh     <- reactiveVal(0)    # bump to force template_select to refresh
    
    canvas_id      <- ns("bbox_canvas")
    img_id         <- ns("template_img")
    new_box_id     <- ns("new_box")
    resize_box_id  <- ns("resize_box")
    
    # ---- image/PDF upload ------------------------------------------------
    observeEvent(input$blank_form_upload, {
      req(input$blank_form_upload)
      f <- input$blank_form_upload
      
      ext <- tolower(fs::path_ext(f$name))
      source_path <- f$datapath
      source_name <- f$name
      
      if (ext == "pdf") {
        converted <- convert_pdf_page_to_image(f$datapath, page = 1)
        source_path <- converted$path
        source_name <- paste0(fs::path_ext_remove(f$name), ".png")
        if (converted$page_count > 1) {
          showNotification(
            sprintf(paste0("This PDF has %d pages — only page 1 was used as the form image. ",
                           "If the form is on a different page, split the PDF before uploading."),
                    converted$page_count),
            type = "warning", duration = 10
          )
        }
      }
      
      rel_path <- persist_template_image(source_path, source_name)
      dims <- get_image_dims(source_path)
      
      current_template_id(NULL)
      image_file(rel_path)
      image_dims(dims)
      is_filled_reference(isTRUE(input$upload_is_filled))
      fields_rv(empty_fields_df())
      updateCheckboxInput(session, "upload_is_filled", value = FALSE)
    })
    
    image_url <- reactive({
      req(image_file())
      file.path("template_images", basename(image_file()))
    })
    
    output$image_canvas_ui <- renderUI({
      req(image_file())
      tagList(
        if (isTRUE(is_filled_reference())) {
          tags$div(
            style = "background:#fff3cd;border:1px solid #ffe08a;padding:8px 12px;margin-bottom:8px;border-radius:4px;",
            tags$strong("Filled reference: "),
            "this was annotated from a completed form, not a blank one. ",
            "Draw boxes around where each field's response goes — try not to trace ",
            "the existing handwriting itself, since the box position (not its current ",
            "contents) is what gets reused against other submissions."
          )
        },
        tags$div(
          class = "fe-canvas-wrap",
          tags$img(
            id = img_id,
            src = image_url(),
            onload = sprintf("window.formExtractR.initCanvas('%s','%s','%s','%s')",
                             img_id, canvas_id, new_box_id, resize_box_id)
          ),
          tags$canvas(id = canvas_id)
        )
      )
    })
    
    # ---- push current field boxes to the JS canvas ------------------------
    push_boxes_to_canvas <- function() {
      fl <- fields_rv()
      
      field_boxes <- if (nrow(fl) == 0) list() else lapply(seq_len(nrow(fl)), function(i) {
        list(id = fl$field_id[i], x = fl$x[i], y = fl$y[i], w = fl$w[i], h = fl$h[i],
             label = fl$name[i], kind = "field")
      })
      
      session$sendCustomMessage("formExtractR_render_boxes",
                                list(canvasId = canvas_id, boxes = field_boxes))
    }
    
    observeEvent(input$bbox_canvas_ready, {
      push_boxes_to_canvas()
    }, ignoreInit = TRUE)
    
    # ---- resizing an existing field box ------------------------------------
    observeEvent(input$resize_box, {
      box <- input$resize_box
      
      fl <- fields_rv()
      idx_f <- which(fl$field_id == box$id)
      if (length(idx_f) == 1) {
        fl$x[idx_f] <- box$x; fl$y[idx_f] <- box$y; fl$w[idx_f] <- box$w; fl$h[idx_f] <- box$h
        fields_rv(fl)
      }
    })
    
    # ---- drawing a new box -> prompt for name/type/options -----------------
    observeEvent(input$new_box, {
      req(image_file())
      pending_box(input$new_box)
      edit_field_id(NULL)
      showModal(field_modal(mode = "add"))
    })
    
    field_modal <- function(mode = "add", field = NULL) {
      type_choices <- setNames(names(FIELD_TYPE_LABELS), FIELD_TYPE_LABELS)
      modalDialog(
        title = if (mode == "add") "New Field" else "Edit Field",
        textInput(ns("field_name_input"), "Field name",
                  value = if (!is.null(field)) field$name else ""),
        selectInput(ns("field_type_input"), "Data type",
                    choices = type_choices,
                    selected = if (!is.null(field)) field$type else type_choices[1]),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'circled_text'", ns("field_type_input")),
          textInput(ns("field_options_input"),
                    "Options as printed on the form (semicolon-separated)",
                    value = if (!is.null(field)) field$options else "",
                    placeholder = "e.g. Fixed; Outreach; Mobile"),
          helpText("List every option a respondent could circle, in the ",
                   "order they appear on the form.")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("field_modal_ok"), "Save Field", class = "btn-primary")
        )
      )
    }
    
    observeEvent(input$field_modal_ok, {
      nm <- trimws(input$field_name_input)
      if (nzchar(nm) == FALSE) {
        showNotification("Field name cannot be empty.", type = "error")
        return()
      }
      
      selected_type <- input$field_type_input
      opts <- ""
      if (selected_type %in% FIELD_TYPES_WITH_OPTIONS) {
        opts_vec <- trimws(strsplit(input$field_options_input, ";")[[1]])
        opts_vec <- opts_vec[nzchar(opts_vec)]
        if (length(opts_vec) < 2) {
          showNotification("Circled-text fields need at least two options (semicolon-separated).", type = "error")
          return()
        }
        opts <- paste(opts_vec, collapse = ";")
      }
      
      fl <- fields_rv()
      
      if (is.null(edit_field_id())) {
        box <- pending_box()
        req(box)
        new_row <- data.frame(
          field_id = uuid::UUIDgenerate(), name = nm, type = selected_type,
          x = box$x, y = box$y, w = box$w, h = box$h, options = opts,
          stringsAsFactors = FALSE
        )
        fields_rv(rbind(fl, new_row))
      } else {
        idx <- which(fl$field_id == edit_field_id())
        if (length(idx) == 1) {
          fl$name[idx] <- nm
          fl$type[idx] <- selected_type
          fl$options[idx] <- opts
          fields_rv(fl)
        }
      }
      
      pending_box(NULL)
      edit_field_id(NULL)
      removeModal()
      push_boxes_to_canvas()
    })
    
    # ---- field table (with inline edit/delete buttons) --------------------
    # Vertically scrollable, no pagination — the whole list stays in one
    # scroll area regardless of how many fields a form has.
    output$field_table <- renderDT({
      fl <- fields_rv()
      if (nrow(fl) == 0) {
        return(datatable(
          data.frame(Message = "No fields yet — draw a box on the image to add one."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE)
        ))
      }
      
      display <- data.frame(
        Name = fl$name,
        Type = FIELD_TYPE_LABELS[fl$type],
        Options = ifelse(fl$type %in% FIELD_TYPES_WITH_OPTIONS,
                         gsub(";", " / ", fl$options), ""),
        Actions = vapply(fl$field_id, function(fid) {
          sprintf(
            paste0(
              '<button class="btn btn-primary btn-sm fe-btn-sm" ',
              'onclick="Shiny.setInputValue(\'%s\', {id:\'%s\', nonce:Date.now()}, {priority:\'event\'})">Edit</button> ',
              '<button class="btn btn-danger btn-sm fe-btn-sm" ',
              'onclick="Shiny.setInputValue(\'%s\', {id:\'%s\', nonce:Date.now()}, {priority:\'event\'})">Delete</button>'
            ),
            ns("edit_field"), fid, ns("delete_field"), fid
          )
        }, character(1)),
        stringsAsFactors = FALSE
      )
      
      datatable(
        display, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE, scrollY = "420px", scrollCollapse = TRUE)
      )
    })
    
    observeEvent(input$edit_field, {
      fl <- fields_rv()
      row <- fl[fl$field_id == input$edit_field$id, ]
      req(nrow(row) == 1)
      edit_field_id(row$field_id)
      showModal(field_modal(mode = "edit",
                            field = list(name = row$name, type = row$type, options = row$options)))
    })
    
    observeEvent(input$delete_field, {
      fl <- fields_rv()
      fields_rv(fl[fl$field_id != input$delete_field$id, ])
      push_boxes_to_canvas()
    })
    
    # ---- save / load / delete template -------------------------------------
    refresh_template_choices <- function(selected = NULL) {
      tmpls <- list_templates()
      choices <- if (nrow(tmpls) == 0) {
        c("No saved templates" = "")
      } else {
        setNames(tmpls$template_id,
                 sprintf("%s (%d fields, updated %s)", tmpls$template_name,
                         tmpls$field_count, substr(tmpls$updated_at, 1, 10)))
      }
      updateSelectInput(session, "template_select", choices = choices, selected = selected)
    }
    observe({ templates_refresh(); refresh_template_choices() })
    
    observeEvent(input$save_template_btn, {
      req(image_file(), image_dims())
      if (!nzchar(trimws(input$template_name))) {
        showNotification("Please enter a template name before saving.", type = "error")
        return()
      }
      
      tid <- save_template(
        template_id          = current_template_id(),
        template_name        = trimws(input$template_name),
        image_file            = image_file(),
        image_width           = image_dims()$width,
        image_height          = image_dims()$height,
        is_filled_reference   = is_filled_reference(),
        fields                = fields_rv()
      )
      current_template_id(tid)
      templates_refresh(templates_refresh() + 1)
      showNotification(paste("Template saved:", input$template_name), type = "message")
    })
    
    observeEvent(input$load_template_btn, {
      req(input$template_select, nzchar(input$template_select))
      t <- load_template(input$template_select)
      
      current_template_id(t$template_id)
      updateTextInput(session, "template_name", value = t$template_name)
      image_file(t$image_file)
      image_dims(list(width = t$image_width, height = t$image_height))
      is_filled_reference(isTRUE(t$is_filled_reference))
      
      fl <- t$fields
      if (is.null(fl) || NROW(fl) == 0) {
        fields_rv(empty_fields_df())
      } else {
        fl_df <- as.data.frame(fl, stringsAsFactors = FALSE)
        if (!"options" %in% names(fl_df)) fl_df$options <- ""  # backward compat with pre-options templates
        fields_rv(fl_df)
      }
    })
    
    observeEvent(input$delete_template_btn, {
      req(input$template_select, nzchar(input$template_select))
      delete_template(input$template_select)
      templates_refresh(templates_refresh() + 1)
      showNotification("Template deleted.", type = "message")
    })
    
    observeEvent(input$new_template_btn, {
      current_template_id(NULL)
      image_file(NULL)
      image_dims(NULL)
      is_filled_reference(FALSE)
      fields_rv(empty_fields_df())
      updateTextInput(session, "template_name", value = "")
      updateCheckboxInput(session, "upload_is_filled", value = FALSE)
    })
    
  })
}