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
# REFERENCE POINTS: alongside fields, the user can mark 2-4 reference
# points — small regions containing printed text, used at extraction
# time to align each submission to this template, rather than assuming
# submissions line up with the template edge-to-edge. Drawing one asks
# for a name AND the exact text at that location (anchor_text) — auto-
# read via the API when the box is drawn (ocr_patch()), always
# editable. A "Drawing mode" toggle decides whether a newly-drawn box
# becomes a field or a reference point; reference points render in
# green on the canvas (fields stay orange) and get their own small
# table. See utils_extraction.R for how anchor_text actually gets used
# at extraction time (locate_text_via_api(), align_submission()).
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
        h4("Drawing Mode"),
        radioButtons(ns("draw_mode"), NULL,
                     choices = c("Field (extracted data)" = "field",
                                 "Reference Point (alignment anchor)" = "reference"),
                     selected = "field"),
        helpText("Reference points are small regions containing PRINTED TEXT — a ",
                 "label near a grid corner, a distinctive phrase, etc. — used to locate ",
                 "and align each uploaded submission to this template before fields are ",
                 "cropped; not extracted as data themselves. You'll type (or accept an ",
                 "auto-read of) the exact text at each one. Mark 2-4 for best results: 2 ",
                 "gives position/scale/rotation correction, 4 (e.g. near the four corners ",
                 "of a grid) enables full perspective correction for photos taken at an angle."),
        tags$hr(),
        helpText("Draw a box on the image to mark a field or reference point ",
                 "(per the mode above). Click an existing box to select it and drag a ",
                 "corner handle to resize. Click 'Edit' or 'Delete' in either table below ",
                 "to modify existing items.")
      ),
      mainPanel(
        width = 9,
        uiOutput(ns("image_canvas_ui")),
        tags$br(),
        h4("Reference Points"),
        DTOutput(ns("reference_point_table")),
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
    empty_ref_points_df <- function() {
      data.frame(
        ref_id = character(), name = character(),
        x = numeric(), y = numeric(), w = numeric(), h = numeric(),
        anchor_text = character(),
        stringsAsFactors = FALSE
      )
    }

    current_template_id <- reactiveVal(NULL)   # NULL = unsaved/new template
    image_file           <- reactiveVal(NULL)  # relative path under data/template_images/
    image_dims            <- reactiveVal(NULL) # list(width=, height=)
    is_filled_reference   <- reactiveVal(FALSE)
    fields_rv             <- reactiveVal(empty_fields_df())
    reference_points_rv   <- reactiveVal(empty_ref_points_df())
    pending_box           <- reactiveVal(NULL) # box coords awaiting name/type via modal
    edit_field_id         <- reactiveVal(NULL) # non-NULL => field modal is editing this field
    edit_ref_id            <- reactiveVal(NULL) # non-NULL => reference point modal is editing this point
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
      reference_points_rv(empty_ref_points_df())
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

    # ---- push current field boxes + reference points to the JS canvas ----
    push_boxes_to_canvas <- function() {
      fl <- fields_rv()
      rp <- reference_points_rv()

      field_boxes <- if (nrow(fl) == 0) list() else lapply(seq_len(nrow(fl)), function(i) {
        list(id = fl$field_id[i], x = fl$x[i], y = fl$y[i], w = fl$w[i], h = fl$h[i],
             label = fl$name[i], kind = "field")
      })
      ref_boxes <- if (nrow(rp) == 0) list() else lapply(seq_len(nrow(rp)), function(i) {
        list(id = rp$ref_id[i], x = rp$x[i], y = rp$y[i], w = rp$w[i], h = rp$h[i],
             label = rp$name[i], kind = "reference")
      })

      session$sendCustomMessage("formExtractR_render_boxes",
                                 list(canvasId = canvas_id, boxes = c(field_boxes, ref_boxes)))
    }

    observeEvent(input$bbox_canvas_ready, {
      push_boxes_to_canvas()
    }, ignoreInit = TRUE)

    # ---- resizing an existing box (field OR reference point) --------------
    observeEvent(input$resize_box, {
      box <- input$resize_box

      fl <- fields_rv()
      idx_f <- which(fl$field_id == box$id)
      if (length(idx_f) == 1) {
        fl$x[idx_f] <- box$x; fl$y[idx_f] <- box$y; fl$w[idx_f] <- box$w; fl$h[idx_f] <- box$h
        fields_rv(fl)
        return()
      }

      rp <- reference_points_rv()
      idx_r <- which(rp$ref_id == box$id)
      if (length(idx_r) == 1) {
        rp$x[idx_r] <- box$x; rp$y[idx_r] <- box$y; rp$w[idx_r] <- box$w; rp$h[idx_r] <- box$h
        reference_points_rv(rp)
      }
    })

    # ---- drawing a new box -> prompt for name (+type/options if a field) --
    observeEvent(input$new_box, {
      req(image_file())
      pending_box(input$new_box)
      if (identical(input$draw_mode, "reference")) {
        edit_ref_id(NULL)

        # Best-effort auto-read of the patch's text, to save typing it
        # out by hand — always editable in the modal either way. Silent
        # failure (no key, network issue) just leaves it blank.
        auto_text <- tryCatch({
          api_key <- Sys.getenv("ANTHROPIC_API_KEY")
          if (!nzchar(api_key)) {
            NA_character_
          } else {
            box <- input$new_box
            dims <- image_dims()
            img <- magick::image_read(image_file())
            patch <- magick::image_crop(img, sprintf(
              "%dx%d+%d+%d",
              round(box$w * dims$width), round(box$h * dims$height),
              round(box$x * dims$width), round(box$y * dims$height)
            ))
            ocr_patch(api_key, patch)
          }
        }, error = function(e) NA_character_)

        showModal(reference_point_modal(mode = "add", prefill_text = auto_text))
      } else {
        edit_field_id(NULL)
        showModal(field_modal(mode = "add"))
      }
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

    # ---- reference point modal (name + the exact text to search for) ------
    reference_point_modal <- function(mode = "add", ref = NULL, prefill_text = NULL) {
      # prefill_text (from auto-OCR, "add" mode only) wins over any
      # existing ref$anchor_text (there won't be one yet in "add" mode
      # anyway) — NA means auto-read failed/no API key, leave it blank
      # for the user to type.
      text_value <- if (!is.null(prefill_text) && !is.na(prefill_text)) {
        prefill_text
      } else if (!is.null(ref)) {
        ref$anchor_text
      } else {
        ""
      }

      modalDialog(
        title = if (mode == "add") "New Reference Point" else "Edit Reference Point",
        helpText("Used to align each uploaded submission to this template. Not ",
                 "extracted as data."),
        textInput(ns("ref_name_input"), "Reference point name",
                  value = if (!is.null(ref)) ref$name else "",
                  placeholder = "e.g. Top-left grid corner"),
        textInput(ns("ref_anchor_text_input"), "Exact text at this location",
                  value = text_value, width = "100%",
                  placeholder = "e.g. Facility Name:"),
        helpText(
          if (!is.null(prefill_text) && is.na(prefill_text)) {
            "Auto-read didn't run (no ANTHROPIC_API_KEY set, or a network issue) — type the exact text yourself."
          } else if (!is.null(prefill_text) && !nzchar(prefill_text)) {
            "Auto-read found no legible text in this box — pick a spot with clear printed text, or type it in manually."
          } else if (!is.null(prefill_text)) {
            "Auto-read from the box you drew — check it's exactly right (including punctuation) and edit if not."
          } else {
            "This exact text gets searched for on every uploaded submission — pick something short, distinctive, and unlikely to repeat elsewhere on the form."
          }
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("reference_point_modal_ok"), "Save Reference Point", class = "btn-primary")
        )
      )
    }

    observeEvent(input$reference_point_modal_ok, {
      nm <- trimws(input$ref_name_input)
      if (nzchar(nm) == FALSE) {
        showNotification("Reference point name cannot be empty.", type = "error")
        return()
      }
      anchor_text <- trimws(input$ref_anchor_text_input)
      if (nzchar(anchor_text) == FALSE) {
        showNotification("Anchor text cannot be empty — this is what gets searched for on each submission.", type = "error")
        return()
      }

      rp <- reference_points_rv()

      if (is.null(edit_ref_id())) {
        box <- pending_box()
        req(box)
        new_row <- data.frame(
          ref_id = uuid::UUIDgenerate(), name = nm,
          x = box$x, y = box$y, w = box$w, h = box$h,
          anchor_text = anchor_text,
          stringsAsFactors = FALSE
        )
        reference_points_rv(rbind(rp, new_row))
      } else {
        idx <- which(rp$ref_id == edit_ref_id())
        if (length(idx) == 1) {
          rp$name[idx] <- nm
          rp$anchor_text[idx] <- anchor_text
          reference_points_rv(rp)
        }
      }

      pending_box(NULL)
      edit_ref_id(NULL)
      removeModal()
      push_boxes_to_canvas()
    })

    output$reference_point_table <- renderDT({
      rp <- reference_points_rv()
      if (nrow(rp) == 0) {
        return(datatable(
          data.frame(Message = paste("No reference points yet — switch Drawing mode to",
                                      "Reference Point and draw a box around a distinctive printed feature.")),
          rownames = FALSE, options = list(dom = "t", paging = FALSE)
        ))
      }

      display <- data.frame(
        Name = rp$name,
        `Anchor Text` = rp$anchor_text,
        Actions = vapply(rp$ref_id, function(rid) {
          sprintf(
            paste0(
              '<button class="btn btn-primary btn-sm fe-btn-sm" ',
              'onclick="Shiny.setInputValue(\'%s\', {id:\'%s\', nonce:Date.now()}, {priority:\'event\'})">Edit</button> ',
              '<button class="btn btn-danger btn-sm fe-btn-sm" ',
              'onclick="Shiny.setInputValue(\'%s\', {id:\'%s\', nonce:Date.now()}, {priority:\'event\'})">Delete</button>'
            ),
            ns("edit_reference_point"), rid, ns("delete_reference_point"), rid
          )
        }, character(1)),
        check.names = FALSE, stringsAsFactors = FALSE
      )

      datatable(
        display, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(dom = "t", paging = FALSE, scrollY = "160px", scrollCollapse = TRUE)
      )
    })

    observeEvent(input$edit_reference_point, {
      rp <- reference_points_rv()
      row <- rp[rp$ref_id == input$edit_reference_point$id, ]
      req(nrow(row) == 1)
      edit_ref_id(row$ref_id)
      showModal(reference_point_modal(mode = "edit", ref = list(name = row$name, anchor_text = row$anchor_text)))
    })

    observeEvent(input$delete_reference_point, {
      rp <- reference_points_rv()
      reference_points_rv(rp[rp$ref_id != input$delete_reference_point$id, ])
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
        reference_points      = reference_points_rv(),
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

      rp <- t$reference_points
      if (is.null(rp) || NROW(rp) == 0) {
        reference_points_rv(empty_ref_points_df())  # backward compat with pre-reference-point templates
      } else {
        rp_df <- as.data.frame(rp, stringsAsFactors = FALSE)
        if (!"anchor_text" %in% names(rp_df)) rp_df$anchor_text <- ""  # backward compat with pre-anchor_text templates
        reference_points_rv(rp_df)
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
      reference_points_rv(empty_ref_points_df())
      updateTextInput(session, "template_name", value = "")
      updateCheckboxInput(session, "upload_is_filled", value = FALSE)
    })

  })
}
