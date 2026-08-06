# mod_extraction.R
#
# Shiny module: Extraction + Manual Review
#
# Load one or more completed-form images/PDFs, pick which saved template
# they match (manual selection — no auto version/template matching is
# built), and run field-by-field extraction via the Claude API
# (utils_extraction.R). Results land in a wide table (one row per
# submission) with per-cell low-confidence flags, two summary columns
# (Overall Confidence, Low Confidence Count), Edit/Delete action buttons
# per row, and can be downloaded as CSV. Delete drops a submission from
# the results entirely.
#
# Edit opens a review popup with two views (toggle at the top):
#   - "Fields": source image + field boxes overlaid, values editable,
#     the focused field highlighted in red on the image. Save Changes
#     commits manual edits (marks them REVIEWED, clears their flag).
#   - "Reference Points": the PRE-WARP submission image (before
#     alignment) with each reference point's matched location marked —
#     green/orange/red for good/low-confidence/failed match — and
#     DRAGGABLE, so a wrong match can be corrected by hand for this one
#     submission. "Re-align & Re-extract" then re-warps using the
#     corrected point(s) and re-runs the actual field extraction
#     (new API calls) against the newly-aligned image, replacing that
#     submission's results.
#
# DATA MODEL: long_results_rv() (one row per field per submission,
# carrying field_name/field_type/x/y/w/h/value/confidence/error/
# submission_index/submission_image_path/submission_prewarp_image_path)
# is the source of truth for FIELD data; results_rv() (the wide table)
# is rebuilt FROM it via build_submission_row()/rebuild_wide_from_long(),
# so those two can't drift apart. ref_matches_rv() is a PARALLEL
# data.frame (submission_index, ref_id, name, x, y, score, tmpl_x,
# tmpl_y) for reference-point data, which doesn't fit the per-field
# long format — reference points aren't fields, there are only 2-4 of
# them per submission, and they carry their own (x,y) rather than a
# (x,y,w,h) box. review_overrides_rv() holds manual drag-corrections
# (submission_index, ref_id, x, y), kept separate from ref_matches_rv()
# so "what the algorithm found" and "what the user corrected it to"
# stay distinguishable — Re-align & Re-extract prefers an override when
# one exists, falling back to the original match otherwise.
#
# current_template_rv() caches the template object used in the last
# extraction run, so the review modal's Re-align & Re-extract button
# (which needs template$fields, image_width/height, etc.) doesn't have
# to reload it from disk on every click.
#
# There's no dedicated "field-level details" UI (raw per-field
# confidence/error detail is fully present in long_results_rv(), just
# not surfaced as its own table — see log_extraction_details(), which
# writes it to the R console instead).
#
# This runs synchronously in the Shiny session — for a big batch this
# will block the UI for a while (one API call per field per file).
#
# Public API:
#   mod_extraction_ui(id)
#   mod_extraction_server(id)

library(shiny)
library(DT)
library(htmltools)

mod_extraction_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", href = "css/styles.css"),
      tags$script(src = "js/review_highlight.js"),
      tags$script(src = "js/ref_point_drag.js")
    ),
    titlePanel("Extract Data From Submissions"),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(ns("template_select"), "Template to extract against", choices = NULL),
        fileInput(ns("submission_upload"), "Upload completed form(s)",
                  multiple = TRUE,
                  accept = c(".png", ".jpg", ".jpeg", ".tif", ".tiff", ".pdf")),
        actionButton(ns("run_extraction_btn"), "Run Extraction", class = "btn-primary", width = "100%"),
        tags$hr(),
        downloadButton(ns("download_csv_btn"), "Download Results (CSV)"),
        tags$hr(),
        helpText("Each field is read individually via the Claude API — the field's ",
                 "type controls how it's prompted (handwriting vs. printed text, ",
                 "which option is circled, tally-mark counting, etc.)."),
        helpText(tags$strong("Review:"), " click Edit on any row to open a popup. ",
                 "The Fields view shows editable values against the source image; ",
                 "the Reference Points view (if the template has any) shows where each ",
                 "alignment anchor was matched, and lets you drag one to correct it and ",
                 "re-run extraction for just that submission."),
        helpText(tags$strong("Alignment:"), " if you place reference points below before ",
                 "running extraction, those are used directly — no guessing involved. ",
                 "Otherwise, if the template has 2+ reference points, each one's exact ",
                 "printed text gets located automatically via the API (less reliable — ",
                 "manual placement is recommended if accuracy matters). Falls back further ",
                 "to matching the form's printed outer border, then plain resizing."),
        helpText(tags$strong("Requires"), " the ", tags$code("ANTHROPIC_API_KEY"),
                 " environment variable to be set on the server running this app.")
      ),
      mainPanel(
        width = 9,
        uiOutput(ns("status_ui")),
        uiOutput(ns("ref_point_placement_ui")),
        DTOutput(ns("results_table"))
      )
    )
  )
}

mod_extraction_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    long_results_rv <- reactiveVal(NULL)  # one row per field per submission — source of truth
    results_rv       <- reactiveVal(NULL) # wide pivot, rebuilt from long_results_rv()
    ref_matches_rv    <- reactiveVal(NULL) # one row per reference point per submission
    review_overrides_rv <- reactiveVal(data.frame(submission_index = integer(), ref_id = character(),
                                                   x = numeric(), y = numeric(), stringsAsFactors = FALSE))
    current_template_rv <- reactiveVal(NULL)  # template object from the last extraction run
    status_rv         <- reactiveVal(NULL)
    review_submission_id <- reactiveVal(NULL)  # which submission_index the review modal is currently editing
    review_view_mode      <- reactiveVal("fields")  # "fields" | "refpoints", reset each time Edit is clicked

    # ---- pre-extraction reference point placement --------------------------
    # Lets the user manually place reference points on each uploaded file
    # BEFORE running the (expensive) field extraction, rather than only
    # being able to correct a possibly-wrong automatic guess after 18
    # API calls have already run against bad alignment. Confirmed via
    # testing that manual placement produces accurate extraction — the
    # bottleneck was specifically automatic detection, not anything
    # downstream of it.
    pre_ref_idx <- reactiveVal(1)  # which uploaded file is currently shown
    pre_ref_overrides_rv <- reactiveVal(data.frame(
      file_name = character(), ref_id = character(), x = numeric(), y = numeric(),
      stringsAsFactors = FALSE
    ))

    observeEvent(input$submission_upload, { pre_ref_idx(1) }, ignoreInit = TRUE)
    observeEvent(input$template_select, { pre_ref_idx(1) }, ignoreInit = TRUE)

    # The currently-selected template's reference points, or NULL if
    # there aren't at least 2 — drives whether the placement panel
    # shows at all.
    selected_template_refpoints <- reactive({
      req(input$template_select, nzchar(input$template_select))
      t <- tryCatch(load_template(input$template_select), error = function(e) NULL)
      if (is.null(t) || is.null(t$reference_points) || NROW(t$reference_points) < 2) return(NULL)
      as.data.frame(t$reference_points, stringsAsFactors = FALSE)
    })

    # Prepared (converted/deskewed) preview of whichever file is
    # currently shown — uses the SAME prep_submission_image() the real
    # extraction loop uses, so what you place against is guaranteed to
    # be what extraction actually aligns against.
    current_pre_ref_image <- reactive({
      files <- input$submission_upload
      req(files)
      idx <- pre_ref_idx()
      req(idx >= 1, idx <= nrow(files))
      prep_submission_image(files$datapath[idx], files$name[idx])
    })

    output$ref_point_placement_ui <- renderUI({
      rp <- selected_template_refpoints()
      files <- input$submission_upload
      if (is.null(rp) || is.null(files)) return(NULL)

      idx <- pre_ref_idx()
      n <- nrow(files)
      img <- current_pre_ref_image()
      if (is.null(img)) {
        return(tags$div(
          h4("Reference Points"),
          helpText(sprintf("Couldn't preview file %d (%s) — it'll fall back to automatic ",
                            "alignment detection at extraction time.", idx, files$name[idx]))
        ))
      }

      info <- magick::image_info(img)
      img_w <- info$width[1]; img_h <- info$height[1]
      tmp <- tempfile(fileext = ".png")
      magick::image_write(img, tmp, format = "png")
      img_data <- base64enc::base64encode(tmp)
      unlink(tmp)

      overrides <- pre_ref_overrides_rv()
      overrides <- overrides[overrides$file_name == files$name[idx], ]

      markers <- lapply(seq_len(nrow(rp)), function(i) {
        r <- rp[i, ]
        ov <- overrides[overrides$ref_id == r$ref_id, ]
        if (nrow(ov) == 1) {
          center_x <- ov$x[1] / img_w; center_y <- ov$y[1] / img_h
          placed <- TRUE
        } else {
          # Default: same relative position as on the template — a free
          # geometric guess, no API call, often a reasonable starting
          # point on its own for forms with fairly consistent layout.
          center_x <- r$x + r$w / 2; center_y <- r$y + r$h / 2
          placed <- FALSE
        }
        left_frac <- center_x - r$w / 2
        top_frac <- center_y - r$h / 2

        tags$div(
          class = paste("fe-refpoint-marker", if (placed) "fe-refpoint-good" else "fe-refpoint-low"),
          `data-ref-id` = r$ref_id,
          `data-drag-input` = ns("pre_refpoint_drag"),
          style = sprintf("left:%.3f%%; top:%.3f%%; width:%.3f%%; height:%.3f%%;",
                           left_frac * 100, top_frac * 100, r$w * 100, r$h * 100),
          title = sprintf("%s%s — drag to the correct spot on this form", r$name,
                           if (placed) " (placed)" else " (default guess — drag to confirm/correct)"),
          tags$span(class = "fe-refpoint-label", r$name)
        )
      })

      tagList(
        h4("Reference Points"),
        helpText("Drag each marker to its correct spot on THIS file (green = you've placed ",
                 "it; orange = still at the untouched default guess — same relative position ",
                 "as on the template). Files you don't visit here still get that default guess ",
                 "used directly at extraction time — automatic API detection is no longer the ",
                 "default; it's a fallback only for reference points with no anchor text, ",
                 "or if fewer than 2 points end up usable for a file."),
        tags$div(
          style = "display:flex; align-items:center; gap:12px; margin-bottom:8px;",
          actionButton(ns("pre_ref_prev_btn"), "< Prev", class = "btn-sm"),
          tags$strong(sprintf("File %d of %d: %s", idx, n, files$name[idx])),
          actionButton(ns("pre_ref_next_btn"), "Next >", class = "btn-sm")
        ),
        tags$div(
          class = "fe-review-image-pane", style = "flex: 0 0 100%; max-width: 700px;",
          tags$div(
            class = "fe-overlay-wrap",
            tags$img(src = sprintf("data:image/png;base64,%s", img_data), style = "width:100%; display:block;"),
            tagList(markers)
          )
        )
      )
    })

    observeEvent(input$pre_ref_prev_btn, {
      pre_ref_idx(max(1, pre_ref_idx() - 1))
    })
    observeEvent(input$pre_ref_next_btn, {
      files <- input$submission_upload
      req(files)
      pre_ref_idx(min(nrow(files), pre_ref_idx() + 1))
    })

    observeEvent(input$pre_refpoint_drag, {
      drag <- input$pre_refpoint_drag
      files <- input$submission_upload
      req(files)
      idx <- pre_ref_idx()
      req(idx >= 1, idx <= nrow(files))
      fname <- files$name[idx]

      img <- current_pre_ref_image()
      req(img)
      info <- magick::image_info(img)

      ov <- pre_ref_overrides_rv()
      ov <- ov[!(ov$file_name == fname & ov$ref_id == drag$ref_id), ]
      ov <- rbind(ov, data.frame(
        file_name = fname, ref_id = drag$ref_id,
        x = drag$x * info$width[1], y = drag$y * info$height[1],
        stringsAsFactors = FALSE
      ))
      pre_ref_overrides_rv(ov)
    })

    # ---- pivot: one submission's long-format rows -> one wide row -------
    # Reused for the initial extraction AND every manual-edit save, so
    # the wide table's meta columns (confidence summary, flags) are
    # always freshly derived from long_results_rv(), never hand-patched.
    build_submission_row <- function(sub_long) {
      flag_mask <- needs_review(sub_long$value, sub_long$confidence, sub_long$error)

      wide_vals <- as.data.frame(
        as.list(setNames(sub_long$value, sub_long$field_name)),
        stringsAsFactors = FALSE
      )
      value_cols <- names(wide_vals)  # de-duplicated (e.g. Facility.Type, Facility.Type.1)

      flagged <- vapply(which(flag_mask), function(idx) {
        if (!is.na(sub_long$error[idx])) {
          sprintf("%s (error: %s)", value_cols[idx], sub_long$error[idx])
        } else if (is.na(sub_long$value[idx])) {
          sprintf("%s (no value returned)", value_cols[idx])
        } else if (sub_long$value[idx] %in% c("UNCLEAR", "BLANK")) {
          sprintf("%s (%s)", value_cols[idx], sub_long$value[idx])
        } else {
          sprintf("%s (low confidence)", value_cols[idx])
        }
      }, character(1))

      meta <- data.frame(
        source_file = sub_long$source_file[1],
        `Overall Confidence` = if (any(flag_mask)) "LOW" else "HIGH",
        `Low Confidence Count` = sum(flag_mask),
        flags = if (length(flagged) > 0) paste(flagged, collapse = "; ") else "",
        check.names = FALSE, stringsAsFactors = FALSE
      )
      row <- cbind(meta, wide_vals, stringsAsFactors = FALSE)
      row$submission_index <- sub_long$submission_index[1]  # internal only — stripped before display/CSV
      row
    }

    rebuild_wide_from_long <- function(long_df) {
      do.call(rbind, lapply(split(long_df, long_df$submission_index), build_submission_row))
    }

    # Prewarp images aren't stored with their own pixel dimensions
    # anywhere, so this reads them back via image_info() when needed
    # (converting between drag-fraction and pixel coordinates, or
    # rendering markers). Only called when the Reference Points view is
    # actually open, not on every extraction — the cost is acceptable.
    get_prewarp_dims <- function(sid) {
      long_df <- long_results_rv()
      req(long_df)
      path <- long_df$submission_prewarp_image_path[long_df$submission_index == sid][1]
      if (is.na(path) || !file.exists(path)) return(NULL)
      info <- magick::image_info(magick::image_read(path))
      list(width = info$width[1], height = info$height[1])
    }

    # ---- template list ----------------------------------------------------
    observe({
      tmpls <- list_templates()
      choices <- if (nrow(tmpls) == 0) {
        c("No saved templates — build one in Template Designer first" = "")
      } else {
        setNames(tmpls$template_id, sprintf("%s (%d fields)", tmpls$template_name, tmpls$field_count))
      }
      updateSelectInput(session, "template_select", choices = choices)
    })

    output$status_ui <- renderUI({
      s <- status_rv()
      if (is.null(s)) return(NULL)
      tags$div(style = "margin-bottom:10px;", s)
    })

    # ---- run extraction -----------------------------------------------------
    observeEvent(input$run_extraction_btn, {
      req(input$template_select, nzchar(input$template_select))
      req(input$submission_upload)

      api_key <- Sys.getenv("ANTHROPIC_API_KEY")
      if (!nzchar(api_key)) {
        showNotification(
          "ANTHROPIC_API_KEY is not set on the server. Set it as an environment variable and restart the app.",
          type = "error", duration = NULL
        )
        return()
      }

      template <- load_template(input$template_select)
      fields <- template$fields
      if (is.null(fields) || NROW(fields) == 0) {
        showNotification("This template has no fields to extract.", type = "error")
        return()
      }
      template$fields <- as.data.frame(fields, stringsAsFactors = FALSE)
      n_fields <- nrow(template$fields)

      # Compute the template's own border once for the whole run (same
      # every time within it) rather than redetecting it per submission.
      # NULL here just means the border-detection FALLBACK is
      # unavailable for this run — logged, not fatal. Reference-point
      # alignment (if the template has >= 2) doesn't need the
      # template's own image at all anymore — it searches the
      # SUBMISSION for known text via the API, not a cropped patch from
      # the template — so this only affects the fallback chain, never
      # reference-point alignment itself.
      template_img_path <- file.path(TEMPLATE_IMAGE_DIR, basename(template$image_file))
      template_img <- tryCatch(magick::image_read(template_img_path), error = function(e) NULL)
      if (is.null(template_img)) {
        message("Couldn't read the template's own image — border-detection fallback is disabled for this run (reference points, if the template has >= 2, are unaffected).")
      }
      template_border <- if (is.null(template_img)) NULL else tryCatch({
        detect_page_border(template_img)
      }, error = function(e) NULL)
      if (!is.null(template_img) && is.null(template_border)) {
        message("Couldn't detect an outer border on the template image — border-detection fallback is disabled for this run (reference points, if the template has >= 2, are unaffected).")
      }

      files <- input$submission_upload
      n_files <- nrow(files)
      long_rows <- vector("list", n_files)
      ref_match_rows <- vector("list", n_files)

      rp_df <- if (!is.null(template$reference_points) && NROW(template$reference_points) >= 2) {
        as.data.frame(template$reference_points, stringsAsFactors = FALSE)
      } else {
        NULL
      }

      withProgress(message = "Extracting fields", value = 0, {
        for (i in seq_len(n_files)) {
          fname <- files$name[i]
          fpath <- files$datapath[i]

          sub_result_full <- tryCatch({
            if (!is.null(rp_df)) {
              # PRIMARY alignment path when the template has reference
              # points: manually-placed (or default geometric-guess,
              # same relative position as on the template) points from
              # the placement panel above — no API call for alignment
              # at all. Replaced automatic API-based text search as the
              # default after confirming: manual points -> accurate
              # extraction, automatic detection -> not reliable enough.
              prep_img <- prep_submission_image(fpath, fname)
              if (is.null(prep_img)) stop("Couldn't read/convert this submission file.")
              info <- magick::image_info(prep_img)

              overrides <- pre_ref_overrides_rv()
              overrides <- overrides[overrides$file_name == fname, ]

              points <- do.call(rbind, lapply(seq_len(nrow(rp_df)), function(k) {
                r <- rp_df[k, ]
                ov <- overrides[overrides$ref_id == r$ref_id, ]
                if (nrow(ov) == 1) {
                  px <- ov$x[1]; py <- ov$y[1]
                } else {
                  px <- (r$x + r$w / 2) * info$width[1]
                  py <- (r$y + r$h / 2) * info$height[1]
                }
                data.frame(
                  ref_id = r$ref_id, name = r$name, w = r$w, h = r$h,
                  x = px, y = py, score = NA_real_,
                  tmpl_x = (r$x + r$w / 2) * template$image_width,
                  tmpl_y = (r$y + r$h / 2) * template$image_height,
                  stringsAsFactors = FALSE
                )
              }))

              prewarp_path <- tempfile(fileext = ".png")
              magick::image_write(prep_img, prewarp_path, format = "png")

              reextract_submission_with_points(
                api_key, prewarp_path, template, points,
                on_field = function(j, nf, field_name) {
                  incProgress(1 / (n_files * n_fields),
                              detail = sprintf("File %d/%d (%s): %s", i, n_files, fname, field_name))
                }
              )
            } else {
              # No reference points on this template — border detection
              # (if the template image has a detectable outer border),
              # else plain resize.
              src_path <- fpath
              if (tolower(fs::path_ext(fname)) == "pdf") {
                converted <- convert_pdf_page_to_image(fpath, page = 1)
                src_path <- converted$path
              }
              extract_submission(
                api_key, src_path, template, template_border = template_border,
                on_field = function(j, nf, field_name) {
                  incProgress(1 / (n_files * n_fields),
                              detail = sprintf("File %d/%d (%s): %s", i, n_files, fname, field_name))
                }
              )
            }
          }, error = function(e) {
            list(
              results = data.frame(
                field_name = template$fields$name, field_type = template$fields$type,
                x = template$fields$x, y = template$fields$y, w = template$fields$w, h = template$fields$h,
                value = NA_character_, confidence = "UNKNOWN",
                error = paste("Submission-level error:", conditionMessage(e)),
                stringsAsFactors = FALSE
              ),
              image_path = NA_character_, prewarp_image_path = NA_character_, ref_matches = NULL
            )
          })

          sub_result <- sub_result_full$results
          sub_result$source_file <- fname
          sub_result$submission_index <- i
          sub_result$submission_image_path <- sub_result_full$image_path
          sub_result$submission_prewarp_image_path <- sub_result_full$prewarp_image_path
          long_rows[[i]] <- sub_result

          if (!is.null(sub_result_full$ref_matches)) {
            rm <- sub_result_full$ref_matches
            rm$submission_index <- i
            ref_match_rows[[i]] <- rm
          }
        }
      })

      long_df <- do.call(rbind, Filter(Negate(is.null), long_rows))
      long_results_rv(long_df)
      results_rv(rebuild_wide_from_long(long_df))
      log_extraction_details(long_df)  # console-only now — see helper below

      ref_matches_rv(do.call(rbind, Filter(Negate(is.null), ref_match_rows)))
      current_template_rv(template)

      status_rv(sprintf("Extracted %d submission(s) against template '%s' (%d fields each). Use Edit to review a submission.",
                         n_files, template$template_name, n_fields))
    })

    # ---- results table (with per-cell low-confidence flags + Edit/Delete) --
    output$results_table <- renderDT({
      df <- results_rv()
      if (is.null(df)) {
        return(datatable(
          data.frame(Message = "Upload submissions, pick a template, and click Run Extraction."),
          rownames = FALSE, options = list(dom = "t", paging = FALSE)
        ))
      }

      # flags is kept in results_rv()/the CSV export (it's useful there)
      # but dropped from this on-screen table — Edit + the per-cell
      # highlight already surface what needs review without a text column.
      display_cols <- setdiff(names(df), c("submission_index", "flags"))
      disp <- df[, display_cols, drop = FALSE]

      long_df <- long_results_rv()
      meta_n <- 3  # source_file, Overall Confidence, Low Confidence Count
      field_col_idx <- (meta_n + 1):ncol(disp)

      for (r in seq_len(nrow(disp))) {
        sub_long <- long_df[long_df$submission_index == df$submission_index[r], ]
        flag_mask <- needs_review(sub_long$value, sub_long$confidence, sub_long$error)
        for (k in seq_along(field_col_idx)) {
          if (isTRUE(flag_mask[k])) {
            col <- field_col_idx[k]
            val <- disp[r, col]
            disp[r, col] <- sprintf(
              '<span class="fe-low-conf" title="Low confidence — click Edit to review">%s</span>',
              htmlspecial(if (is.na(val)) "" else val)
            )
          }
        }
      }

      actions_vec <- vapply(df$submission_index, function(sid) {
        sprintf(
          paste0(
            '<button class="btn btn-primary btn-sm fe-btn-sm" ',
            'onclick="Shiny.setInputValue(\'%s\', {id:%d, nonce:Date.now()}, {priority:\'event\'})">Edit</button> ',
            '<button class="btn btn-danger btn-sm fe-btn-sm" ',
            'onclick="Shiny.setInputValue(\'%s\', {id:%d, nonce:Date.now()}, {priority:\'event\'})">Delete</button>'
          ),
          ns("edit_submission"), sid, ns("delete_submission"), sid
        )
      }, character(1))

      # Actions goes in first column — shift the field-column indices (used
      # for the escape= list below) by one to account for it, rather than
      # recomputing the flagging loop above against a post-insert layout.
      disp <- cbind(data.frame(Actions = actions_vec, stringsAsFactors = FALSE), disp, stringsAsFactors = FALSE)
      field_col_idx <- field_col_idx + 1
      actions_col_idx <- 1

      datatable(
        disp, rownames = FALSE, selection = "none",
        escape = -c(field_col_idx, actions_col_idx),
        options = list(dom = "t", paging = FALSE, scrollY = "500px", scrollX = TRUE, scrollCollapse = TRUE)
      )
    })

    # ---- Edit button -> review modal ---------------------------------------
    observeEvent(input$edit_submission, {
      sid <- input$edit_submission$id
      long_df <- long_results_rv(); req(long_df)
      sub_long <- long_df[long_df$submission_index == sid, ]
      req(nrow(sub_long) > 0)

      review_submission_id(sid)
      review_view_mode("fields")
      showModal(review_modal(sid))
    })

    # ---- Delete button -> drop submission from results ---------------------
    observeEvent(input$delete_submission, {
      sid <- input$delete_submission$id
      long_df <- long_results_rv(); req(long_df)
      long_df <- long_df[long_df$submission_index != sid, ]

      long_results_rv(if (nrow(long_df) == 0) NULL else long_df)
      results_rv(if (nrow(long_df) == 0) NULL else rebuild_wide_from_long(long_df))

      rm_df <- ref_matches_rv()
      if (!is.null(rm_df)) ref_matches_rv(rm_df[rm_df$submission_index != sid, ])
      ov_df <- review_overrides_rv()
      review_overrides_rv(ov_df[ov_df$submission_index != sid, ])

      showNotification("Submission removed from results.", type = "message")
    })

    # ---- review modal: two views (Fields / Reference Points) --------------
    review_modal <- function(sid) {
      long_df <- long_results_rv()
      sub_long <- long_df[long_df$submission_index == sid, ]

      rm_df <- ref_matches_rv()
      sub_matches <- if (!is.null(rm_df)) rm_df[rm_df$submission_index == sid, ] else NULL
      has_ref_points <- !is.null(sub_matches) && nrow(sub_matches) > 0

      mode <- review_view_mode()
      if (!has_ref_points) mode <- "fields"

      toggle_ui <- if (has_ref_points) {
        tags$div(
          class = "fe-refpoint-toggle",
          actionButton(ns("review_view_fields_btn"), "Fields",
                       class = if (mode == "fields") "btn-primary btn-sm" else "btn-default btn-sm"),
          actionButton(ns("review_view_refpoints_btn"), "Reference Points",
                       class = if (mode == "refpoints") "btn-primary btn-sm" else "btn-default btn-sm")
        )
      } else NULL

      body <- if (mode == "refpoints") review_refpoints_view(sid, sub_matches) else review_fields_view(sub_long)

      footer_buttons <- if (mode == "refpoints") {
        tagList(
          modalButton("Cancel"),
          actionButton(ns("reextract_submission_btn"), "Re-align & Re-extract This Submission", class = "btn-primary")
        )
      } else {
        tagList(
          modalButton("Cancel"),
          actionButton(ns("save_review_btn"), "Save Changes", class = "btn-primary")
        )
      }

      modalDialog(
        title = sprintf("Review: %s", sub_long$source_file[1]),
        size = "l",
        toggle_ui,
        body,
        footer = footer_buttons
      )
    }

    # ---- Fields view: values editable, boxes + focus-highlight on image ---
    review_fields_view <- function(sub_long) {
      field_rows <- lapply(seq_len(nrow(sub_long)), function(i) {
        low <- needs_review(sub_long$value[i], sub_long$confidence[i], sub_long$error[i])
        input_id <- ns(sprintf("review_field_%d", i))
        # data-field-idx ties this row to its matching overlay box on the
        # image — www/js/review_highlight.js listens for focus anywhere
        # under .fe-review-fields-pane and highlights the box with the
        # same index in red.
        input_tag <- htmltools::tagAppendAttributes(
          textInput(input_id, NULL, value = ifelse(is.na(sub_long$value[i]), "", sub_long$value[i]), width = "100%"),
          `data-field-idx` = i
        )
        tags$div(
          class = if (low) "fe-review-row fe-review-row-low" else "fe-review-row",
          tags$div(class = "fe-review-label",
                    tags$strong(sub_long$field_name[i]),
                    tags$span(class = "fe-review-type", sprintf(" (%s)", sub_long$field_type[i])),
                    if (low) tags$span(class = "fe-low-conf-badge", "LOW CONFIDENCE")
          ),
          input_tag
        )
      })

      img_path <- sub_long$submission_image_path[1]
      image_pane <- if (!is.na(img_path) && file.exists(img_path)) {
        img_data <- base64enc::base64encode(img_path)
        overlays <- lapply(seq_len(nrow(sub_long)), function(i) {
          low <- needs_review(sub_long$value[i], sub_long$confidence[i], sub_long$error[i])
          tags$div(
            class = if (low) "fe-overlay-box fe-overlay-box-low" else "fe-overlay-box",
            `data-field-idx` = i,
            title = sub_long$field_name[i],
            style = sprintf(
              "left:%.3f%%; top:%.3f%%; width:%.3f%%; height:%.3f%%;",
              sub_long$x[i] * 100, sub_long$y[i] * 100, sub_long$w[i] * 100, sub_long$h[i] * 100
            )
          )
        })
        tags$div(
          class = "fe-review-image-pane",
          tags$div(
            class = "fe-overlay-wrap",
            tags$img(src = sprintf("data:image/png;base64,%s", img_data), style = "width:100%; display:block;"),
            tagList(overlays)
          )
        )
      } else {
        tags$div(class = "fe-review-image-pane",
                  helpText("Original form image isn't available for this submission."))
      }

      tags$div(
        class = "fe-review-split",
        image_pane,
        tags$div(class = "fe-review-fields-pane", tagList(field_rows))
      )
    }

    # ---- Reference Points view: pre-warp image, draggable match markers ---
    review_refpoints_view <- function(sid, sub_matches) {
      long_df <- long_results_rv()
      sub_long <- long_df[long_df$submission_index == sid, ]
      img_path <- sub_long$submission_prewarp_image_path[1]

      if (is.na(img_path) || !file.exists(img_path)) {
        return(helpText("The pre-alignment image isn't available for this submission."))
      }

      dims <- get_prewarp_dims(sid)
      if (is.null(dims)) return(helpText("Couldn't read the pre-alignment image for this submission."))

      overrides <- review_overrides_rv()
      overrides <- overrides[overrides$submission_index == sid, ]

      img_data <- base64enc::base64encode(img_path)

      markers <- lapply(seq_len(nrow(sub_matches)), function(i) {
        rid <- sub_matches$ref_id[i]
        nm <- sub_matches$name[i]
        ov <- overrides[overrides$ref_id == rid, ]

        # Box size as annotated on the template (normalized 0-1), used
        # directly as a fraction of the displayed prewarp image too —
        # an approximation (template and submission aren't necessarily
        # identical scale) but a much better visual reference than a
        # fixed-size dot regardless of how big the box actually was.
        w_frac <- if ("w" %in% names(sub_matches) && !is.na(sub_matches$w[i])) sub_matches$w[i] else 0.05
        h_frac <- if ("h" %in% names(sub_matches) && !is.na(sub_matches$h[i])) sub_matches$h[i] else 0.03

        if (nrow(ov) == 1) {
          center_x <- ov$x[1] / dims$width; center_y <- ov$y[1] / dims$height
          status <- "corrected"
        } else if (!is.na(sub_matches$x[i])) {
          center_x <- sub_matches$x[i] / dims$width; center_y <- sub_matches$y[i] / dims$height
          status <- if (is.na(sub_matches$score[i])) {
            "corrected"  # a previously-accepted correction, re-shown after a prior re-extract
          } else if (sub_matches$score[i] >= EXTRACTION_REF_MIN_SCORE) "good" else "low"
        } else {
          center_x <- 0.5; center_y <- 0.5  # never matched — no sensible position; drag it into place
          status <- "missing"
        }

        left_frac <- center_x - w_frac / 2
        top_frac <- center_y - h_frac / 2

        status_class <- switch(status,
          corrected = "fe-refpoint-good", good = "fe-refpoint-good",
          low = "fe-refpoint-low", missing = "fe-refpoint-missing"
        )
        score_txt <- if (status == "corrected") {
          "manually corrected"
        } else if (!is.na(sub_matches$score[i])) {
          # score here is a confidence-derived pseudo-value (1.0/0.2 for
          # HIGH/LOW), not a real match-quality metric — show it as
          # what it actually is rather than a decimal that would look
          # like false precision.
          if (sub_matches$score[i] >= EXTRACTION_REF_MIN_SCORE) "located, high confidence" else "located, low confidence"
        } else {
          "no match found"
        }

        tags$div(
          class = paste("fe-refpoint-marker", status_class),
          `data-ref-id` = rid,
          `data-drag-input` = ns("refpoint_drag"),
          style = sprintf("left:%.3f%%; top:%.3f%%; width:%.3f%%; height:%.3f%%;",
                           left_frac * 100, top_frac * 100, w_frac * 100, h_frac * 100),
          title = sprintf("%s (%s)", nm, score_txt),
          tags$span(class = "fe-refpoint-label", nm)
        )
      })

      tagList(
        helpText("Drag a marker to correct its position for THIS submission only (the template ",
                 "itself is never changed), then click \"Re-align & Re-extract\" below. ",
                 "Green = matched confidently or manually corrected; orange = matched but low ",
                 "confidence; red = no match found at all (placed at center — drag it into position)."),
        tags$div(
          class = "fe-review-image-pane", style = "flex: 0 0 100%; max-width: 700px;",
          tags$div(
            class = "fe-overlay-wrap",
            tags$img(src = sprintf("data:image/png;base64,%s", img_data), style = "width:100%; display:block;"),
            tagList(markers)
          )
        )
      )
    }

    observeEvent(input$review_view_fields_btn, {
      review_view_mode("fields")
      sid <- review_submission_id(); req(!is.null(sid))
      showModal(review_modal(sid))
    })

    observeEvent(input$review_view_refpoints_btn, {
      review_view_mode("refpoints")
      sid <- review_submission_id(); req(!is.null(sid))
      showModal(review_modal(sid))
    })

    # ---- dragging a reference point marker ---------------------------------
    observeEvent(input$refpoint_drag, {
      drag <- input$refpoint_drag
      sid <- review_submission_id(); req(!is.null(sid))
      dims <- get_prewarp_dims(sid); req(!is.null(dims))

      ov <- review_overrides_rv()
      ov <- ov[!(ov$submission_index == sid & ov$ref_id == drag$ref_id), ]
      ov <- rbind(ov, data.frame(
        submission_index = sid, ref_id = drag$ref_id,
        x = drag$x * dims$width, y = drag$y * dims$height,
        stringsAsFactors = FALSE
      ))
      review_overrides_rv(ov)

      # Re-render so the marker switches to "corrected" (green) styling
      # without needing another interaction to see the change took.
      showModal(review_modal(sid))
    })

    # ---- Re-align & Re-extract: apply corrections, re-run this submission -
    observeEvent(input$reextract_submission_btn, {
      sid <- review_submission_id(); req(!is.null(sid))

      api_key <- Sys.getenv("ANTHROPIC_API_KEY")
      if (!nzchar(api_key)) {
        showNotification("ANTHROPIC_API_KEY is not set on the server.", type = "error", duration = NULL)
        return()
      }

      template <- current_template_rv()
      if (is.null(template)) {
        showNotification("No template context available for this session — run a fresh extraction first.",
                          type = "error")
        return()
      }

      long_df <- long_results_rv(); req(long_df)
      sub_long <- long_df[long_df$submission_index == sid, ]
      prewarp_path <- sub_long$submission_prewarp_image_path[1]
      if (is.na(prewarp_path) || !file.exists(prewarp_path)) {
        showNotification("The pre-alignment image for this submission is no longer available — can't re-align.",
                          type = "error")
        return()
      }

      rm_df <- ref_matches_rv()
      points <- rm_df[rm_df$submission_index == sid, ]
      if (nrow(points) < 2) {
        showNotification("Need at least 2 reference points to align.", type = "error")
        return()
      }

      overrides <- review_overrides_rv()
      overrides <- overrides[overrides$submission_index == sid, ]
      for (i in seq_len(nrow(points))) {
        ov <- overrides[overrides$ref_id == points$ref_id[i], ]
        if (nrow(ov) == 1) {
          points$x[i] <- ov$x[1]
          points$y[i] <- ov$y[1]
        }
      }

      removeModal()
      n_fields <- nrow(template$fields)

      result <- NULL
      withProgress(message = sprintf("Re-extracting %s", sub_long$source_file[1]), value = 0, {
        result <- tryCatch({
          reextract_submission_with_points(
            api_key, prewarp_path, template, points,
            on_field = function(j, nf, field_name) incProgress(1 / nf, detail = field_name)
          )
        }, error = function(e) {
          showNotification(paste("Re-extraction failed:", conditionMessage(e)), type = "error", duration = NULL)
          NULL
        })
      })
      req(!is.null(result))

      new_sub <- result$results
      new_sub$source_file <- sub_long$source_file[1]
      new_sub$submission_index <- sid
      new_sub$submission_image_path <- result$image_path
      new_sub$submission_prewarp_image_path <- result$prewarp_image_path

      long_df <- long_results_rv()
      long_df <- rbind(long_df[long_df$submission_index != sid, ], new_sub)
      long_results_rv(long_df)
      results_rv(rebuild_wide_from_long(long_df))

      # The corrected positions are now the accepted match for this
      # submission going forward — score NA marks them "corrected"
      # rather than a fresh algorithmic match (see the styling logic in
      # review_refpoints_view()) — and the separate overrides bookkeeping
      # is cleared since it's now baked into ref_matches_rv() itself.
      points$score <- NA_real_
      rm_all <- ref_matches_rv()
      ref_matches_rv(rbind(rm_all[rm_all$submission_index != sid, ], points))
      ov_all <- review_overrides_rv()
      review_overrides_rv(ov_all[ov_all$submission_index != sid, ])

      review_view_mode("fields")
      showModal(review_modal(sid))
      showNotification(sprintf("Re-extracted %s using corrected alignment.", sub_long$source_file[1]), type = "message")
    })

    observeEvent(input$save_review_btn, {
      sid <- review_submission_id(); req(!is.null(sid))

      long_df <- long_results_rv(); req(long_df)
      idx <- which(long_df$submission_index == sid)

      for (i in seq_along(idx)) {
        new_val <- input[[sprintf("review_field_%d", i)]]
        if (!is.null(new_val)) {
          long_df$value[idx[i]] <- new_val
          long_df$confidence[idx[i]] <- "REVIEWED"  # human-verified — no longer flagged
          long_df$error[idx[i]] <- NA_character_
        }
      }

      long_results_rv(long_df)
      results_rv(rebuild_wide_from_long(long_df))
      review_submission_id(NULL)
      removeModal()
    })

    # ---- CSV export (never includes the internal submission_index) --------
    output$download_csv_btn <- downloadHandler(
      filename = function() sprintf("extraction_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) {
        df <- results_rv()
        req(df)
        write.csv(df[, setdiff(names(df), "submission_index"), drop = FALSE], file, row.names = FALSE, na = "")
      }
    )

  })
}

# Minimal HTML-escaping so a field value containing <, >, or & can't
# break the low-confidence <span> wrapper it gets embedded in. Not using
# htmltools::htmlEscape to avoid adding a new package dependency for
# four gsub() calls.
htmlspecial <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

#' Print one line per field to the R console (via message(), so it goes
#' to stderr/server logs rather than stdout) after each extraction run.
#' This is the replacement for the old "Show field-level details" UI
#' table — the same information (value, confidence, error), still fully
#' available, just for whoever's running the app to see in logs rather
#' than a table end users have to think about.
log_extraction_details <- function(long_df) {
  if (is.null(long_df) || nrow(long_df) == 0) return(invisible())
  message(sprintf("---- Extraction details: %d field result(s) across %d submission(s) ----",
                   nrow(long_df), length(unique(long_df$submission_index))))
  for (i in seq_len(nrow(long_df))) {
    r <- long_df[i, ]
    flag <- if (needs_review(r$value, r$confidence, r$error)) " [NEEDS REVIEW]" else ""
    err_part <- if (!is.na(r$error)) sprintf(" | error: %s", r$error) else ""
    message(sprintf(
      "[%s] %s = %s (confidence: %s)%s%s",
      r$source_file, r$field_name, if (is.na(r$value)) "<NA>" else r$value,
      r$confidence, err_part, flag
    ))
  }
  message("---- End extraction details ----")
}
