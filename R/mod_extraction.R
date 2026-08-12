# mod_extraction.R
#
# Shiny module: Extraction + Manual Review
#
# Load one or more completed-form images/PDFs, pick which saved template
# they match (manual selection — no auto version/template matching is
# built), and run extraction via the Claude API (utils_extraction.R):
# ONE API call per submission, the full unmodified image plus every
# field's name/type/instructions, asking Claude to read the whole form
# and return every value at once. No alignment step of any kind —
# Claude locates each field itself, by reading the form's own printed
# labels and structure, the same way a person would. See
# extract_submission_holistic() in utils_extraction.R for the prompt
# and the module header there for why this replaced the earlier
# reference-point/homography/per-field-crop pipeline (short version:
# that pipeline's actual failure mode was locating fields correctly
# across differently-framed photos, not reading them once located —
# giving the model the whole page removes the need to locate anything
# via geometry at all).
#
# Results land in a wide table (one row per submission) with per-cell
# low-confidence flags, two summary columns (Overall Confidence, Low
# Confidence Count), Edit/Delete action buttons per row, and can be
# downloaded as CSV. Delete drops a submission from the results
# entirely.
#
# Edit opens a review popup with a single view: the full submission
# image next to every extracted value as an editable field, flagged
# (low-confidence/BLANK/UNCLEAR/error) rows highlighted. No overlay
# boxes — there's no per-field crop position to draw anymore, since
# nothing crops per field. Save Changes commits manual edits (marks
# them REVIEWED, clears their flag).
#
# DATA MODEL: long_results_rv() (one row per field per submission,
# carrying field_name/field_type/value/confidence/error/
# submission_index/submission_image_path) is the source of truth for
# FIELD data; results_rv() (the wide table) is rebuilt FROM it via
# build_submission_row()/rebuild_wide_from_long(), so those two can't
# drift apart.
#
# current_template_rv() caches the template object used in the last
# extraction run.
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
      tags$link(rel = "stylesheet", href = "css/styles.css")
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
        helpText("A multi-page PDF is treated as multiple submissions, one per page — ",
                 "e.g. a facility that scans several completed tally sheets into a single ",
                 "file will get one results row per page, labeled \"page P of N\", not just ",
                 "the first page."),
        helpText("Each submission is read as a whole in a single API call — the full ",
                 "image plus every field's name, type, and instructions (handwriting vs. ",
                 "printed text, which option is circled, tally-mark counting, etc.), so ",
                 "Claude locates each value by reading the form's own labels and structure, ",
                 "the same way a person reviewing it would. It also checks whether the ",
                 "submission is actually the expected form at all — if not, every field is ",
                 "left blank rather than guessed, and the row is flagged 'UNEXPECTED FORM' ",
                 "in the Form Match column instead of being scored on confidence."),
        helpText(tags$strong("Review:"), " click Edit on any row to open a popup showing the ",
                 "full source image alongside every extracted value as an editable field, ",
                 "flagged rows highlighted. Most submissions won't need this — only rows with ",
                 "a nonzero Low Confidence Count, or a Form Match of \"No\"."),
        helpText(tags$strong("Requires"), " the ", tags$code("ANTHROPIC_API_KEY"),
                 " environment variable to be set on the server running this app.")
      ),
      mainPanel(
        width = 9,
        uiOutput(ns("status_ui")),
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
    current_template_rv <- reactiveVal(NULL)  # template object from the last extraction run
    status_rv         <- reactiveVal(NULL)
    review_submission_id <- reactiveVal(NULL)  # which submission_index the review modal is currently editing
    
    
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
      
      # form_match_ok/form_match_reason are the same value repeated on
      # every field row for this submission (set once per submission in
      # the extraction loop below) — same pattern as source_file.
      form_match_ok <- if ("form_match_ok" %in% names(sub_long)) sub_long$form_match_ok[1] else NA
      form_match_reason <- if ("form_match_reason" %in% names(sub_long)) sub_long$form_match_reason[1] else NA_character_
      mismatched <- isFALSE(form_match_ok)
      
      flagged <- if (mismatched) {
        # Every field already carries the same "doesn't match" error
        # message (see extract_submission_holistic()) — repeating it
        # once per field here would just be noise; one clear line
        # covers it.
        sprintf("UNEXPECTED FORM — %s", if (!is.na(form_match_reason) && nzchar(form_match_reason)) form_match_reason else "no reason given")
      } else {
        vapply(which(flag_mask), function(idx) {
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
      }
      
      overall_confidence <- if (mismatched) {
        "UNEXPECTED FORM"
      } else if (any(flag_mask)) {
        "LOW"
      } else {
        "HIGH"
      }
      
      meta <- data.frame(
        source_file = sub_long$source_file[1],
        `Form Match` = if (is.na(form_match_ok)) "Unknown" else if (form_match_ok) "Yes" else "No",
        `Overall Confidence` = overall_confidence,
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
      
      files <- input$submission_upload
      # One row per SUBMISSION, not per uploaded FILE — a multi-page PDF
      # expands into one work item per page (see expand_submission_uploads()
      # in utils_extraction.R). A single-page PDF or an image file is
      # still exactly one row, so this is a no-op for the common case.
      work_items <- expand_submission_uploads(files)
      n_files <- nrow(work_items)
      long_rows <- vector("list", n_files)
      
      withProgress(message = "Extracting submissions", value = 0, {
        for (i in seq_len(n_files)) {
          fname <- work_items$fname[i]
          fpath <- work_items$fpath[i]
          page <- work_items$page[i]
          display_name <- work_items$display_name[i]
          incProgress(1 / n_files, detail = sprintf("File %d/%d: %s", i, n_files, display_name))
          
          sub_result_full <- tryCatch({
            prep_img <- prep_submission_image(fpath, fname, page = page)
            if (is.null(prep_img)) stop("Couldn't read/convert this submission file.")
            img_path <- tempfile(fileext = ".png")
            magick::image_write(prep_img, img_path, format = "png")
            # ONE call for the whole submission: the full image plus every
            # field's name/type/instructions, so Claude locates each value
            # by reading the form itself (labels, structure) rather than a
            # precomputed crop position. See extract_submission_holistic()
            # in utils_extraction.R — it also checks whether this even IS
            # the expected form (form_match) and forces every field to
            # BLANK if not, rather than guessing at unrelated content.
            extract_submission_holistic(api_key, img_path, template)
          }, error = function(e) {
            list(
              results = data.frame(
                field_name = template$fields$name, field_type = template$fields$type,
                value = NA_character_, confidence = "UNKNOWN",
                error = paste("Submission-level error:", conditionMessage(e)),
                stringsAsFactors = FALSE
              ),
              image_path = NA_character_,
              # A call/read failure here isn't a form-match verdict either
              # way — NA, not FALSE, so it's not mistaken for a genuine
              # "wrong form" flag in the results table.
              form_match = list(is_match = NA, reason = NA_character_)
            )
          })
          
          sub_result <- sub_result_full$results
          sub_result$source_file <- display_name
          sub_result$submission_index <- i
          sub_result$submission_image_path <- sub_result_full$image_path
          sub_result$form_match_ok <- sub_result_full$form_match$is_match
          sub_result$form_match_reason <- sub_result_full$form_match$reason
          long_rows[[i]] <- sub_result
        }
      })
      
      long_df <- do.call(rbind, Filter(Negate(is.null), long_rows))
      long_results_rv(long_df)
      results_rv(rebuild_wide_from_long(long_df))
      log_extraction_details(long_df)  # console-only now — see helper below
      
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
      meta_n <- 4  # source_file, Form Match, Overall Confidence, Low Confidence Count
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
      showModal(review_modal(sid))
    })
    
    # ---- Delete button -> drop submission from results ---------------------
    observeEvent(input$delete_submission, {
      sid <- input$delete_submission$id
      long_df <- long_results_rv(); req(long_df)
      long_df <- long_df[long_df$submission_index != sid, ]
      
      long_results_rv(if (nrow(long_df) == 0) NULL else long_df)
      results_rv(if (nrow(long_df) == 0) NULL else rebuild_wide_from_long(long_df))
      
      showNotification("Submission removed from results.", type = "message")
    })
    
    # ---- review modal: single view, full image beside editable fields -----
    review_modal <- function(sid) {
      long_df <- long_results_rv()
      sub_long <- long_df[long_df$submission_index == sid, ]
      
      mismatched <- isFALSE(sub_long$form_match_ok[1])
      banner <- if (mismatched) {
        tags$div(
          style = "background:#fdecea;border:1px solid #f5c6cb;color:#611a15;padding:8px 12px;margin-bottom:10px;border-radius:4px;",
          tags$strong("Unexpected form: "),
          "this submission doesn't appear to match the selected template",
          if (!is.na(sub_long$form_match_reason[1]) && nzchar(sub_long$form_match_reason[1])) {
            sprintf(" — %s", sub_long$form_match_reason[1])
          },
          ". Fields were left blank rather than guessed. Check the image below — ",
          "this may be the wrong file, the wrong template selected, or the wrong PDF page."
        )
      } else NULL
      
      modalDialog(
        title = sprintf("Review: %s", sub_long$source_file[1]),
        size = "l",
        banner,
        review_fields_view(sub_long),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_review_btn"), "Save Changes", class = "btn-primary")
        )
      )
    }
    
    # ---- Fields view: full source image beside an editable value per field.
    # No overlay boxes and no alignment-correction UI — the image is shown
    # as-is; the reviewer locates each value on it the same way the
    # extraction model does, by reading labels, not by trusting a computed
    # crop position. Simpler, and doesn't imply a precision the underlying
    # alignment doesn't actually have.
    review_fields_view <- function(sub_long) {
      field_rows <- lapply(seq_len(nrow(sub_long)), function(i) {
        low <- needs_review(sub_long$value[i], sub_long$confidence[i], sub_long$error[i])
        input_id <- ns(sprintf("review_field_%d", i))
        tags$div(
          class = if (low) "fe-review-row fe-review-row-low" else "fe-review-row",
          tags$div(class = "fe-review-label",
                   tags$strong(sub_long$field_name[i]),
                   tags$span(class = "fe-review-type", sprintf(" (%s)", sub_long$field_type[i])),
                   if (low) tags$span(class = "fe-low-conf-badge", "LOW CONFIDENCE")
          ),
          textInput(input_id, NULL, value = ifelse(is.na(sub_long$value[i]), "", sub_long$value[i]), width = "100%")
        )
      })
      
      img_path <- sub_long$submission_image_path[1]
      image_pane <- if (!is.na(img_path) && file.exists(img_path)) {
        img_data <- base64enc::base64encode(img_path)
        tags$div(
          class = "fe-review-image-pane",
          tags$img(src = sprintf("data:image/png;base64,%s", img_data), style = "width:100%; display:block;")
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