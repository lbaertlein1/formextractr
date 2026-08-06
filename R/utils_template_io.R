# utils_template_io.R
#
# Storage layer for form templates. JSON-file backend now; swappable for
# Postgres later without touching callers.
#
# One template = one form image (blank, or a filled example if no blank
# was available — see is_filled_reference) + its field annotations.
# (Earlier revision supported multiple layout "versions" per template;
# removed for complexity — one template per layout, create a new
# template if a form's layout changes.)
#
# {
#   "template_id":   "<uuid>",
#   "template_name": "<string>",
#   "created_at":    "<ISO8601>",
#   "updated_at":    "<ISO8601>",
#   "image_file":    "<relative path under data/template_images/>",
#   "image_width":   <int px>,
#   "image_height":  <int px>,
#   "is_filled_reference": <bool — TRUE if annotated from a completed
#       form because no blank was available>,
#   "reference_points": [
#     { "ref_id": "<uuid>", "name": "<string>", "x":<0-1>,"y":<0-1>,"w":<0-1>,"h":<0-1>,
#       "anchor_text": "<the exact text printed at this location, used to locate this
#         point on a submission at extraction time via a Claude API call — see
#         locate_text_via_api() in utils_extraction.R. Auto-filled by reading the
#         patch when the box is drawn in Template Designer, always user-editable.
#         Empty string for reference points created before this field existed;
#         those get skipped at extraction time with a message asking to fill it in.>" },
#     ...
#   ],
#   "fields": [
#     {
#       "field_id": "<uuid>", "name": "<string>", "type": "<FIELD_TYPES value>",
#       "x":<0-1>,"y":<0-1>,"w":<0-1>,"h":<0-1>,
#       "options": "<semicolon-separated string, only for circled_text; '' otherwise>"
#     }, ...
#   ]
# }

library(jsonlite)
library(uuid)
library(fs)

#' Save a template (creates new or overwrites existing by template_id)
save_template <- function(template_id = NULL, template_name,
                           image_file, image_width, image_height,
                           is_filled_reference = FALSE, reference_points = NULL, fields) {
  fs::dir_create(TEMPLATE_DIR)

  is_new <- is.null(template_id)
  if (is_new) template_id <- uuid::UUIDgenerate()

  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS0Z", tz = "UTC")
  existing <- if (!is_new) tryCatch(load_template(template_id), error = function(e) NULL) else NULL

  template <- list(
    template_id          = template_id,
    template_name        = template_name,
    created_at           = if (!is.null(existing)) existing$created_at else now,
    updated_at           = now,
    image_file           = image_file,
    image_width          = image_width,
    image_height         = image_height,
    is_filled_reference  = is_filled_reference,
    reference_points     = if (is.null(reference_points)) list() else reference_points,
    fields               = fields
  )

  out_path <- fs::path(TEMPLATE_DIR, paste0(template_id, ".json"))
  jsonlite::write_json(template, out_path, auto_unbox = TRUE, pretty = TRUE)

  template_id
}

#' Load a single template by ID
load_template <- function(template_id) {
  path <- fs::path(TEMPLATE_DIR, paste0(template_id, ".json"))
  if (!fs::file_exists(path)) {
    stop("Template not found: ", template_id)
  }
  jsonlite::read_json(path, simplifyVector = TRUE)
}

#' List all saved templates (summary only)
list_templates <- function() {
  fs::dir_create(TEMPLATE_DIR)
  files <- fs::dir_ls(TEMPLATE_DIR, glob = "*.json")

  if (length(files) == 0) {
    return(data.frame(
      template_id = character(), template_name = character(),
      updated_at = character(), field_count = integer(),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(files, function(f) {
    t <- jsonlite::read_json(f, simplifyVector = TRUE)
    n_fields <- if (is.null(t$fields)) 0 else NROW(t$fields)
    data.frame(
      template_id   = t$template_id,
      template_name = t$template_name,
      updated_at    = t$updated_at,
      field_count   = n_fields,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out[order(out$updated_at, decreasing = TRUE), ]
}

#' Delete a template
delete_template <- function(template_id) {
  path <- fs::path(TEMPLATE_DIR, paste0(template_id, ".json"))
  if (fs::file_exists(path)) fs::file_delete(path)
  invisible(TRUE)
}
