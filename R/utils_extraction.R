# utils_extraction.R
#
# Field-level extraction via the Claude API (vision). Each field's
# annotated bounding box gets cropped out of a submission image and sent
# as a single-field question, with the prompt tailored to the field's
# `type` — this is why type matters so much in the designer (a
# circled_text field needs its options list; a tally_count field needs
# a counting instruction; etc).
#
# ALIGNMENT: fields are cropped from the ORIGINAL, UNMODIFIED submission
# image — nothing here ever warps/resamples/distorts the actual photo.
# Alignment produces a COORDINATE TRANSFORM (a function mapping template
# pixel coordinates to submission pixel coordinates), not a transformed
# image; each field's stored template-space box gets mapped through
# that transform to find its real position on the real photo, and gets
# cropped directly from there. See fit_point_transform() below.
#
# Three ways to get that transform, tried in order:
#   1. Reference points (preferred, general-purpose): if the template
#      has >= 2 user-marked reference points (Template Designer, each
#      one an exact printed phrase — "anchor_text"), every one is
#      LOCATED on the submission via a Claude API call
#      (locate_reference_points_two_pass()) — "where on this form does
#      this text appear" — and the located points fit a transform:
#      similarity (2 points: translate/rotate/scale), affine (3: +shear),
#      or full perspective/homography (4: true keystone correction for
#      a photo taken at an angle). fit_point_transform() does the fit
#      AND inverts it (submission->template gets fit, then inverted to
#      template->submission, which is the direction actually needed)
#      via ordinary linear algebra (solve()) — no image warping.
#
#      An earlier version of this used magick::image_distort() to
#      literally warp the whole submission image into template space,
#      then cropped fields from that. It worked geometrically but had a
#      real failure mode: a degenerate point set (clustered together,
#      nearly collinear) doesn't make image_distort() fail — it just
#      produces a wild, sheared, unusable image with no error, since
#      resampling an entire image will always "succeed" at producing
#      *something*. Coordinate transforms don't have that failure mode
#      the same way: fitting one from a degenerate point set makes the
#      underlying linear system singular, which solve() actually
#      refuses to do — an explicit, catchable failure instead of a
#      silent bad result. There's also no reason to ever reconstruct a
#      full aligned page in the first place — every field gets cropped
#      and sent to the API individually regardless, so nothing downstream
#      needs a warped whole-image artifact to exist at all.
#
#      This in turn replaced an even earlier from-scratch pixel cross-
#      correlation matching approach that went through five rounds of
#      debugging without becoming reliable (see prior history in this
#      file's git log / conversation record) — a vision model that can
#      read text finding text it can read was simply a better-suited
#      approach than hand-rolled patch matching.
#   2. Border detection (fallback): if no reference points are defined,
#      detects the form's own printed outer border in both the template
#      and the submission (detect_page_border()) and fits a simple
#      scale+translate transform between them — narrower (needs a clear
#      rectangular border) but automatic, no annotation required.
#   3. Plain scale (last resort): if neither of the above finds enough
#      to work with, falls back to a pure scale-from-origin transform
#      (submission_dims / template_dims) — the "just resize" behavior,
#      expressed as a transform like everything else rather than as a
#      special case.
# See align_submission() below for the actual fallback chain.
#
# No deskew/rotation-correction step exists (removed entirely) — with
# reference points, rotation is handled as one degree of freedom within
# the similarity/affine/perspective fit; a separate blind pre-rotation
# pass would be redundant at best.

library(httr2)
library(base64enc)
library(magick)

#' Build the type-specific reading instructions for a field (what to
#' look for, how to answer, what counts as blank). The shared response-
#' format wrapper (confidence + value) is applied separately in
#' build_field_prompt() so that contract lives in exactly one place.
build_field_instructions <- function(field) {
  common_blank <- "If the area is blank, the value is: BLANK"

  switch(field$type,
    "text_printed" = paste(
      "Read the printed text in this image crop from a paper form.",
      "The value is the text content, nothing else.", common_blank
    ),
    "text_handwritten" = paste(
      "Read the handwritten text in this image crop from a paper form.",
      "The value is the text content, nothing else.",
      "If the area is blank or illegible, the value is: BLANK"
    ),
    "date_handwritten" = paste(
      "Read the handwritten date in this image crop from a paper form.",
      "The value is the date in YYYY-MM-DD format if you can determine it.",
      common_blank,
      "If a date is present but the day/month/year is ambiguous or illegible, the value is: UNCLEAR"
    ),
    "numeric" = paste(
      "Read the number written in this image crop from a paper form.",
      "The value is the digits only, no other text, no commas.", common_blank
    ),
    "checkbox" = paste(
      "Look at this image crop of a checkbox from a paper form.",
      "The value is CHECKED if it is filled in/marked, or UNCHECKED if it is empty."
    ),
    "circled_text" = {
      opts <- trimws(strsplit(field$options, ";")[[1]])
      sprintf(paste(
        "This image crop shows a printed list of options from a paper form: %s.",
        "Exactly one option should be circled, underlined, or otherwise marked as selected.",
        "The value is the exact text of the selected option, matching one of the options listed.",
        "If none appear selected, the value is: BLANK",
        "If it's ambiguous which one is selected, the value is: UNCLEAR"
      ), paste(opts, collapse = ", "))
    },
    "tally_count" = paste(
      "This image crop shows handwritten tally/hash marks on a paper form",
      "(individual strokes, sometimes grouped in 5s with a diagonal strike through each group of 4).",
      "Count the TOTAL number of individual marks (a group of 5 counts as 5, not 1).",
      "The value is the total count as a single integer.",
      "If there are none, the value is: 0"
    ),
    # fallback for any future/unrecognized type
    "Read the content of this image crop from a paper form. The value is the content, nothing else."
  )
}

#' Shared response-format contract, wrapped around the type-specific
#' instructions. Asking the model to self-report confidence is what lets
#' extract_field_value() flag "confidently wrong" answers (like a
#' misread stylized "26" coming back as "Rd") that would otherwise sail
#' through silently — BLANK/UNCLEAR only catch the cases where the model
#' itself hedges.
build_field_prompt <- function(field) {
  sprintf(
    paste(
      "%s",
      "",
      "Respond in exactly this two-line format, nothing else:",
      "CONFIDENCE: HIGH or LOW",
      "VALUE: <the value, per the instructions above>",
      "",
      "Use LOW confidence if the handwriting is unclear, stylized in a way",
      "that could plausibly be misread, ambiguous, or you are guessing at",
      "all. Use HIGH only if you are confident the value is unambiguous.",
      sep = "\n"
    ),
    build_field_instructions(field)
  )
}

#' Crop one field's region from the ORIGINAL, UNMODIFIED submission
#' image — no whole-image resize/warp ever happens. Maps the field's
#' stored template-space box (all 4 corners, not just center) through
#' `transform_fn` (from fit_point_transform()) to find where it
#' actually is on the real photo, crops the axis-aligned bounding box
#' of the result, and upscales small crops for legibility.
#'
#' A rotated/sheared field box maps to a non-axis-aligned quadrilateral
#' in submission space; cropping its bounding box (rather than warping
#' just that small region to a perfect rectangle) can include a bit of
#' extra margin around the true field content for anything beyond a
#' pure translate+scale transform. That's an acceptable trade — a few
#' extra background pixels around a field crop don't meaningfully
#' confuse the vision model reading it, and this never distorts the
#' field's own content the way warping would.
#'
#' @param submission_img The ORIGINAL submission image (any size —
#'   never pre-resized to template dimensions)
#' @param field One field's row-as-list: x, y, w, h (template-normalized
#'   0-1)
#' @param transform_fn A function(tmpl_x, tmpl_y) -> list(x, y) mapping
#'   TEMPLATE pixel coordinates to SUBMISSION pixel coordinates, from
#'   fit_point_transform()
#' @param template_width,template_height Template's pixel dimensions —
#'   needed to convert the field's normalized 0-1 box into template
#'   PIXEL coordinates before applying transform_fn
#' @return list(path = <temp PNG path>, x0, y0, x1, y1 = the crop's
#'   bounding box in the ORIGINAL submission's own pixel coordinates —
#'   kept so callers can record exactly where each field actually came
#'   from, e.g. for the manual-review UI's overlay boxes)
crop_field_image <- function(submission_img, field, transform_fn, template_width, template_height) {
  x0t <- field$x * template_width
  y0t <- field$y * template_height
  x1t <- (field$x + field$w) * template_width
  y1t <- (field$y + field$h) * template_height

  corners <- list(
    transform_fn(x0t, y0t), transform_fn(x1t, y0t),
    transform_fn(x0t, y1t), transform_fn(x1t, y1t)
  )
  xs <- vapply(corners, function(c) c$x, numeric(1))
  ys <- vapply(corners, function(c) c$y, numeric(1))

  sub_info <- magick::image_info(submission_img)
  sub_w <- sub_info$width[1]; sub_h <- sub_info$height[1]

  x0 <- max(0, min(xs)); y0 <- max(0, min(ys))
  x1 <- min(sub_w, max(xs)); y1 <- min(sub_h, max(ys))
  w <- max(1, round(x1 - x0)); h <- max(1, round(y1 - y0))

  cropped <- magick::image_crop(submission_img, sprintf("%dx%d+%d+%d", w, h, round(x0), round(y0)))

  if (min(w, h) < EXTRACTION_MIN_CROP_DIM) {
    scale <- ceiling(EXTRACTION_MIN_CROP_DIM / max(1, min(w, h)))
    # Explicit target pixel dims + "!", not percentage geometry — see
    # the note on why "%" resize is risky (DPI/density-metadata
    # sensitivity, confirmed as a real bug earlier in this project).
    cropped <- magick::image_resize(cropped, sprintf("%dx%d!", w * scale, h * scale))
  }

  tmp <- tempfile(fileext = ".png")
  magick::image_write(cropped, tmp, format = "png")
  list(path = tmp, x0 = x0, y0 = y0, x1 = x1, y1 = y1)
}

#' Call the Claude API on one cropped field image. Retries once on
#' transient failures (network blip, 429, 5xx) before giving up.
#'
#' @return list(value = <string or NA>, confidence = "HIGH"/"LOW"/"UNKNOWN",
#'   error = <string or NULL>)
extract_field_value <- function(api_key, image_path, field, retries = 1) {
  img_b64 <- base64enc::base64encode(image_path)
  prompt <- build_field_prompt(field)

  body <- list(
    model = EXTRACTION_MODEL,
    max_tokens = 200,
    messages = list(list(
      role = "user",
      content = list(
        list(type = "image", source = list(type = "base64", media_type = "image/png", data = img_b64)),
        list(type = "text", text = prompt)
      )
    ))
  )

  attempt <- function() {
    httr2::request(ANTHROPIC_API_URL) |>
      httr2::req_headers(
        "x-api-key" = api_key,
        "anthropic-version" = ANTHROPIC_API_VERSION,
        "content-type" = "application/json"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(60) |>
      httr2::req_error(is_error = function(resp) FALSE) |>  # never auto-throw; we inspect status ourselves below
      httr2::req_perform()
  }

  resp <- tryCatch(attempt(), error = function(e) e)
  if (inherits(resp, "error") && retries > 0) {
    Sys.sleep(2)
    resp <- tryCatch(attempt(), error = function(e) e)
  }

  if (inherits(resp, "error")) {
    # Genuine network/connection failure (DNS, TLS, timeout) — never
    # reached Anthropic's servers at all.
    return(list(value = NA_character_, confidence = "UNKNOWN",
                error = paste("Connection failed:", conditionMessage(resp))))
  }

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    # Anthropic returns a structured {"error":{"message":...}} body on
    # 4xx/5xx — that message is the actual reason (e.g. "temperature:
    # Input should be..." or "invalid x-api-key"), which a bare status
    # code can't tell us. Surface it directly instead of just "HTTP 400".
    err_body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    err_msg <- if (!is.null(err_body$error$message)) {
      err_body$error$message
    } else {
      tryCatch(httr2::resp_body_string(resp), error = function(e) "(no response body)")
    }

    if (status %in% c(429, 500, 502, 503, 529) && retries > 0) {
      Sys.sleep(2)
      return(extract_field_value(api_key, image_path, field, retries = retries - 1))
    }
    return(list(value = NA_character_, confidence = "UNKNOWN",
                error = sprintf("API error (HTTP %d): %s", status, err_msg)))
  }

  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$content)) {
    return(list(value = NA_character_, confidence = "UNKNOWN", error = "Unexpected API response shape"))
  }

  text_blocks <- Filter(function(b) identical(b$type, "text"), parsed$content)
  raw_text <- if (length(text_blocks) > 0) trimws(text_blocks[[1]]$text) else ""

  # Parse the "CONFIDENCE: .../VALUE: ..." format. If the model didn't
  # follow it for some reason, fall back to the raw text as the value
  # and mark confidence UNKNOWN — which gets flagged for review just
  # like LOW, since we can't verify it was a clean read.
  conf_m  <- regmatches(raw_text, regexec("CONFIDENCE:\\s*(HIGH|LOW)", raw_text, ignore.case = TRUE))[[1]]
  value_m <- regmatches(raw_text, regexec("VALUE:\\s*(.*)", raw_text, ignore.case = TRUE))[[1]]

  confidence <- if (length(conf_m) >= 2) toupper(conf_m[2]) else "UNKNOWN"
  value <- if (length(value_m) >= 2) trimws(value_m[2]) else raw_text

  list(value = value, confidence = confidence, error = NULL)
}

#' A field's extraction "needs review" if the call errored, returned no
#' value, came back BLANK/UNCLEAR, or the model itself reported LOW/
#' UNKNOWN confidence. This is the single definition used everywhere
#' that decision matters — the flags column, per-cell highlighting in
#' the results table, and the per-record summary counts in
#' mod_extraction.R — so those views can't drift out of sync with each
#' other the way flags vs. the wide table briefly did during development.
needs_review <- function(value, confidence, error) {
  !is.na(error) | is.na(value) | value %in% c("UNCLEAR", "BLANK") | confidence %in% c("LOW", "UNKNOWN")
}

#' Detect the four edges (in pixel coordinates) of the strong outer
#' black-bordered rectangle that most printed tally-sheet-style forms
#' have framing the whole table. This is the "reference point" this
#' app uses to align a submission to its template.
#'
#' Implementation note: rather than reading raw per-pixel data (whose
#' array dimension ordering from magick::image_data() isn't something
#' this was able to verify against a live R session), this collapses
#' each row to a single average-brightness pixel by resizing the whole
#' image to 1px wide (and separately to 1px tall for columns) —
#' ImageMagick averages when downsampling, so a solid black line
#' spanning nearly a full row/column pulls that row/column's average
#' brightness way down, while ordinary text-on-white rows stay bright.
#' The output of a 1xh (or wx1) resize is unambiguous by construction —
#' there's no dimension-order guess involved.
#'
#' @return list(top, bottom, left, right) in pixel coordinates, or NULL
#'   if no row/column was dark enough on every side (e.g. the border
#'   was cropped out of the photo, or the form doesn't have one) —
#'   callers should fall back to plain resizing in that case, not fail.
detect_page_border <- function(img, brightness_threshold = EXTRACTION_BORDER_BRIGHTNESS_MAX) {
  gray <- magick::image_convert(img, colorspace = "gray")
  info <- magick::image_info(gray)
  w <- info$width[1]; h <- info$height[1]

  row_avg <- as.integer(magick::image_data(magick::image_resize(gray, sprintf("1x%d!", h)), channels = "gray"))
  col_avg <- as.integer(magick::image_data(magick::image_resize(gray, sprintf("%dx1!", w)), channels = "gray"))

  top_rows <- which(row_avg < brightness_threshold)
  left_cols <- which(col_avg < brightness_threshold)

  if (length(top_rows) == 0 || length(left_cols) == 0) return(NULL)

  list(top = min(top_rows), bottom = max(top_rows),
       left = min(left_cols), right = max(left_cols))
}

#' Fit a scale+translate transform (template pixel coords -> submission
#' pixel coords) from each image's own detected outer border — the
#' border-detection alignment path, expressed as a coordinate transform
#' like everything else in this file (no image warping). If a
#' submission was scanned with different margins/crop/scale than the
#' template, this corrects for that; it does NOT correct rotation
#' (unlike the reference-point fits) since a detected border alone
#' doesn't tell you the rotation angle, only its axis-aligned extent.
#'
#' Falls back to fit_plain_scale_transform() if either border can't be
#' detected — this should never be worse than that baseline, only
#' sometimes miss out on the improvement.
#'
#' @return A function(tmpl_x, tmpl_y) -> list(x, y), or NULL if the
#'   submission's own border couldn't be detected AND no fallback makes
#'   sense (in practice this always returns a function — the plain-scale
#'   fallback covers the "couldn't detect anything" case)
fit_border_transform <- function(img, template_border, template_width, template_height) {
  if (is.null(template_border)) {
    return(fit_plain_scale_transform(img, template_width, template_height))
  }

  sub_border <- detect_page_border(img)
  if (is.null(sub_border)) {
    message("Couldn't detect this submission's outer border — falling back to plain scale (no border alignment) for it.")
    return(fit_plain_scale_transform(img, template_width, template_height))
  }

  sub_w <- sub_border$right - sub_border$left
  sub_h <- sub_border$bottom - sub_border$top
  if (sub_w < 10 || sub_h < 10) {
    message("Detected submission border was implausibly small — falling back to plain scale for it.")
    return(fit_plain_scale_transform(img, template_width, template_height))
  }

  tmpl_w <- template_border$right - template_border$left
  tmpl_h <- template_border$bottom - template_border$top
  scale_x <- sub_w / tmpl_w
  scale_y <- sub_h / tmpl_h

  function(tmpl_x, tmpl_y) {
    list(x = sub_border$left + (tmpl_x - template_border$left) * scale_x,
         y = sub_border$top + (tmpl_y - template_border$top) * scale_y)
  }
}

#' Last-resort transform: pure scale from the origin, submission
#' dimensions / template dimensions — the "just resize" behavior,
#' expressed as a transform like every other alignment method here
#' rather than as a special image-resizing case.
fit_plain_scale_transform <- function(img, template_width, template_height) {
  info <- magick::image_info(img)
  scale_x <- info$width[1] / template_width
  scale_y <- info$height[1] / template_height
  function(tmpl_x, tmpl_y) list(x = tmpl_x * scale_x, y = tmpl_y * scale_y)
}

#' Read whatever text is in a small image crop. Used two ways:
#'  - Template Designer auto-fills a reference point's anchor_text with
#'    this when the box is drawn, so the person annotating doesn't have
#'    to type out exactly what's printed there (still fully editable).
#'  - Nothing else — extraction-time text LOCATION is a different call
#'    (locate_text_via_api(), below), since "what does this say" and
#'    "where on this whole page does that text appear" are different
#'    questions with different prompts.
#' Simplified relative to extract_field_value(): no field-type-specific
#' prompt, no confidence/retry machinery — this is a best-effort
#' convenience fill, not something extraction accuracy depends on.
#'
#' @return The read text, "" if the crop appears blank, or NA on any
#'   failure (network, missing key, unexpected response) — callers
#'   should treat NA as "auto-fill didn't work, leave it for the user
#'   to type," not as an error to surface loudly.
ocr_patch <- function(api_key, patch_img) {
  tmp <- tempfile(fileext = ".png")
  magick::image_write(patch_img, tmp, format = "png")
  img_b64 <- tryCatch(base64enc::base64encode(tmp), error = function(e) NA_character_)
  unlink(tmp)
  if (is.na(img_b64)) return(NA_character_)

  body <- list(
    model = EXTRACTION_MODEL, max_tokens = 100,
    messages = list(list(role = "user", content = list(
      list(type = "image", source = list(type = "base64", media_type = "image/png", data = img_b64)),
      list(type = "text", text = paste(
        "Read the text in this image crop from a paper form.",
        "Respond with ONLY the exact text, nothing else.",
        "If there's no legible text, respond with exactly: NONE"
      ))
    )))
  )

  resp <- tryCatch({
    httr2::request(ANTHROPIC_API_URL) |>
      httr2::req_headers("x-api-key" = api_key, "anthropic-version" = ANTHROPIC_API_VERSION,
                          "content-type" = "application/json") |>
      httr2::req_body_json(body) |> httr2::req_timeout(30) |>
      httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
  }, error = function(e) e)

  if (inherits(resp, "error") || httr2::resp_status(resp) >= 400) return(NA_character_)
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$content)) return(NA_character_)
  text_blocks <- Filter(function(b) identical(b$type, "text"), parsed$content)
  if (length(text_blocks) == 0) return(NA_character_)
  raw <- trimws(text_blocks[[1]]$text)
  if (identical(raw, "NONE")) "" else raw
}

#' Locate a piece of known text somewhere in a (possibly large) form
#' Draw visible boxes + labels for each reference point directly onto a
#' COPY of the template image (magick::image_draw() opens the image as
#' a base-R graphics device with pixel coordinates matching the image
#' directly — top-left origin, no flipping needed). Used to give the
#' holistic locate call (below) a visual "here's exactly what I mean"
#' reference alongside the reference points' names, rather than relying
#' on text description alone.
annotate_template_with_refpoints <- function(template_img, reference_points) {
  info <- magick::image_info(template_img)
  w <- info$width[1]; h <- info$height[1]

  img <- magick::image_draw(template_img)
  for (i in seq_len(nrow(reference_points))) {
    r <- reference_points[i, ]
    x0 <- r$x * w; y0 <- r$y * h
    x1 <- (r$x + r$w) * w; y1 <- (r$y + r$h) * h
    graphics::rect(x0, y0, x1, y1, border = "red", lwd = 4)
    graphics::text(x0, max(0, y0 - 8), labels = r$name, col = "red", cex = 1.6, adj = c(0, 1), font = 2)
  }
  grDevices::dev.off()
  img
}

#' Draw a labeled reference grid (spreadsheet-style: columns A,B,C...,
#' rows 1,2,3...) directly onto a COPY of an image, same drawing
#' technique as annotate_template_with_refpoints() above.
#'
#' Why: asking a vision model for a precise continuous fraction (e.g.
#' "X: 0.73") is a known weak spot — confirmed in practice here (see
#' locate_reference_points_holistic()'s history: format parsing got
#' fixed, but coordinate accuracy didn't improve, including an
#' out-of-range value that pointed at a genuine estimation error, not
#' just a formatting one). Asking "which labeled cell is this in"
#' instead is a much easier, more reliable task — closer to
#' classification than continuous estimation — at the cost of coarser
#' precision, which is an acceptable trade since the placement panel's
#' drag-to-correct exists specifically to refine a starting point, not
#' to be bypassed by it.
draw_coordinate_grid <- function(img, n_cols = EXTRACTION_REF_GRID_COLS, n_rows = EXTRACTION_REF_GRID_ROWS) {
  info <- magick::image_info(img)
  w <- info$width[1]; h <- info$height[1]
  cell_w <- w / n_cols
  cell_h <- h / n_rows

  out <- magick::image_draw(img)
  grid_col <- grDevices::rgb(0, 0.4, 1, 0.55)
  for (c in 0:n_cols) {
    x <- c * cell_w
    graphics::lines(c(x, x), c(0, h), col = grid_col, lwd = 1)
  }
  for (r in 0:n_rows) {
    y <- r * cell_h
    graphics::lines(c(0, w), c(y, y), col = grid_col, lwd = 1)
  }
  col_labels <- LETTERS[seq_len(n_cols)]
  label_cex <- max(0.7, min(1.3, w / (n_cols * 90)))
  for (c in seq_len(n_cols)) {
    cx <- (c - 0.5) * cell_w
    graphics::text(cx, cell_h * 0.28, labels = col_labels[c], col = "blue", cex = label_cex, font = 2)
  }
  for (r in seq_len(n_rows)) {
    ry <- (r - 0.5) * cell_h
    graphics::text(cell_w * 0.12, ry, labels = as.character(r), col = "blue", cex = label_cex, font = 2, adj = c(0, 0.5))
  }
  grDevices::dev.off()
  out
}

#' Convert a grid cell label (e.g. "E7", from draw_coordinate_grid())
#' back to an approximate fractional (0-1) position — the CENTER of
#' that cell, which is as precise as a cell reference can be by
#' construction; refining beyond that is what the placement panel's
#' drag-to-correct is for.
#'
#' @return list(x, y) fractions, or NULL if the label doesn't parse or
#'   is out of the grid's actual range
grid_cell_to_fraction <- function(cell_label, n_cols = EXTRACTION_REF_GRID_COLS, n_rows = EXTRACTION_REF_GRID_ROWS) {
  m <- regmatches(trimws(cell_label), regexec("^([A-Za-z]+)(\\d+)$", trimws(cell_label)))[[1]]
  if (length(m) < 3) return(NULL)
  col_idx <- match(toupper(m[2]), LETTERS)
  row_num <- suppressWarnings(as.integer(m[3]))
  if (is.na(col_idx) || col_idx > n_cols || is.na(row_num) || row_num < 1 || row_num > n_rows) return(NULL)
  list(x = (col_idx - 0.5) / n_cols, y = (row_num - 0.5) / n_rows)
}

#' Locate ALL of a template's reference points in a submission with a
#' SINGLE API call, sending both the (annotated) template and the
#' submission together — rather than asking N isolated "find this text"
#' questions, this lets the model use full visual context: it can see
#' exactly what each reference point looks like (from the template) and
#' cross-reference that against the whole submission at once, the way a
#' person visually aligning two documents would. This is a candidate
#' REPLACEMENT for the per-point locate_text_via_api() approach, not
#' layered on top of it — used as the source for the "Auto-suggest via
#' AI" button in the pre-extraction placement panel (mod_extraction.R),
#' feeding into the same manual-review/correction flow either way, since
#' that flow is what's actually confirmed reliable so far.
#'
#' @param api_key Anthropic API key
#' @param template_img The template's own reference image
#' @param reference_points data.frame(ref_id, name, x, y, w, h, ...)
#'   from the template, in the SAME row order this function returns
#'   results in
#' @param submission_img The submission image to locate points in
#' @return A list, one entry per row of reference_points (same order),
#'   each list(x, y, confidence, error) — x,y are PIXEL coordinates in
#'   submission_img's own frame, or NA if not found/errored
locate_reference_points_holistic <- function(api_key, template_img, reference_points, submission_img) {
  n <- nrow(reference_points)
  na_result <- function(err = NULL) list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = err)
  if (n == 0) return(list())

  message(sprintf("---- Auto-suggest (holistic): starting for %d reference point(s): %s ----",
                   n, paste(reference_points$name, collapse = ", ")))

  annotated <- tryCatch(annotate_template_with_refpoints(template_img, reference_points), error = function(e) {
    message("Auto-suggest: annotate_template_with_refpoints() failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(annotated)) {
    return(rep(list(na_result("Couldn't annotate template image")), n))
  }
  ann_info <- magick::image_info(annotated)
  message(sprintf("Auto-suggest: annotated template OK, %dx%d", ann_info$width[1], ann_info$height[1]))

  downscale <- function(img) {
    info <- magick::image_info(img)
    iw <- info$width[1]; ih <- info$height[1]
    if (max(iw, ih) <= EXTRACTION_REF_API_MAX_DIM) return(img)
    scale <- EXTRACTION_REF_API_MAX_DIM / max(iw, ih)
    magick::image_resize(img, sprintf("%dx%d!", round(iw * scale), round(ih * scale)))
  }
  annotated_small <- downscale(annotated)
  submission_small <- downscale(submission_img)

  # Grid overlay on the (downscaled) submission — see draw_coordinate_grid()'s
  # doc comment for why: asking for a cell label is far more reliable
  # than asking for a continuous fraction, which testing showed the
  # model getting genuinely wrong (not just mis-formatted).
  submission_gridded <- tryCatch(draw_coordinate_grid(submission_small), error = function(e) {
    message("Auto-suggest: draw_coordinate_grid() failed, falling back to ungridded submission: ", conditionMessage(e))
    submission_small
  })

  sub_info <- magick::image_info(submission_img)
  sub_w <- sub_info$width[1]; sub_h <- sub_info$height[1]
  small_tmpl_info <- magick::image_info(annotated_small)
  small_sub_info <- magick::image_info(submission_gridded)
  message(sprintf("Auto-suggest: sending template %dx%d, submission (gridded, %dx%d cells) %dx%d (submission's own real size is %dx%d)",
                   small_tmpl_info$width[1], small_tmpl_info$height[1],
                   EXTRACTION_REF_GRID_COLS, EXTRACTION_REF_GRID_ROWS,
                   small_sub_info$width[1], small_sub_info$height[1], sub_w, sub_h))

  to_b64 <- function(img) {
    tmp <- tempfile(fileext = ".png")
    magick::image_write(img, tmp, format = "png")
    b64 <- tryCatch(base64enc::base64encode(tmp), error = function(e) NA_character_)
    unlink(tmp)
    b64
  }
  tmpl_b64 <- to_b64(annotated_small)
  sub_b64 <- to_b64(submission_gridded)
  if (is.na(tmpl_b64) || is.na(sub_b64)) {
    message("Auto-suggest: base64-encoding one or both images failed.")
    return(rep(list(na_result("Couldn't encode images for the API call")), n))
  }

  point_list <- paste(sprintf("%d. %s", seq_len(n), reference_points$name), collapse = "\n")

  prompt <- sprintf(paste(
    "The FIRST image is a BLANK TEMPLATE form, with %d reference points marked",
    "with red boxes and red labels:",
    "%s",
    "",
    "The SECOND image is a SUBMITTED (filled-in) version of the SAME form layout —",
    "it may be at a different scale, slightly rotated, or cropped differently than",
    "the template, and it has handwritten/filled content the template doesn't. It",
    "has a blue reference grid drawn on it: %d columns labeled A-%s (left to right)",
    "and %d rows labeled 1-%d (top to bottom).",
    "",
    "For EACH reference point (in the SAME ORDER as listed above), find where the",
    "same landmark appears in the SECOND (submitted) image — use the RED BOX in the",
    "template to see exactly what to look for, then identify WHICH GRID CELL that",
    "location falls in on the submitted form (e.g. \"E7\" means column E, row 7).",
    "",
    "Respond with EXACTLY %d lines, nothing else — one line per reference point, in",
    "the SAME ORDER as listed above, each in this exact pipe-separated format:",
    "STATUS|CONFIDENCE|CELL",
    "STATUS must be the single word LOCATED if you found it, or NOTFOUND if you",
    "could not (do not write anything else there — not \"YES\", not \"FOUND\", exactly",
    "LOCATED or NOTFOUND). CONFIDENCE must be the single word HIGH or LOW. CELL is",
    "the grid cell letter+number (e.g. E7) where that point falls in the SECOND",
    "(submitted) image — read it directly off the blue grid labels, do not estimate",
    "a fraction. If STATUS is NOTFOUND, still fill in CELL as: A1",
    sep = "\n"
  ), n, point_list, EXTRACTION_REF_GRID_COLS, LETTERS[EXTRACTION_REF_GRID_COLS],
     EXTRACTION_REF_GRID_ROWS, EXTRACTION_REF_GRID_ROWS, n)

  body <- list(
    model = EXTRACTION_MODEL, max_tokens = 500,
    messages = list(list(role = "user", content = list(
      list(type = "text", text = "TEMPLATE (reference points marked in red):"),
      list(type = "image", source = list(type = "base64", media_type = "image/png", data = tmpl_b64)),
      list(type = "text", text = "SUBMITTED FORM (find the corresponding points in this one):"),
      list(type = "image", source = list(type = "base64", media_type = "image/png", data = sub_b64)),
      list(type = "text", text = prompt)
    )))
  )

  attempt <- function() {
    httr2::request(ANTHROPIC_API_URL) |>
      httr2::req_headers("x-api-key" = api_key, "anthropic-version" = ANTHROPIC_API_VERSION,
                          "content-type" = "application/json") |>
      httr2::req_body_json(body) |> httr2::req_timeout(90) |>
      httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
  }

  resp <- tryCatch(attempt(), error = function(e) e)
  if (inherits(resp, "error")) {
    message("Auto-suggest: connection failed: ", conditionMessage(resp))
    return(rep(list(na_result(paste("Connection failed:", conditionMessage(resp)))), n))
  }
  status <- httr2::resp_status(resp)
  message("Auto-suggest: API HTTP status ", status)
  if (status >= 400) {
    err_body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    err_msg <- if (!is.null(err_body$error$message)) err_body$error$message else sprintf("HTTP %d", status)
    message("Auto-suggest: API error: ", err_msg)
    return(rep(list(na_result(sprintf("API error (HTTP %d): %s", status, err_msg))), n))
  }
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$content)) {
    message("Auto-suggest: response had no parseable content.")
    return(rep(list(na_result("Unexpected API response shape")), n))
  }
  text_blocks <- Filter(function(b) identical(b$type, "text"), parsed$content)
  raw_text <- if (length(text_blocks) > 0) trimws(text_blocks[[1]]$text) else ""

  message("---- Auto-suggest: RAW response text (exactly what the model returned) ----")
  message(raw_text)
  message("---- Auto-suggest: end raw response ----")
  if (!is.null(parsed$stop_reason)) {
    message("Auto-suggest: stop_reason = ", parsed$stop_reason,
            if (identical(parsed$stop_reason, "max_tokens")) " <-- response was CUT OFF before finishing; try a smaller reference point count or check if max_tokens needs raising" else "")
  }

  lines <- trimws(strsplit(raw_text, "\n")[[1]])
  lines <- lines[nzchar(lines)]
  message(sprintf("Auto-suggest: got %d non-empty line(s), expected %d (one per reference point)", length(lines), n))

  results <- vector("list", n)
  for (i in seq_len(n)) {
    pt_name <- reference_points$name[i]
    if (i > length(lines)) {
      message(sprintf("Auto-suggest: [%s] no line %d in the response — treating as not found.", pt_name, i))
      results[[i]] <- na_result("Response had fewer lines than reference points")
      next
    }
    parts <- trimws(strsplit(lines[i], "\\|")[[1]])
    if (length(parts) < 3) {
      message(sprintf("Auto-suggest: [%s] line %d ('%s') doesn't have 3 pipe-separated parts — got %d.",
                       pt_name, i, lines[i], length(parts)))
      results[[i]] <- na_result(sprintf("Couldn't parse line %d of the response", i))
      next
    }
    # Accept the intended NOTFOUND/anything-else-negative as "not found";
    # accept LOCATED and a couple of plausible near-misses (models don't
    # always follow a vocabulary exactly even when told to) as "found" —
    # deliberately lenient here since a strict match already failed once
    # in practice (the model wrote "FOUND" instead of the expected "YES").
    status_word <- toupper(parts[1])
    found <- status_word %in% c("LOCATED", "FOUND", "YES")
    confidence <- toupper(parts[2])
    cell_label <- parts[3]

    frac <- if (found) grid_cell_to_fraction(cell_label) else NULL
    if (found && is.null(frac)) {
      message(sprintf("Auto-suggest: [%s] line %d says LOCATED but cell '%s' doesn't parse as a valid grid reference (expected A-%s, 1-%d) — treating as not found.",
                       pt_name, i, cell_label, LETTERS[EXTRACTION_REF_GRID_COLS], EXTRACTION_REF_GRID_ROWS))
    }

    if (!found || is.null(frac)) {
      message(sprintf("Auto-suggest: [%s] line %d parsed as NOT FOUND (status='%s', confidence=%s, cell='%s')",
                       pt_name, i, parts[1], confidence, cell_label))
      results[[i]] <- list(x = NA_real_, y = NA_real_, confidence = confidence, error = NULL)
    } else {
      message(sprintf("Auto-suggest: [%s] line %d parsed OK — confidence=%s, cell=%s -> fraction=(%.3f, %.3f) -> pixels=(%.1f, %.1f) of %dx%d submission",
                       pt_name, i, confidence, toupper(cell_label), frac$x, frac$y, frac$x * sub_w, frac$y * sub_h, sub_w, sub_h))
      results[[i]] <- list(x = frac$x * sub_w, y = frac$y * sub_h, confidence = confidence, error = NULL)
    }
  }
  n_found <- sum(vapply(results, function(r) !is.na(r$x), logical(1)))
  message(sprintf("---- Auto-suggest: done — %d of %d point(s) located ----", n_found, n))
  results
}

#' Refine a single reference point's location within a small window of
#' the FULL-RESOLUTION submission, given a starting guess (from the
#' coarse whole-page pass, or the geometric default). This is pass 2 of
#' locate_reference_points_two_pass() — a small crop at full resolution
#' plus a finer grid gives the model both more actual pixel detail and
#' an easier (smaller-range) question than the whole-page pass could.
#'
#' @param center_x_frac,center_y_frac Fractions (0-1) of the FULL
#'   submission — the refine window is centered here
#' @return list(x, y, confidence, error) — x,y in FULL submission pixel
#'   coordinates (already converted back from the crop's own frame)
refine_reference_point <- function(api_key, template_img, ref_point, submission_img,
                                    center_x_frac, center_y_frac) {
  sub_info <- magick::image_info(submission_img)
  sub_w <- sub_info$width[1]; sub_h <- sub_info$height[1]

  half <- EXTRACTION_REF_REFINE_WINDOW_FRAC / 2
  window_x0 <- max(0, center_x_frac - half); window_x1 <- min(1, center_x_frac + half)
  window_y0 <- max(0, center_y_frac - half); window_y1 <- min(1, center_y_frac + half)

  crop_x0 <- round(window_x0 * sub_w); crop_y0 <- round(window_y0 * sub_h)
  crop_w <- max(10, round((window_x1 - window_x0) * sub_w))
  crop_h <- max(10, round((window_y1 - window_y0) * sub_h))

  sub_crop <- tryCatch(
    magick::image_crop(submission_img, sprintf("%dx%d+%d+%d", crop_w, crop_h, crop_x0, crop_y0)),
    error = function(e) NULL
  )
  if (is.null(sub_crop)) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Couldn't crop refine window"))
  }
  sub_crop_gridded <- tryCatch(
    draw_coordinate_grid(sub_crop, n_cols = EXTRACTION_REF_REFINE_GRID_COLS, n_rows = EXTRACTION_REF_REFINE_GRID_ROWS),
    error = function(e) sub_crop
  )

  # Small template patch for visual reference — same landmark, cropped
  # tight with a bit of padding for context, rather than shown on the
  # whole annotated page (that was pass 1's job).
  tmpl_info <- magick::image_info(template_img)
  tmpl_w <- tmpl_info$width[1]; tmpl_h <- tmpl_info$height[1]
  pad <- 0.6
  patch_x0 <- max(0, round((ref_point$x - ref_point$w * pad) * tmpl_w))
  patch_y0 <- max(0, round((ref_point$y - ref_point$h * pad) * tmpl_h))
  patch_x1 <- min(tmpl_w, round((ref_point$x + ref_point$w * (1 + pad)) * tmpl_w))
  patch_y1 <- min(tmpl_h, round((ref_point$y + ref_point$h * (1 + pad)) * tmpl_h))
  tmpl_patch <- tryCatch(
    magick::image_crop(template_img, sprintf("%dx%d+%d+%d", patch_x1 - patch_x0, patch_y1 - patch_y0, patch_x0, patch_y0)),
    error = function(e) NULL
  )
  if (is.null(tmpl_patch)) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Couldn't crop template reference patch"))
  }

  to_b64 <- function(img) {
    tmp <- tempfile(fileext = ".png")
    magick::image_write(img, tmp, format = "png")
    b64 <- tryCatch(base64enc::base64encode(tmp), error = function(e) NA_character_)
    unlink(tmp)
    b64
  }
  tmpl_b64 <- to_b64(tmpl_patch)
  sub_b64 <- to_b64(sub_crop_gridded)
  if (is.na(tmpl_b64) || is.na(sub_b64)) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Couldn't encode refine-pass images"))
  }

  anchor_desc <- if ("anchor_text" %in% names(ref_point) && nzchar(trimws(ref_point$anchor_text))) {
    sprintf('It contains the printed text "%s".', trimws(ref_point$anchor_text))
  } else {
    ""
  }

  prompt <- sprintf(paste(
    "The FIRST image is a close-up crop from a BLANK TEMPLATE form, showing a",
    "landmark called \"%s\". %s",
    "",
    "The SECOND image is a small cropped region from a SUBMITTED (filled-in)",
    "version of the same form, zoomed in on roughly where that landmark should",
    "be. It has a fine grid overlaid: %d columns labeled A-%s (left to right) and",
    "%d rows labeled 1-%d (top to bottom).",
    "",
    "Find the SAME landmark in the SECOND image and identify which grid cell it",
    "falls in. It might not be perfectly centered in this crop — that's expected,",
    "this is a rough starting guess being refined.",
    "",
    "Respond with EXACTLY ONE line, nothing else, in this format:",
    "STATUS|CONFIDENCE|CELL",
    "STATUS is the single word LOCATED or NOTFOUND. CONFIDENCE is HIGH or LOW.",
    "CELL is the grid cell (e.g. E7), read directly off the grid labels, not",
    "estimated. If NOTFOUND, still write CELL as A1.",
    sep = "\n"
  ), ref_point$name, anchor_desc,
     EXTRACTION_REF_REFINE_GRID_COLS, LETTERS[EXTRACTION_REF_REFINE_GRID_COLS],
     EXTRACTION_REF_REFINE_GRID_ROWS, EXTRACTION_REF_REFINE_GRID_ROWS)

  body <- list(
    model = EXTRACTION_MODEL, max_tokens = 100,
    messages = list(list(role = "user", content = list(
      list(type = "text", text = "TEMPLATE close-up:"),
      list(type = "image", source = list(type = "base64", media_type = "image/png", data = tmpl_b64)),
      list(type = "text", text = "SUBMITTED FORM close-up (gridded):"),
      list(type = "image", source = list(type = "base64", media_type = "image/png", data = sub_b64)),
      list(type = "text", text = prompt)
    )))
  )

  attempt <- function() {
    httr2::request(ANTHROPIC_API_URL) |>
      httr2::req_headers("x-api-key" = api_key, "anthropic-version" = ANTHROPIC_API_VERSION,
                          "content-type" = "application/json") |>
      httr2::req_body_json(body) |> httr2::req_timeout(60) |>
      httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
  }

  resp <- tryCatch(attempt(), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN",
                error = paste("Connection failed:", conditionMessage(resp))))
  }
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    err_body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    err_msg <- if (!is.null(err_body$error$message)) err_body$error$message else sprintf("HTTP %d", status)
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN",
                error = sprintf("API error (HTTP %d): %s", status, err_msg)))
  }
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$content)) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Unexpected API response shape"))
  }
  text_blocks <- Filter(function(b) identical(b$type, "text"), parsed$content)
  raw_text <- if (length(text_blocks) > 0) trimws(text_blocks[[1]]$text) else ""
  message(sprintf("Refine [%s]: raw response = '%s'", ref_point$name, raw_text))

  ln <- trimws(strsplit(raw_text, "\n")[[1]])
  ln <- ln[nzchar(ln)]
  if (length(ln) == 0) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Empty response"))
  }
  parts <- trimws(strsplit(ln[1], "\\|")[[1]])
  if (length(parts) < 3) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Couldn't parse refine response"))
  }
  found <- toupper(parts[1]) %in% c("LOCATED", "FOUND", "YES")
  confidence <- toupper(parts[2])
  cell_frac <- if (found) grid_cell_to_fraction(parts[3], n_cols = EXTRACTION_REF_REFINE_GRID_COLS,
                                                 n_rows = EXTRACTION_REF_REFINE_GRID_ROWS) else NULL

  if (!found || is.null(cell_frac)) {
    return(list(x = NA_real_, y = NA_real_, confidence = confidence, error = NULL))
  }

  # Crop-local fraction -> full-submission fraction -> pixels.
  full_x_frac <- window_x0 + cell_frac$x * (window_x1 - window_x0)
  full_y_frac <- window_y0 + cell_frac$y * (window_y1 - window_y0)
  list(x = full_x_frac * sub_w, y = full_y_frac * sub_h, confidence = confidence, error = NULL)
}

#' Two-pass reference point location: coarse whole-page grid classification
#' (locate_reference_points_holistic()) to get a rough starting guess,
#' then a focused, full-resolution, finer-grid refine pass
#' (refine_reference_point()) per point. This is the primary automatic
#' alignment method — the goal is full automation with manual placement
#' as a backup, not the other way around.
#'
#' Cross-check: for each point, if the coarse pass found it with HIGH
#' confidence, the refine window centers on that guess. Otherwise (LOW
#' confidence or not found at all) it centers on the geometric default
#' — the same relative position the point has on the template — since
#' testing showed that's often already a reasonable starting point on
#' its own, and it's certainly better than centering on a guess the
#' coarse pass itself wasn't confident about.
#'
#' @return A list, one entry per row of reference_points (same order),
#'   each list(x, y, confidence, error) in submission_img's pixel frame
locate_reference_points_two_pass <- function(api_key, template_img, reference_points, submission_img) {
  n <- nrow(reference_points)
  if (n == 0) return(list())

  coarse_results <- locate_reference_points_holistic(api_key, template_img, reference_points, submission_img)

  sub_info <- magick::image_info(submission_img)
  sub_w <- sub_info$width[1]; sub_h <- sub_info$height[1]

  message(sprintf("---- Two-pass: starting refine pass for %d point(s) ----", n))

  final_results <- vector("list", n)
  for (i in seq_len(n)) {
    r <- reference_points[i, ]
    coarse <- coarse_results[[i]]

    default_x_frac <- r$x + r$w / 2
    default_y_frac <- r$y + r$h / 2

    use_coarse <- !is.na(coarse$x) && identical(coarse$confidence, "HIGH")
    if (use_coarse) {
      center_x_frac <- coarse$x / sub_w
      center_y_frac <- coarse$y / sub_h
      center_source <- "coarse pass (HIGH confidence)"
    } else {
      center_x_frac <- default_x_frac
      center_y_frac <- default_y_frac
      center_source <- if (!is.na(coarse$x)) "geometric default (coarse pass was LOW confidence)" else "geometric default (coarse pass didn't find it)"
    }
    message(sprintf("Two-pass: [%s] refine window centered via %s -> (%.3f, %.3f)",
                     r$name, center_source, center_x_frac, center_y_frac))

    refined <- tryCatch(
      refine_reference_point(api_key, template_img, r, submission_img, center_x_frac, center_y_frac),
      error = function(e) {
        message(sprintf("Two-pass: [%s] refine call errored: %s", r$name, conditionMessage(e)))
        list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = conditionMessage(e))
      }
    )

    if (!is.na(refined$x) && !is.na(refined$y)) {
      final_results[[i]] <- refined
    } else {
      # Refine pass failed or didn't confirm it — fall back to the
      # window-center guess itself rather than nothing, so there's
      # always SOMETHING placed for full automation to hand off cleanly
      # to the review panel (marked LOW so it's visibly flagged).
      message(sprintf("Two-pass: [%s] refine pass didn't confirm a location — using the window-center guess, flagged LOW confidence.", r$name))
      final_results[[i]] <- list(x = center_x_frac * sub_w, y = center_y_frac * sub_h,
                                  confidence = "LOW", error = NULL)
    }
  }
  message("---- Two-pass: done ----")
  final_results
}

#' Convert a template-normalized (x,y) position into a rough, human-
#' readable page-region phrase (e.g. "the top-left area of the page").
#' Used to give locate_text_via_api() a spatial hint alongside the text
#' itself — a busy/repetitive form (a tally sheet full of similar-
#' looking cells) can have text that isn't uniquely identifiable by
#' content alone; "find this text, and it should be roughly here" is a
#' meaningfully easier search than "find this text" with no constraint
#' on where to look.
describe_region <- function(x, y) {
  h <- if (x < 1 / 3) "left" else if (x < 2 / 3) "center" else "right"
  v <- if (y < 1 / 3) "top" else if (y < 2 / 3) "middle" else "bottom"
  if (h == "center" && v == "middle") return("the CENTER of the page")
  if (v == "middle") return(sprintf("the vertically-centered, horizontally-%s area of the page", h))
  if (h == "center") return(sprintf("the horizontally-centered, vertically-%s area of the page", v))
  sprintf("the %s-%s area of the page", v, h)
}

#' Locate a piece of known text somewhere in a (possibly large) form
#' image, via the Claude API. This is the alignment mechanism itself —
#' it replaced an earlier from-scratch pixel cross-correlation approach
#' that went through five rounds of debugging without becoming
#' reliable. Asking a model that can read to find text it can read is a
#' much better-suited approach than asking pixel correlation (or a
#' vision model doing improvised patch-matching) to do the same job.
#'
#' @param api_key Anthropic API key
#' @param haystack_img The (aligned-so-far, i.e. not yet warped)
#'   submission image to search
#' @param anchor_text The exact text to look for, from the reference
#'   point's stored anchor_text (captured at annotation time)
#' @param region_hint Optional, from describe_region() — a rough page-
#'   region phrase to narrow the search, since text content alone can
#'   be ambiguous on a repetitive form. NULL skips the hint.
#' @return list(x, y, confidence, error) — x,y are PIXEL coordinates in
#'   haystack_img's own frame (converted back from the model's
#'   fractional 0-1 response), confidence "HIGH"/"LOW", x/y NA if not
#'   found or on error (check `error` to tell those apart — NA with
#'   error=NULL means "legitimately not found on this form," not a failure)
locate_text_via_api <- function(api_key, haystack_img, anchor_text, region_hint = NULL) {
  info <- magick::image_info(haystack_img)
  img_w <- info$width[1]; img_h <- info$height[1]

  # Downscale for the API call if large — full-resolution multi-
  # megapixel form images cost more tokens/time than needed to locate
  # text, and the result (a fraction of image width/height) is
  # resolution-independent, so this doesn't cost precision.
  search_img <- if (max(img_w, img_h) > EXTRACTION_REF_API_MAX_DIM) {
    scale <- EXTRACTION_REF_API_MAX_DIM / max(img_w, img_h)
    magick::image_resize(haystack_img, sprintf("%dx%d!", round(img_w * scale), round(img_h * scale)))
  } else {
    haystack_img
  }

  tmp <- tempfile(fileext = ".png")
  magick::image_write(search_img, tmp, format = "png")
  img_b64 <- tryCatch(base64enc::base64encode(tmp), error = function(e) NA_character_)
  unlink(tmp)
  if (is.na(img_b64)) return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Couldn't encode search image"))

  region_line <- if (!is.null(region_hint)) {
    sprintf("This text should be located in roughly %s — use that as a strong hint for WHERE to look, especially if similar text might appear elsewhere on the form (e.g. a repeated column header or label).",
            region_hint)
  } else {
    ""
  }

  prompt <- sprintf(paste(
    "This image is a scanned paper form. Find this exact text on the form: \"%s\"",
    "",
    "%s",
    "",
    "Respond in exactly this four-line format, nothing else:",
    "FOUND: YES or NO",
    "CONFIDENCE: HIGH or LOW",
    "X: <fraction from 0 to 1, horizontal position of the CENTER of that text>",
    "Y: <fraction from 0 to 1, vertical position of the CENTER of that text>",
    "",
    "Use LOW confidence if the text is small, faint, ambiguous, or you're",
    "not fully sure of the exact position. If the text appears more than",
    "once, use the occurrence closest to the region described above.",
    "If you cannot find this text anywhere on the form, respond FOUND: NO,",
    "CONFIDENCE: LOW, X: 0, Y: 0.",
    sep = "\n"
  ), anchor_text, region_line)

  body <- list(
    model = EXTRACTION_MODEL, max_tokens = 100,
    messages = list(list(role = "user", content = list(
      list(type = "image", source = list(type = "base64", media_type = "image/png", data = img_b64)),
      list(type = "text", text = prompt)
    )))
  )

  attempt <- function() {
    httr2::request(ANTHROPIC_API_URL) |>
      httr2::req_headers("x-api-key" = api_key, "anthropic-version" = ANTHROPIC_API_VERSION,
                          "content-type" = "application/json") |>
      httr2::req_body_json(body) |> httr2::req_timeout(60) |>
      httr2::req_error(is_error = function(resp) FALSE) |> httr2::req_perform()
  }

  resp <- tryCatch(attempt(), error = function(e) e)
  if (inherits(resp, "error")) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN",
                error = paste("Connection failed:", conditionMessage(resp))))
  }

  status <- httr2::resp_status(resp)
  if (status >= 400) {
    err_body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
    err_msg <- if (!is.null(err_body$error$message)) err_body$error$message else sprintf("HTTP %d", status)
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN",
                error = sprintf("API error (HTTP %d): %s", status, err_msg)))
  }

  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$content)) {
    return(list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = "Unexpected API response shape"))
  }
  text_blocks <- Filter(function(b) identical(b$type, "text"), parsed$content)
  raw_text <- if (length(text_blocks) > 0) trimws(text_blocks[[1]]$text) else ""

  found_m <- regmatches(raw_text, regexec("FOUND:\\s*(YES|NO)", raw_text, ignore.case = TRUE))[[1]]
  conf_m  <- regmatches(raw_text, regexec("CONFIDENCE:\\s*(HIGH|LOW)", raw_text, ignore.case = TRUE))[[1]]
  x_m <- regmatches(raw_text, regexec("X:\\s*([0-9.]+)", raw_text))[[1]]
  y_m <- regmatches(raw_text, regexec("Y:\\s*([0-9.]+)", raw_text))[[1]]

  confidence <- if (length(conf_m) >= 2) toupper(conf_m[2]) else "UNKNOWN"
  found <- length(found_m) >= 2 && toupper(found_m[2]) == "YES"

  if (!found || length(x_m) < 2 || length(y_m) < 2) {
    return(list(x = NA_real_, y = NA_real_, confidence = confidence, error = NULL))  # legitimately not found
  }

  frac_x <- suppressWarnings(as.numeric(x_m[2]))
  frac_y <- suppressWarnings(as.numeric(y_m[2]))
  if (is.na(frac_x) || is.na(frac_y)) {
    return(list(x = NA_real_, y = NA_real_, confidence = confidence, error = "Couldn't parse coordinates from API response"))
  }

  # Convert from the (possibly downscaled) search image's fractional
  # position back to haystack_img's own real pixel coordinates.
  list(x = frac_x * img_w, y = frac_y * img_h, confidence = confidence, error = NULL)
}

#' Sanity-check a point set before warping — rejects clearly degenerate
#' configurations (points collapsed close together, or nearly
#' collinear) that would make image_distort() produce a wild, unusable
#' Fit a coordinate transform (TEMPLATE pixel coords -> SUBMISSION pixel
#' coords) from 2-4 point correspondences — this is the reference-point
#' alignment mechanism itself. Dispatches by point count:
#'   2 points -> similarity (translate + rotate + uniform scale)
#'   3 points -> affine (+ shear)
#'   4 points -> full perspective/homography (true keystone correction)
#'
#' Deliberately fits and returns coordinate math only — nothing here
#' touches image pixels. Every field gets cropped straight from the
#' original, untouched submission image by mapping its box through the
#' returned function; there's no intermediate "aligned image" at all.
#' This is also why a degenerate point set fails LOUDLY: fitting from
#' clustered or nearly-collinear points makes the underlying linear
#' system singular, and solve() throws rather than silently returning
#' something usable-looking — a real error to catch, not a wild-looking
#' image with no explanation (which is what warping a whole page from a
#' bad point set produced instead, before this rewrite).
#'
#' @param point_pairs list of list(sub_x, sub_y, tmpl_x, tmpl_y) — 2-4 entries
#' @return A function(tmpl_x, tmpl_y) -> list(x, y) in submission pixel
#'   space, or NULL if fewer than 2 points were given or the fit failed
#'   (singular system / degenerate points)
fit_point_transform <- function(point_pairs) {
  if (length(point_pairs) < 2) return(NULL)
  if (length(point_pairs) > 4) point_pairs <- point_pairs[1:4]  # perspective wants exactly 4

  n <- length(point_pairs)
  fitted <- tryCatch({
    if (n == 2) fit_similarity_transform(point_pairs)
    else if (n == 3) fit_affine_transform(point_pairs)
    else fit_homography_transform(point_pairs)
  }, error = function(e) {
    message(sprintf("fit_point_transform(): fitting failed with %d point(s) — %s. Points are likely too clustered together or nearly collinear for a stable %s fit.",
                     n, conditionMessage(e),
                     switch(as.character(n), "2" = "similarity", "3" = "affine", "perspective")))
    NULL
  })
  fitted
}

#' Similarity transform (2 points): translate + rotate + uniform scale.
#' Solves the linear system for (a, b, tx, ty) in
#'   sub_x = a*tmpl_x - b*tmpl_y + tx
#'   sub_y = b*tmpl_x + a*tmpl_y + ty
#' directly in the template->submission direction (a = scale*cos(theta),
#' b = scale*sin(theta)), so no separate inversion step is needed.
fit_similarity_transform <- function(pairs) {
  p1 <- pairs[[1]]; p2 <- pairs[[2]]
  A <- matrix(c(
    p1$tmpl_x, -p1$tmpl_y, 1, 0,
    p1$tmpl_y,  p1$tmpl_x, 0, 1,
    p2$tmpl_x, -p2$tmpl_y, 1, 0,
    p2$tmpl_y,  p2$tmpl_x, 0, 1
  ), nrow = 4, byrow = TRUE)
  b <- c(p1$sub_x, p1$sub_y, p2$sub_x, p2$sub_y)
  params <- solve(A, b)  # throws if the 2 points coincide (singular A)
  a <- params[1]; bb <- params[2]; tx <- params[3]; ty <- params[4]
  function(tmpl_x, tmpl_y) list(x = a * tmpl_x - bb * tmpl_y + tx, y = bb * tmpl_x + a * tmpl_y + ty)
}

#' Affine transform (3 points): translate + rotate + scale + shear.
#' Solves the 6x6 linear system for (a,b,c,d,e,f) in
#'   sub_x = a*tmpl_x + b*tmpl_y + c
#'   sub_y = d*tmpl_x + e*tmpl_y + f
#' directly in the template->submission direction.
fit_affine_transform <- function(pairs) {
  A <- matrix(0, nrow = 6, ncol = 6)
  b <- numeric(6)
  for (i in 1:3) {
    p <- pairs[[i]]
    A[2 * i - 1, ] <- c(p$tmpl_x, p$tmpl_y, 1, 0, 0, 0)
    A[2 * i, ]     <- c(0, 0, 0, p$tmpl_x, p$tmpl_y, 1)
    b[2 * i - 1] <- p$sub_x
    b[2 * i] <- p$sub_y
  }
  params <- solve(A, b)  # throws if the 3 points are collinear (singular A)
  a <- params[1]; bb <- params[2]; c <- params[3]; d <- params[4]; e <- params[5]; f <- params[6]
  function(tmpl_x, tmpl_y) list(x = a * tmpl_x + bb * tmpl_y + c, y = d * tmpl_x + e * tmpl_y + f)
}

#' Perspective/homography transform (4 points): true keystone correction
#' — handles a photo taken at an angle, not just offset/rotated. Direct
#' Linear Transform (DLT): solves the 8x8 system for h1..h8 (h9 fixed to
#' 1) in the standard homography form
#'   sub_x = (h1*tmpl_x + h2*tmpl_y + h3) / (h7*tmpl_x + h8*tmpl_y + 1)
#'   sub_y = (h4*tmpl_x + h5*tmpl_y + h6) / (h7*tmpl_x + h8*tmpl_y + 1)
#' fit directly in the template->submission direction, same as the
#' other two — no separate inversion needed.
fit_homography_transform <- function(pairs) {
  A <- matrix(0, nrow = 8, ncol = 8)
  b <- numeric(8)
  for (i in 1:4) {
    p <- pairs[[i]]
    x <- p$tmpl_x; y <- p$tmpl_y; xp <- p$sub_x; yp <- p$sub_y
    A[2 * i - 1, ] <- c(x, y, 1, 0, 0, 0, -x * xp, -y * xp)
    A[2 * i, ]     <- c(0, 0, 0, x, y, 1, -x * yp, -y * yp)
    b[2 * i - 1] <- xp
    b[2 * i] <- yp
  }
  h <- solve(A, b)  # throws if the 4 points are degenerate (e.g. 3+ collinear, or a repeated point)
  H <- matrix(c(h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8], 1), nrow = 3, byrow = TRUE)
  function(tmpl_x, tmpl_y) {
    v <- H %*% c(tmpl_x, tmpl_y, 1)
    list(x = v[1] / v[3], y = v[2] / v[3])
  }
}

#' Locate every reference point on a submission via locate_text_via_api()
#' (one API call each) and fit a coordinate transform from the located
#' points, via fit_point_transform() — no image warping. Every point
#' that was located AT ALL gets used — EXTRACTION_REF_MIN_SCORE (against
#' a confidence-derived pseudo-score: HIGH=1.0, LOW=0.2) only affects
#' coloring in the manual-review UI, not inclusion. Transform choice
#' follows how many points were located: 2 -> similarity, 3 -> affine,
#' 4+ -> full perspective/keystone correction.
#'
#' @param api_key Anthropic API key
#' @param submission_img The submission image to search (as originally
#'   read — no skew/rotation correction is applied anywhere upstream,
#'   and this function never modifies it)
#' @param reference_points data.frame(ref_id, name, x, y, w, h, anchor_text)
#'   from the template — x/y/w/h normalized 0-1 against the template's
#'   own dimensions (used only to compute each point's TARGET position;
#'   anchor_text is what actually gets searched for)
#' @param template_width,template_height Template's pixel dimensions
#' @return list(
#'   transform_fn = function(tmpl_x, tmpl_y) -> list(x, y) in submission
#'     pixel space, or NULL if fewer than 2 points were located at all,
#'     or the fit failed (degenerate points — see fit_point_transform()),
#'   matches = data.frame(ref_id, name, w, h, x, y, score, tmpl_x, tmpl_y) —
#'     one row per reference point *attempted*, x/y/score NA for any that
#'     failed to locate at all. This is what the manual-review UI shows
#'     and lets the user drag-correct; kept even when transform_fn is
#'     NULL so there's still something to review/correct and retry from.
#' )
align_via_reference_points <- function(api_key, submission_img, reference_points,
                                        template_width, template_height) {
  if (is.null(reference_points) || nrow(reference_points) < 2) return(list(transform_fn = NULL, matches = NULL))

  match_rows <- vector("list", nrow(reference_points))
  matched_pairs <- list()

  for (i in seq_len(nrow(reference_points))) {
    rp <- reference_points[i, ]
    tmpl_x <- (rp$x + rp$w / 2) * template_width
    tmpl_y <- (rp$y + rp$h / 2) * template_height
    region_hint <- describe_region(rp$x + rp$w / 2, rp$y + rp$h / 2)

    anchor <- if ("anchor_text" %in% names(rp)) trimws(rp$anchor_text) else ""
    if (!nzchar(anchor)) {
      message(sprintf("Reference point '%s' has no anchor text saved (older template, or left blank) — edit it in Template Designer to set one. Skipping it for now.", rp$name))
      match_rows[[i]] <- data.frame(ref_id = rp$ref_id, name = rp$name, w = rp$w, h = rp$h,
                                     x = NA_real_, y = NA_real_, score = NA_real_,
                                     tmpl_x = tmpl_x, tmpl_y = tmpl_y, stringsAsFactors = FALSE)
      next
    }

    loc <- tryCatch(locate_text_via_api(api_key, submission_img, anchor, region_hint = region_hint), error = function(e) {
      list(x = NA_real_, y = NA_real_, confidence = "UNKNOWN", error = conditionMessage(e))
    })

    if (!is.null(loc$error)) {
      message(sprintf("Reference point '%s' ('%s') matching failed: %s", rp$name, anchor, loc$error))
    }
    if (is.na(loc$x) || is.na(loc$y)) {
      message(sprintf("Reference point '%s' ('%s') could not be located on this submission (searched %s).", rp$name, anchor, region_hint))
      match_rows[[i]] <- data.frame(ref_id = rp$ref_id, name = rp$name, w = rp$w, h = rp$h,
                                     x = NA_real_, y = NA_real_, score = NA_real_,
                                     tmpl_x = tmpl_x, tmpl_y = tmpl_y, stringsAsFactors = FALSE)
      next
    }

    # Pseudo-score derived from self-reported confidence, NOT a true
    # match-quality metric (there's no equivalent of a correlation
    # coefficient here) — kept purely so the existing review-UI
    # coloring (green/orange against EXTRACTION_REF_MIN_SCORE) keeps
    # working without needing its own rework.
    pseudo_score <- if (identical(loc$confidence, "HIGH")) 1.0 else 0.2

    match_rows[[i]] <- data.frame(ref_id = rp$ref_id, name = rp$name, w = rp$w, h = rp$h,
                                   x = loc$x, y = loc$y, score = pseudo_score,
                                   tmpl_x = tmpl_x, tmpl_y = tmpl_y, stringsAsFactors = FALSE)
    if (pseudo_score < EXTRACTION_REF_MIN_SCORE) {
      message(sprintf("Reference point '%s' ('%s') located with LOW confidence — using it anyway; check it in the Reference Points review view.",
                       rp$name, anchor))
    }
    matched_pairs[[length(matched_pairs) + 1]] <- list(sub_x = loc$x, sub_y = loc$y, tmpl_x = tmpl_x, tmpl_y = tmpl_y)
  }

  matches_df <- do.call(rbind, match_rows)

  if (length(matched_pairs) < 2) {
    message(sprintf("Only %d of %d reference point(s) could be located at all — need at least 2 to align; falling back.",
                     length(matched_pairs), nrow(reference_points)))
    return(list(transform_fn = NULL, matches = matches_df))
  }

  list(transform_fn = fit_point_transform(matched_pairs), matches = matches_df)
}

#' Full alignment fallback chain for one submission: reference points
#' (if the template has >= 2), else border detection, else plain scale.
#' See the module header for why this order.
#'
#' @return list(transform_fn = function(tmpl_x, tmpl_y) -> list(x, y) in
#'   submission pixel space (never NULL — the plain-scale fallback
#'   always succeeds), matches = <data.frame from
#'   align_via_reference_points(), or NULL if reference points weren't
#'   used at all>, method = "reference_points"|"border"|"resize")
align_submission <- function(api_key, img, template, template_border) {
  ref_points <- template$reference_points
  if (!is.null(ref_points) && NROW(ref_points) >= 2) {
    ref_result <- align_via_reference_points(
      api_key, img, as.data.frame(ref_points, stringsAsFactors = FALSE),
      template$image_width, template$image_height
    )
    if (!is.null(ref_result$transform_fn)) {
      return(list(transform_fn = ref_result$transform_fn, matches = ref_result$matches, method = "reference_points"))
    }
    message("Reference-point alignment did not succeed for this submission — falling back to border detection.")
    fallback_fn <- fit_border_transform(img, template_border, template$image_width, template$image_height)
    return(list(transform_fn = fallback_fn, matches = ref_result$matches,
                method = if (!is.null(template_border)) "border" else "resize"))
  }
  fallback_fn <- fit_border_transform(img, template_border, template$image_width, template$image_height)
  list(transform_fn = fallback_fn, matches = NULL, method = if (!is.null(template_border)) "border" else "resize")
}

#' Convert (if PDF) a single uploaded submission file into a magick
#' image object — no skew/rotation correction (removed; see the module
#' header for why). Shared by both the pre-extraction reference-point
#' placement preview and the extraction loop itself, so "what the
#' placement UI shows you" and "what extraction actually aligns
#' against" are guaranteed to be the identical image — otherwise a
#' marker placed correctly in preview could land wrong at extraction
#' time if the two code paths ever drifted apart.
#'
#' @return magick image, or NULL if reading/converting the file failed
prep_submission_image <- function(fpath, fname) {
  ext <- tolower(fs::path_ext(fname))
  src_path <- fpath
  if (ext == "pdf") {
    converted <- tryCatch(convert_pdf_page_to_image(fpath, page = 1), error = function(e) NULL)
    if (is.null(converted)) return(NULL)
    src_path <- converted$path
  }
  tryCatch(magick::image_read(src_path), error = function(e) NULL)
}

#' Run extraction for one submission image against every field in a
#' loaded template. Aligns via align_submission() (which tries
#' reference points, then border detection, then plain scale — see
#' that function and the module header), before cropping fields. No
#' image warping happens anywhere — every field gets cropped straight
#' from the ORIGINAL submission image using its box mapped through the
#' alignment transform. No skew/rotation correction either (removed;
#' see the module header for why).
#'
#' @param api_key Anthropic API key
#' @param submission_img_path Path to the (already PDF-converted, if
#'   applicable) submission image
#' @param template A loaded template (list with image_width, image_height,
#'   fields, reference_points)
#' @param template_border Pre-computed via detect_page_border() on the
#'   template's own image — computed once per extraction run by the
#'   caller (mod_extraction.R) rather than once per submission. Used
#'   only for the border-detection fallback; reference-point alignment
#'   doesn't need the template's own image at all anymore (it searches
#'   the SUBMISSION for known text, not a cropped patch from the template).
#' @param on_field Optional callback(i, n_fields, field_name) called
#'   before each field is extracted, for progress reporting
#' @return list(
#'   results = data.frame(field_name, field_type, x, y, w, h, sub_x0,
#'     sub_y0, sub_x1, sub_y1, value, confidence, error) — the sub_*
#'     columns are each field's actual crop box in the ORIGINAL
#'     submission image's own pixel coordinates (from the alignment
#'     transform), kept so the manual-review UI can draw accurate
#'     overlay boxes directly on the real photo,
#'   image_path = path to the submission image (unmodified — the SAME
#'     image every crop came from AND what reference points were
#'     matched against; there's only one image now, no separate
#'     "aligned" version, so this and prewarp_image_path are identical),
#'   prewarp_image_path = same as image_path — kept as a separate key
#'     for backward compatibility with callers that distinguish them,
#'   ref_matches = data.frame from align_via_reference_points() (see
#'     there), or NULL if reference points weren't used for this submission,
#'   align_method = "reference_points" | "border" | "resize"
#' )
extract_submission <- function(api_key, submission_img_path, template,
                                template_border = NULL, on_field = NULL) {
  img <- magick::image_read(submission_img_path)  # ORIGINAL — never modified below

  image_path <- tempfile(fileext = ".png")
  magick::image_write(img, image_path, format = "png")

  align_result <- align_submission(api_key, img, template, template_border)
  transform_fn <- align_result$transform_fn
  if (is.null(transform_fn)) {
    # Shouldn't happen — align_submission()'s fallback chain always
    # bottoms out at fit_plain_scale_transform(), which always
    # succeeds — but guard anyway rather than let field cropping fail
    # outright if this invariant is ever violated.
    message("align_submission() unexpectedly returned no transform — using plain scale as a last-resort safety net.")
    transform_fn <- fit_plain_scale_transform(img, template$image_width, template$image_height)
  }

  fields <- as.data.frame(template$fields, stringsAsFactors = FALSE)
  n_fields <- nrow(fields)
  results <- vector("list", n_fields)

  for (i in seq_len(n_fields)) {
    field <- as.list(fields[i, ])
    if (!is.null(on_field)) on_field(i, n_fields, field$name)

    crop_info <- crop_field_image(img, field, transform_fn, template$image_width, template$image_height)
    res <- extract_field_value(api_key, crop_info$path, field)
    unlink(crop_info$path)

    results[[i]] <- data.frame(
      field_name = field$name, field_type = field$type,
      x = field$x, y = field$y, w = field$w, h = field$h,
      sub_x0 = crop_info$x0, sub_y0 = crop_info$y0, sub_x1 = crop_info$x1, sub_y1 = crop_info$y1,
      value = res$value, confidence = res$confidence,
      error = if (is.null(res$error)) NA_character_ else res$error,
      stringsAsFactors = FALSE
    )
  }

  list(results = do.call(rbind, results), image_path = image_path,
       prewarp_image_path = image_path, ref_matches = align_result$matches,
       align_method = align_result$method)
}

#' Re-run alignment and extraction for one submission using EXPLICIT
#' reference point positions (auto-matched values, manually-corrected
#' ones, or a mix) rather than re-running the matching search — this is
#' what the manual-review UI's "Re-align & Re-extract" button calls
#' after the user has dragged one or more points to correct them. Skips
#' the per-point API location call entirely; goes straight to
#' fit_point_transform() and crops fields from the ORIGINAL image, same
#' as extract_submission() — no image warping here either.
#'
#' @param prewarp_image_path The submission's originally-read image
#'   (saved from the original extract_submission() call — re-used here
#'   rather than re-reading from scratch; this is the ORIGINAL,
#'   unmodified image, same one every crop will come from)
#' @param points data.frame(ref_id, name, x, y, tmpl_x, tmpl_y) — the
#'   point positions to fit a transform from (pixel coords in the
#'   original image's frame, and in the template's frame respectively);
#'   NA rows (points that never matched and were never manually
#'   corrected either) are dropped before fitting
#' @return Same shape as extract_submission()'s return value, with
#'   ref_matches set to `points` itself (so the review UI reflects
#'   exactly what was actually used, corrections included) and
#'   align_method "reference_points_corrected"
reextract_submission_with_points <- function(api_key, prewarp_image_path, template, points, on_field = NULL) {
  img <- magick::image_read(prewarp_image_path)  # ORIGINAL — never modified

  usable <- points[!is.na(points$x) & !is.na(points$y), , drop = FALSE]
  point_pairs <- lapply(seq_len(nrow(usable)), function(i) {
    list(sub_x = usable$x[i], sub_y = usable$y[i], tmpl_x = usable$tmpl_x[i], tmpl_y = usable$tmpl_y[i])
  })

  transform_fn <- fit_point_transform(point_pairs)
  if (is.null(transform_fn)) {
    stop("Re-alignment failed — either fewer than 2 usable point positions, or the point set was rejected as degenerate (too clustered together or nearly collinear — see console for details from fit_point_transform()). Adjust the points and try again.")
  }

  fields <- as.data.frame(template$fields, stringsAsFactors = FALSE)
  n_fields <- nrow(fields)
  results <- vector("list", n_fields)

  for (i in seq_len(n_fields)) {
    field <- as.list(fields[i, ])
    if (!is.null(on_field)) on_field(i, n_fields, field$name)

    crop_info <- crop_field_image(img, field, transform_fn, template$image_width, template$image_height)
    res <- extract_field_value(api_key, crop_info$path, field)
    unlink(crop_info$path)

    results[[i]] <- data.frame(
      field_name = field$name, field_type = field$type,
      x = field$x, y = field$y, w = field$w, h = field$h,
      sub_x0 = crop_info$x0, sub_y0 = crop_info$y0, sub_x1 = crop_info$x1, sub_y1 = crop_info$y1,
      value = res$value, confidence = res$confidence,
      error = if (is.null(res$error)) NA_character_ else res$error,
      stringsAsFactors = FALSE
    )
  }

  list(results = do.call(rbind, results), image_path = prewarp_image_path,
       prewarp_image_path = prewarp_image_path,
       ref_matches = points, align_method = "reference_points_corrected")
}
