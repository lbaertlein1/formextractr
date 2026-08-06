# utils_extraction.R
#
# Field-level extraction via the Claude API (vision). Each field's
# annotated bounding box gets cropped out of a submission image and sent
# as a single-field question, with the prompt tailored to the field's
# `type` — this is why type matters so much in the designer (a
# circled_text field needs its options list; a tally_count field needs
# a counting instruction; etc).
#
# ALIGNMENT: a submission is aligned to the template via one of three
# mechanisms, tried in order:
#   1. Reference points (preferred, general-purpose): if the template
#      has >= 2 user-marked reference points (Template Designer, each
#      one an exact printed phrase — "anchor_text"), every one is
#      LOCATED on the submission via a Claude API call
#      (locate_text_via_api()) — "where on this form does this text
#      appear" — and the located points drive a geometric warp —
#      ScaleRotateTranslate (2 points), Affine (3), or full Perspective/
#      keystone correction (4+) — via magick::image_distort(). This is
#      the general fix: works for any form, gridded or not, and with 4
#      points corrects true perspective distortion (a photo taken at an
#      angle), not just offset/scale.
#
#      This replaced an earlier from-scratch pixel cross-correlation
#      approach (normalized cross-correlation, brute-force coarse-then-
#      refine search over raw pixel data) that went through five rounds
#      of debugging and never became reliable — the pixel-matching
#      concept itself was eventually proven sound (a faithful Python
#      port scored 0.91 against the same image pair, matching OpenCV's
#      independent computation almost exactly), but getting every step
#      of the R/magick-specific implementation right without the
#      ability to run R directly turned out to not be a winnable fight.
#      A vision model that can read text finding text it can read is
#      simply a better-suited approach than hand-rolled patch matching.
#   2. Border detection (fallback): if no reference points are defined,
#      falls back to detecting the form's own printed outer border and
#      aligning submission-border to template-border (see
#      detect_page_border()/align_to_template()) — narrower (needs a
#      clear rectangular border) but automatic, no annotation required.
#   3. Plain resize (last resort): if neither of the above finds enough
#      to work with, falls back to the original "just resize to
#      template dimensions" behavior.
# See align_submission() below for the actual fallback chain.
#
# DESKEW is OFF by default (EXTRACTION_DESKEW_ENABLED in config.R) — it
# was found to over-rotate in practice, and is structurally redundant
# with reference-point alignment's own perspective correction anyway
# (rotation is just one degree of freedom within that general warp).

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

#' Crop one field's region out of a submission image (already resized to
#' match the template's pixel dimensions), upscaling small crops for
#' legibility. Returns a path to a temp PNG.
crop_field_image <- function(submission_img, field, img_width, img_height) {
  x0 <- max(0, round(field$x * img_width))
  y0 <- max(0, round(field$y * img_height))
  w  <- max(1, round(field$w * img_width))
  h  <- max(1, round(field$h * img_height))

  cropped <- magick::image_crop(submission_img, geometry = sprintf("%dx%d+%d+%d", w, h, x0, y0))

  if (min(w, h) < EXTRACTION_MIN_CROP_DIM) {
    scale <- ceiling(EXTRACTION_MIN_CROP_DIM / max(1, min(w, h)))
    # Explicit target pixel dims + "!", not percentage geometry — see
    # the note on why "%" resize is risky (DPI/density-metadata
    # (DPI/density-metadata sensitivity, confirmed as a real bug there).
    cropped <- magick::image_resize(cropped, sprintf("%dx%d!", w * scale, h * scale))
  }

  tmp <- tempfile(fileext = ".png")
  magick::image_write(cropped, tmp, format = "png")
  tmp
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

#' Align a submission image to a template using each one's detected
#' outer border as the reference: crop the submission down to its own
#' border, resize that crop to exactly match the template border's
#' size, then place it on a template-sized canvas at the template
#' border's position. The result is an image the same pixel dimensions
#' as the template, with its printed frame lined up to the template's —
#' so a field's stored 0-1 coordinates land on the right spot even if
#' this submission was scanned with different margins/crop/scale than
#' the template was.
#'
#' Falls back to a plain forced resize (the old behavior) if either
#' image's border can't be detected — this should never be worse than
#' before, only sometimes miss out on the improvement.
align_to_template <- function(img, template_border, template_width, template_height) {
  if (is.null(template_border)) {
    return(magick::image_resize(img, sprintf("%dx%d!", template_width, template_height)))
  }

  sub_border <- detect_page_border(img)
  if (is.null(sub_border)) {
    message("Couldn't detect this submission's outer border — falling back to plain resize (no border alignment) for it.")
    return(magick::image_resize(img, sprintf("%dx%d!", template_width, template_height)))
  }

  crop_w <- sub_border$right - sub_border$left
  crop_h <- sub_border$bottom - sub_border$top
  if (crop_w < 10 || crop_h < 10) {
    message("Detected submission border was implausibly small — falling back to plain resize for it.")
    return(magick::image_resize(img, sprintf("%dx%d!", template_width, template_height)))
  }

  cropped <- magick::image_crop(img, sprintf("%dx%d+%d+%d", crop_w, crop_h, sub_border$left, sub_border$top))

  tmpl_w <- template_border$right - template_border$left
  tmpl_h <- template_border$bottom - template_border$top
  resized <- magick::image_resize(cropped, sprintf("%dx%d!", tmpl_w, tmpl_h))

  canvas <- magick::image_blank(template_width, template_height, color = "white")
  magick::image_composite(canvas, resized, offset = sprintf("+%d+%d", template_border$left, template_border$top))
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

#' Pure-geometry warp: given already-known point pairs (submission
#' pixel coords -> template pixel coords), warp `submission_img` to
#' align with the template. No matching/searching here — this is what
#' both align_via_reference_points() (using auto-matched points) and
#' the manual-correction review flow (using user-dragged corrections)
#' both call, so "how the warp itself works" only needs to be right in
#' one place.
#'
#' @param point_pairs list of list(sub_x, sub_y, tmpl_x, tmpl_y) — 2-4 entries
#' @return The warped image, or NULL if fewer than 2 pairs given or the
#'   distort itself fails
warp_with_points <- function(submission_img, point_pairs, template_width, template_height) {
  if (length(point_pairs) < 2) return(NULL)
  if (length(point_pairs) > 4) point_pairs <- point_pairs[1:4]  # Perspective wants exactly 4

  method <- switch(as.character(length(point_pairs)),
                    "2" = "ScaleRotateTranslate", "3" = "Affine", "4" = "Perspective")

  # image_distort() keeps the input's current canvas size by default
  # (bestfit=FALSE) — since destination coordinates are in TEMPLATE
  # space and the submission's own canvas may be smaller, pre-extend
  # the submission canvas to template size first so there's room for
  # mapped content to land without being clipped. (Not bestfit=TRUE
  # instead because it can also shift the output's origin to fit
  # content mapping to negative coordinates, breaking the fixed
  # top-left-anchored assumption everything else here relies on — this
  # is one of the specific pieces of this feature least verified
  # without a live R session; if aligned images come out visibly
  # shifted/clipped, this canvas-sizing step is the first place to look.)
  submission_img <- magick::image_extent(
    submission_img, sprintf("%dx%d", template_width, template_height),
    gravity = "NorthWest", color = "white"
  )

  # "Source" is the submission (what we're warping); "dest" is where in
  # the output it should land — i.e. the template's coordinates.
  coords <- unlist(lapply(point_pairs, function(m) c(m$sub_x, m$sub_y, m$tmpl_x, m$tmpl_y)))

  warped <- tryCatch(
    magick::image_distort(submission_img, method, coords),
    error = function(e) {
      message("image_distort() failed during reference-point alignment: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(warped)) return(NULL)

  # Perspective/affine distortion can leave the output a different
  # canvas size than requested, and/or transparent where content moved
  # away from — flatten onto a white, template-sized canvas.
  warped <- magick::image_background(warped, "white") |> magick::image_flatten()
  magick::image_extent(warped, sprintf("%dx%d", template_width, template_height),
                        gravity = "NorthWest", color = "white")
}

#' Locate each of a template's reference points in a submission (via
#' locate_text_via_api()) and warp the submission to align, via
#' warp_with_points(). Every point that was located AT ALL gets used —
#' EXTRACTION_REF_MIN_SCORE only affects coloring in the manual-review
#' UI, not inclusion (a low-confidence match is generally still closer
#' Locate every reference point on a submission via locate_text_via_api()
#' (one API call each) and warp the submission to align, via
#' warp_with_points(). Every point that was located AT ALL gets used —
#' EXTRACTION_REF_MIN_SCORE (against a confidence-derived pseudo-score:
#' HIGH=1.0, LOW=0.2) only affects coloring in the manual-review UI, not
#' inclusion. Transform choice follows how many points were located —
#' 2 -> ScaleRotateTranslate, 3 -> Affine, 4+ -> full Perspective/
#' keystone correction.
#'
#' @param api_key Anthropic API key
#' @param submission_img The submission image to search (whatever state
#'   it's in when this is called — deskew is off by default, see config.R)
#' @param reference_points data.frame(ref_id, name, x, y, w, h, anchor_text)
#'   from the template — x/y/w/h normalized 0-1 against the template's
#'   own dimensions (used only to compute each point's TARGET position;
#'   anchor_text is what actually gets searched for)
#' @param template_width,template_height Template's pixel dimensions
#' @return list(
#'   warped = the aligned image, or NULL if fewer than 2 points could be
#'     located at all (caller should fall back to another alignment method),
#'   matches = data.frame(ref_id, name, w, h, x, y, score, tmpl_x, tmpl_y) —
#'     one row per reference point *attempted*, x/y/score NA for any that
#'     failed to locate at all. This is what the manual-review UI shows
#'     and lets the user drag-correct; kept even when warped is NULL so
#'     there's still something to review/correct and retry from.
#' )
align_via_reference_points <- function(api_key, submission_img, reference_points,
                                        template_width, template_height) {
  if (is.null(reference_points) || nrow(reference_points) < 2) return(list(warped = NULL, matches = NULL))

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
    return(list(warped = NULL, matches = matches_df))
  }

  list(warped = warp_with_points(submission_img, matched_pairs, template_width, template_height),
       matches = matches_df)
}

#' Full alignment fallback chain for one submission: reference points
#' (if the template has >= 2), else border detection, else plain
#' resize. See the module header for why this order.
#'
#' @return list(aligned = <image>, matches = <data.frame from
#'   align_via_reference_points(), or NULL if reference points weren't
#'   used at all>, method = "reference_points"|"border"|"resize")
align_submission <- function(api_key, img, template, template_border) {
  ref_points <- template$reference_points
  if (!is.null(ref_points) && NROW(ref_points) >= 2) {
    ref_result <- align_via_reference_points(
      api_key, img, as.data.frame(ref_points, stringsAsFactors = FALSE),
      template$image_width, template$image_height
    )
    if (!is.null(ref_result$warped)) {
      return(list(aligned = ref_result$warped, matches = ref_result$matches, method = "reference_points"))
    }
    message("Reference-point alignment did not succeed for this submission — falling back to border detection.")
    fallback <- align_to_template(img, template_border, template$image_width, template$image_height)
    return(list(aligned = fallback, matches = ref_result$matches,
                method = if (!is.null(template_border)) "border" else "resize"))
  }
  fallback <- align_to_template(img, template_border, template$image_width, template$image_height)
  list(aligned = fallback, matches = NULL, method = if (!is.null(template_border)) "border" else "resize")
}

#' Correct rotational skew (a crooked scan/photo) using ImageMagick's
#' -deskew operator. Detects the dominant near-horizontal/near-vertical
#' lines in the image (typically the form's own printed grid/border) and
#' rotates to straighten. Falls back to the original, un-rotated image
#' on any failure — a failed deskew attempt should never break
#' extraction outright, just mean alignment is whatever it already was.
#'
#' Does NOT correct perspective/keystone distortion — see the note at
#' the top of this file.
deskew_image <- function(img) {
  tryCatch({
    deskewed <- magick::image_deskew(img, threshold = EXTRACTION_DESKEW_THRESHOLD)
    # -deskew rotates within the frame and can expose transparent
    # corners where the image no longer fills the rectangle; flatten
    # onto white so those corners don't end up as stray transparency
    # (or black, under some virtual-pixel settings) in a later crop.
    magick::image_background(deskewed, "white") |> magick::image_flatten()
  }, error = function(e) {
    message("Deskew failed, using original image orientation: ", conditionMessage(e))
    img
  })
}

#' Convert (if PDF) and optionally deskew a single uploaded submission
#' file into a magick image object. Shared by both the pre-extraction
#' reference-point placement preview and the extraction loop itself, so
#' "what the placement UI shows you" and "what extraction actually
#' aligns against" are guaranteed to be the identical image — otherwise
#' a marker placed correctly in preview could land wrong at extraction
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
  img <- tryCatch(magick::image_read(src_path), error = function(e) NULL)
  if (is.null(img)) return(NULL)
  if (EXTRACTION_DESKEW_ENABLED) img <- deskew_image(img)
  img
}

#' Run extraction for one submission image against every field in a
#' loaded template. Deskews, then aligns via align_submission() (which
#' tries reference points, then border detection, then plain resize —
#' see that function and the module header), before cropping fields.
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
#'   results = data.frame(field_name, field_type, x, y, w, h, value, confidence, error),
#'   image_path = path to the aligned submission image (the exact image
#'     every crop came from — kept around so the manual-review UI can
#'     display it with field boxes overlaid, guaranteed to match what
#'     was actually extracted rather than a redundantly-reprocessed copy),
#'   prewarp_image_path = path to the submission image as it was BEFORE
#'     warping (deskewed if EXTRACTION_DESKEW_ENABLED, otherwise as-read)
#'     — what reference points are matched against and shown against in
#'     the manual-review UI's "Reference Points" view,
#'   ref_matches = data.frame from align_via_reference_points() (see
#'     there), or NULL if reference points weren't used for this submission,
#'   align_method = "reference_points" | "border" | "resize"
#' )
extract_submission <- function(api_key, submission_img_path, template,
                                template_border = NULL, on_field = NULL) {
  img <- magick::image_read(submission_img_path)
  if (EXTRACTION_DESKEW_ENABLED) img <- deskew_image(img)

  prewarp_image_path <- tempfile(fileext = ".png")
  magick::image_write(img, prewarp_image_path, format = "png")

  align_result <- align_submission(api_key, img, template, template_border)
  img <- align_result$aligned

  resized_image_path <- tempfile(fileext = ".png")
  magick::image_write(img, resized_image_path, format = "png")

  fields <- as.data.frame(template$fields, stringsAsFactors = FALSE)
  n_fields <- nrow(fields)
  results <- vector("list", n_fields)

  for (i in seq_len(n_fields)) {
    field <- as.list(fields[i, ])
    if (!is.null(on_field)) on_field(i, n_fields, field$name)

    crop_path <- crop_field_image(img, field, template$image_width, template$image_height)
    res <- extract_field_value(api_key, crop_path, field)
    unlink(crop_path)

    results[[i]] <- data.frame(
      field_name = field$name, field_type = field$type,
      x = field$x, y = field$y, w = field$w, h = field$h,
      value = res$value, confidence = res$confidence,
      error = if (is.null(res$error)) NA_character_ else res$error,
      stringsAsFactors = FALSE
    )
  }

  list(results = do.call(rbind, results), image_path = resized_image_path,
       prewarp_image_path = prewarp_image_path, ref_matches = align_result$matches,
       align_method = align_result$method)
}

#' Re-run alignment and extraction for one submission using EXPLICIT
#' reference point positions (auto-matched values, manually-corrected
#' ones, or a mix) rather than re-running the matching search — this is
#' what the manual-review UI's "Re-align & Re-extract" button calls
#' after the user has dragged one or more points to correct them. Skips
#' the per-point API location call entirely; goes straight to warp_with_points().
#'
#' @param prewarp_image_path The submission's already-deskewed image
#'   (saved from the original extract_submission() call — re-used here
#'   rather than re-reading/re-deskewing from scratch)
#' @param points data.frame(ref_id, name, x, y, tmpl_x, tmpl_y) — the
#'   point positions to warp with (pixel coords in the prewarp image's
#'   frame, and in the template's frame respectively); NA rows (points
#'   that never matched and were never manually corrected either) are
#'   dropped before warping
#' @return Same shape as extract_submission()'s return value, with
#'   ref_matches set to `points` itself (so the review UI reflects
#'   exactly what was actually used, corrections included) and
#'   align_method "reference_points_corrected"
reextract_submission_with_points <- function(api_key, prewarp_image_path, template, points, on_field = NULL) {
  img <- magick::image_read(prewarp_image_path)

  usable <- points[!is.na(points$x) & !is.na(points$y), , drop = FALSE]
  point_pairs <- lapply(seq_len(nrow(usable)), function(i) {
    list(sub_x = usable$x[i], sub_y = usable$y[i], tmpl_x = usable$tmpl_x[i], tmpl_y = usable$tmpl_y[i])
  })

  warped <- warp_with_points(img, point_pairs,
                              template_width = template$image_width, template_height = template$image_height)
  if (is.null(warped)) {
    stop("Re-alignment failed with the given points (need at least 2 usable positions).")
  }

  resized_image_path <- tempfile(fileext = ".png")
  magick::image_write(warped, resized_image_path, format = "png")

  fields <- as.data.frame(template$fields, stringsAsFactors = FALSE)
  n_fields <- nrow(fields)
  results <- vector("list", n_fields)

  for (i in seq_len(n_fields)) {
    field <- as.list(fields[i, ])
    if (!is.null(on_field)) on_field(i, n_fields, field$name)

    crop_path <- crop_field_image(warped, field, template$image_width, template$image_height)
    res <- extract_field_value(api_key, crop_path, field)
    unlink(crop_path)

    results[[i]] <- data.frame(
      field_name = field$name, field_type = field$type,
      x = field$x, y = field$y, w = field$w, h = field$h,
      value = res$value, confidence = res$confidence,
      error = if (is.null(res$error)) NA_character_ else res$error,
      stringsAsFactors = FALSE
    )
  }

  list(results = do.call(rbind, results), image_path = resized_image_path,
       prewarp_image_path = prewarp_image_path,
       ref_matches = points, align_method = "reference_points_corrected")
}
