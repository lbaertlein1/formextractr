# config.R
#
# Shared constants used across modules. Keeping these in one place means
# the extraction engine (built later) reads the same field-type vocabulary
# the template designer writes, with no risk of drift.

# Supported field data types for template annotation.
# Each type will map to a distinct extraction prompt/strategy in the
# extraction engine module (not yet built).
FIELD_TYPES <- c(
  "text_printed",
  "text_handwritten",
  "date_handwritten",
  "numeric",
  "checkbox",
  "circled_text",
  "tally_count"
)

# Human-readable labels for the UI dropdown (names = internal value)
FIELD_TYPE_LABELS <- c(
  "text_printed"      = "Printed text",
  "text_handwritten"  = "Handwritten text",
  "date_handwritten"  = "Handwritten date",
  "numeric"           = "Numeric",
  "checkbox"          = "Checkbox (filled/unfilled)",
  "circled_text"      = "Circled text (one of several options)",
  "tally_count"       = "Tally / hash marks (count)"
)

# Field types that carry an extra `options` list (the set of printed
# words/phrases a respondent could circle — e.g. "Fixed / Outreach /
# Mobile"). The designer UI shows an extra input for these types only,
# and the extraction engine (Phase 2) will need this list to know what
# it's choosing between.
FIELD_TYPES_WITH_OPTIONS <- c("circled_text")

# Storage locations (relative to app root). Centralized so swapping to a
# database backend later only requires editing utils_template_io.R, not
# every call site.
TEMPLATE_DIR       <- "data/templates"
TEMPLATE_IMAGE_DIR <- "data/template_images"

# Max display width (px) for the template image in the designer UI.
# Field coordinates are stored normalized (0-1), so this only affects
# on-screen rendering, not stored data.
DESIGNER_IMAGE_MAX_WIDTH <- 900

# ---- Extraction (Phase 2) --------------------------------------------
# Model used for per-field vision extraction. Swap here to trade cost
# vs. accuracy — Haiku is cheaper/faster for high-volume batches if
# handwriting accuracy holds up well enough in practice; Opus if a
# particular form's handwriting is proving unreliable at Sonnet.
# Current model strings per https://docs.claude.com/en/docs/about-claude/models/overview
EXTRACTION_MODEL <- "claude-sonnet-5"
ANTHROPIC_API_URL <- "https://api.anthropic.com/v1/messages"
ANTHROPIC_API_VERSION <- "2023-06-01"

# 0 = always pick the single most-likely reading (deterministic-as-
# possible). NOT currently sent to the API — Claude Sonnet 5 (and Opus
# 4.7+) reject `temperature`/`top_p`/`top_k` outright when set to a
# non-default value (HTTP 400 "temperature is deprecated for this
# model"); the parameter must be omitted entirely, not tuned to 0.
# Sonnet 5's adaptive-thinking architecture controls its own sampling
# internally, which is Anthropic's replacement for this kind of control
# — see https://docs.claude.com/en/docs/about-claude/models/whats-new-sonnet-5
# Left this constant defined (unused) as a breadcrumb in case a future
# model reintroduces temperature support and this needs re-wiring in
# extract_field_value().
EXTRACTION_TEMPERATURE <- 0

# Crops smaller than this (px, either dimension) get upscaled before
# being sent for extraction — small handwriting crops are often too
# low-res for reliable reading otherwise.
EXTRACTION_MIN_CROP_DIM <- 150

# Deskew threshold (%) passed to ImageMagick's -deskew operator via
# magick::image_deskew(). NOTE: disabled by default — see
# EXTRACTION_DESKEW_ENABLED below. Left here in case it's re-enabled.
EXTRACTION_DESKEW_THRESHOLD <- 40

# Deskew was found to over-rotate in practice (sometimes badly) rather
# than correct genuine scan skew, and corrupted everything downstream
# of it when it did. It's also structurally redundant now: with 4
# reference points, alignment does a full perspective warp, which
# handles rotation as part of that general correction anyway — a
# separate blind pre-rotation step adds a failure mode without adding
# a capability reference-point alignment doesn't already have. Default
# OFF; flip to TRUE only if a specific form genuinely needs it and
# reference-point/border alignment alone isn't enough.
EXTRACTION_DESKEW_ENABLED <- FALSE

# Border-alignment (reference-point) detection. Most tally-sheet-style
# forms have a strong printed black rectangle framing the whole table —
# this constant controls how that border gets detected so a
# submission's border can be aligned to the template's, correcting for
# different scan margins/crop/scale across files (NOT full perspective
# warp — see utils_extraction.R for what this does and doesn't cover).
# A row/column is a border-line candidate if its average brightness
# (0=black, 255=white), after ImageMagick collapses it to one pixel via
# resize, is at or below this. A solid black line spanning nearly the
# full row/column width pulls the average a long way down; ordinary
# mostly-white rows with a bit of printed text don't.
EXTRACTION_BORDER_BRIGHTNESS_MAX <- 200

# ---- Reference-point alignment ----------------------------------------
# General-purpose alignment mechanism: the user marks 2-4 small regions
# on the template (grid corners, a specific printed phrase, etc.) as
# "reference points" in the Template Designer, and types (or accepts an
# auto-read) the exact text printed there — the anchor_text. At
# extraction time, each anchor_text gets located on the submission via
# a Claude API call ("where is this text on this form") — see
# locate_text_via_api() in utils_extraction.R — and the located points
# drive a geometric warp: ScaleRotateTranslate (2 points), Affine (3),
# or full Perspective/keystone correction (4+). Tried FIRST if the
# template has >= 2 reference points with anchor_text set; falls back
# to border-detection alignment, then plain resize, if not.
#
# This replaced an earlier from-scratch pixel cross-correlation
# approach (normalized cross-correlation, brute-force coarse-then-
# refine search) that went through five rounds of debugging and never
# became reliable — root-caused eventually to an ImageMagick percentage-
# geometry/DPI-metadata interaction, but even after that fix the
# fundamental approach was fighting an uphill battle: asking pixel
# correlation to do a job a vision model that can actually read text is
# much better suited for. Locating readable text is a natural fit for
# the same API this app already calls for every field; hand-rolled
# image-patch matching wasn't.
EXTRACTION_REF_MIN_SCORE <- 0.5  # threshold on the confidence-derived pseudo-score below — see locate_text_via_api()
EXTRACTION_REF_API_MAX_DIM <- 1568  # px — submission is downscaled to at most this on its longer side before
                                     # the locate-text API call; controls cost/latency without hurting precision,
                                     # since the result is a resolution-independent fraction of the image either way
