# utils_version_matching.R
#
# STATUS: NOT IMPLEMENTED. This file exists to document where
# submission-to-version matching belongs and what it will need, so the
# Template Designer's data model (multiple versions per template, each
# with its own blank image + fields) is already shaped correctly for it.
# The actual matching runs at extraction time — Phase 2 (Extraction
# Engine) / Phase 3 (Batch Ingestion) — neither of which exists yet.
#
# THE PROBLEM
# When a user points batch ingestion at a folder of completed-form scans,
# each scan needs to be matched to the correct *version* of its template
# before any field can be cropped (different versions can have different
# field positions, different numbers of fields, or a different layout
# entirely). This has to happen automatically at scale — no one is going
# to hand-tag thousands of scans with "this is v2".
#
# CANDIDATE APPROACHES (not yet evaluated against real data)
#   1. Perceptual image hashing (e.g. pHash) of the completed scan vs.
#      each version's blank-form image, picking the closest match.
#      Cheap, offline, but sensitive to scan skew/crop/lighting unless
#      the images are normalized first (deskew, crop-to-content). NOTE:
#      versions flagged `is_filled_reference = TRUE` (annotated from a
#      completed form, not a blank) will hash differently from a truly
#      blank submission and from each other — this approach will need
#      to either mask out the annotated field regions before hashing
#      (compare only the static print, not the handwritten content) or
#      fall back to approach 2/3 for those versions specifically.
#   2. Structural/layout fingerprint — detect the page's grid-line
#      positions (already partly solved for tally-sheet cleanup work)
#      and compare against each version's known line positions. More
#      robust to lighting than pHash, more work to build.
#   3. Vision-LLM classification — show the model the scan plus a
#      thumbnail of each version's blank form, ask which one it matches.
#      Most robust to real-world scan variance, costs an API call per
#      submission, needs internet.
#   4. OCR a single distinguishing anchor (e.g. a version code printed
#      in a corner, if the forms have one) — fast and cheap if available,
#      not applicable to forms that don't print a version marker.
#
# In practice this will likely be (3) as a fallback when (1)/(2) come
# back below a confidence threshold, similar to how the review interface
# (Phase 4) already plans to flag low-confidence extractions for manual
# correction rather than trusting automation blindly.
#
# EXPECTED INTERFACE (once built)
#   match_version(template_id, image_path) -> list(version_id, confidence)
#     - Looks up all versions for `template_id` via list_templates()/
#       load_template() (utils_template_io.R).
#     - Returns the best-matching version_id and a 0-1 confidence score.
#     - Batch ingestion (Phase 3) calls this once per submitted image,
#       auto-assigns when confidence is high, and queues low-confidence
#       matches for manual version selection in the review interface
#       rather than guessing.
#
# Not building this now — flagging it here so it isn't silently dropped,
# and so the version schema in utils_template_io.R was designed with this
# consumer in mind (each version already carries its own image_file,
# which is exactly what approaches 1/2/3 above need as a reference).
