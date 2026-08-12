# utils_image.R
#
# Helpers for handling blank-form images during template design.

library(magick)
library(fs)
library(uuid)
library(pdftools)

#' Get pixel dimensions of an image file
get_image_dims <- function(path) {
  info <- magick::image_info(magick::image_read(path))
  list(width = info$width[1], height = info$height[1])
}

#' Number of pages in a PDF, without converting/rendering any of them —
#' used to decide how many separate submissions a single uploaded PDF
#' file expands into (see expand_submission_uploads() in
#' mod_extraction.R). Returns 1 for anything pdftools can't read as a
#' PDF at all, so a genuinely broken/corrupt upload still gets treated
#' as one (failing) submission rather than silently vanishing from the
#' batch.
get_pdf_page_count <- function(pdf_path) {
  n <- tryCatch(pdftools::pdf_info(pdf_path)$pages, error = function(e) NA_integer_)
  if (is.na(n) || n < 1) 1L else as.integer(n)
}

#' Convert one page of a PDF to a PNG file.
#'
#' Paper forms are almost always single-page, so this defaults to page 1
#' and just warns (via the returned page_count) if there were more —
#' it does not attempt to figure out which page is "the form".
#'
#' @param pdf_path Path to the source PDF
#' @param page Page number to render (default 1)
#' @param dpi Render resolution — 200 balances legibility of small
#'   handwriting against file size for typical scanned forms
#' @return list(path = <path to a temp PNG>, page_count = <int, total
#'   pages in the source PDF>)
convert_pdf_page_to_image <- function(pdf_path, page = 1, dpi = 200) {
  page_count <- pdftools::pdf_info(pdf_path)$pages
  page <- max(1, min(page, page_count))
  out_path <- tempfile(fileext = ".png")
  pdftools::pdf_convert(pdf_path, format = "png", pages = page, dpi = dpi,
                        filenames = out_path, verbose = FALSE)
  list(path = out_path, page_count = page_count)
}

#' Persist an uploaded blank-form image into permanent storage
persist_template_image <- function(tmp_path, original_name) {
  ext <- fs::path_ext(original_name)
  new_filename <- paste0(uuid::UUIDgenerate(), ".", ext)
  dest_path <- fs::path(TEMPLATE_IMAGE_DIR, new_filename)
  
  fs::dir_create(TEMPLATE_IMAGE_DIR)
  fs::file_copy(tmp_path, dest_path, overwrite = TRUE)
  
  as.character(dest_path)
}