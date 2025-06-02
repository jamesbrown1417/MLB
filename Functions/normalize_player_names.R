#' Normalize player names by removing accents
#'
#' @param name A character vector of names.
#' @return The same vector with diacritics removed and extraneous
#'   whitespace collapsed.  No other case-changes or re-formatting are applied.
normalize_player_names <- function(name) {
  # Validate input
  if (!is.character(name)) {
    stop("`name` must be a character vector.")
  }
  if (length(name) == 0L) return(character())
  
  # Trim leading/trailing whitespace
  name <- trimws(name)
  
  # Remove diacritics: stringi preferred, iconv fallback
  if (requireNamespace("stringi", quietly = TRUE)) {
    name <- stringi::stri_trans_general(name, "Latin-ASCII")
  } else {
    name <- iconv(name, from = "", to = "ASCII//TRANSLIT", sub = "")
  }
  
  # Collapse runs of internal whitespace to a single space
  name <- gsub("\\s+", " ", name, perl = TRUE)
  
  # Ensure UTF-8 output
  enc2utf8(name)
}