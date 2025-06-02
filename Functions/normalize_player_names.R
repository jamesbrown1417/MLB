# Function to normalize player names
normalize_player_names <- function(name) {
  # Ensure input is a character string
  if (!is.character(name)) {
    stop("Input must be a character string.")
  }

  # Ensure input is not NULL or empty
  if (is.null(name) || nchar(name) == 0) {
    return("")
  }

  # Convert to lowercase
  name <- tolower(name)

  # Define a mapping for accented characters
  accent_map <- list(
    "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u", "ü" = "u",
    "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U", "Ü" = "U",
    "à" = "a", "è" = "e", "ì" = "i", "ò" = "o", "ù" = "u",
    "À" = "A", "È" = "E", "Ì" = "I", "Ò" = "O", "Ù" = "U",
    "â" = "a", "ê" = "e", "î" = "i", "ô" = "o", "û" = "u",
    "Â" = "A", "Ê" = "E", "Î" = "I", "Ô" = "O", "Û" = "U",
    "ä" = "a", "ë" = "e", "ï" = "i", "ö" = "o", "ü" = "u",
    "Ä" = "A", "Ë" = "E", "Ï" = "I", "Ö" = "O", "Ü" = "U",
    "ã" = "a", "õ" = "o", "ñ" = "n",
    "Ã" = "A", "Õ" = "O", "Ñ" = "N",
    "ç" = "c", "Ç" = "C"
  )

  # Replace accented characters
  for (accented_char in names(accent_map)) {
    name <- gsub(accented_char, accent_map[[accented_char]], name)
  }

  # Trim leading/trailing whitespace
  name <- trimws(name)

  # Reduce multiple spaces between names to a single space
  name <- gsub("\\s+", " ", name)

  # Convert to title case (e.g., "FirstName LastName")
  # This is a simplified approach, might need a more robust solution for complex cases
  # Ensure name_parts are not empty before attempting to use substring
  name_parts <- strsplit(name, " ")[[1]]
  if (length(name_parts) > 0) {
    name_parts <- sapply(name_parts, function(part) {
      if (nchar(part) > 0) {
        paste0(toupper(substring(part, 1, 1)), substring(part, 2))
      } else {
        ""
      }
    })
    name <- paste(name_parts[name_parts != ""], collapse = " ")
  } else {
    name <- ""
  }

  # Ensure output is in UTF-8 encoding
  name <- enc2utf8(name)

  return(name)
}
