#' Collect names
#'
#' \code{collect_names} identifies names of faculty members from a selected document and
#' collects them
#'
#' @param flat_file a plain text file that contains one record (i.e., faculty member)
#' per line.
#' @param reformat logical. If FALSE (the default) names are collected exactly they
#' appear on the document, otherwise the first and last names are reordered.
#' @return The output is a character vector that contains the names of faculty members
#' without any special characters indicating whether the faculty member is visiting or on
#' leave.
#' @examples
#' #names <- scrub_catalog("2013-14") %>% collect_names(reformat = FALSE)
#' #names <- scrub_catalog("2015-16") %>% collect_names(reformat = TRUE)
collect_names <- function(flat_file, reformat = FALSE){
  if (reformat == FALSE){
  names <-
    "[A-Z](.*?)," %>%
    regexpr(flat_file) %>%
    regmatches(flat_file, .) %>%
    gsub(",", "", .)
  }
  else if (reformat == TRUE){
    # Obtain first names using regular expressions
    first_names <-
      ",(| )(.*?)," %>%
      regexpr(flat_file) %>%
      regmatches(flat_file, .) %>%
      gsub(",(| )|,", "", .)

    # Similarly,
    last_names <-
      "[A-Z](.*?)," %>%
      regexpr(flat_file) %>%
      regmatches(flat_file, .) %>%
      gsub(",.*", "", .)
    names <- paste(first_names, last_names, sep = " ")
  }
  return(names)
}