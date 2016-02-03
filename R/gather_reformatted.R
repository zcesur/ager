#' Gather and reformat names
#'
#' \code{gather_reformatted} extracts names of faculty members from a selected document and
#' rearranges the order of first and last name.
#'
#' @param data a vector that contains character strings
#' @return The output is a character vector that contains the names of faculty members in
#' the format first-middle-last name and without any special characters at the beginning of
#' strings.
#' @examples
#' scrub("2015-16") %>% gather_reformatted()
#' @seealso \code{\link{gather_exactly}} for an alternative gatherer.
gather_reformatted <- function(data){
  # Obtain first names using regular expressions
  first_name <-
    ",(| )(.*?)," %>%
    regexpr(data) %>%
    regmatches(data, .) %>%
    gsub(",(| )|,", "", .)

  # Similarly,
  last_name <-
    "[A-Z](.*?)," %>%
    regexpr(data) %>%
    regmatches(data, .) %>%
    gsub(",.*", "", .)

  names <- paste(first_name, last_name, sep = " ")
  return(names)
}