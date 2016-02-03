#' Gather names exactly
#'
#' \code{gather_exactly} extracts names of faculty members from a selected document
#'
#' @param data a vector that contains character strings
#' @return The output is a character vector that contains the names of faculty members as
#' they exactly appear on the data source but without any special characters at the
#' beginning of strings.
#' @examples
#' scrub("2010-11") %>% gather_exactly()
#' @seealso \code{\link{gather_reformatted}} for an alternative gatherer.
gather_exactly <- function(data){
  names <-
    "[A-Z](.*?)," %>%
    regexpr(data) %>%
    regmatches(data, .) %>%
    gsub(",", "", .)
  return(names)
}