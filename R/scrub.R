#' Preprocess and clean the data
#'
#' \code{scrub}
#'
#' @param year an academic year of which faculty information is desired.
#' @return The output is a character vector. Each element of this vector is a character
#' string that contains raw information on a faculty member.
#' @examples
#' scrub("2011-12")
scrub <- function(year){
  # Open up a txt file and store lines of information as character strings of the
  # vector 'data'
  data <- paste(year, ".txt", sep = "") %>%
    system.file("extdata", ., package = "ager") %>%
    readLines(skipNul = TRUE)

  # Remove leftovers from the previous page. In this case, we do not have any, however
  # there are, for instance in the catalog from the year 2010-2011, some.
  data <- data[-(1:(grep("FACULTY", data)[1]))]

  # Remove all other irrelevant data at the beginning of the document until coming
  # across a line containing a comma
  data <- data[-(1:(grep(",", data)[1] - 1))]

  # Remove the strings which are of length 0 and those containing page numbers. 4 is not
  # chosen arbitrarily but rather based on other years' documents. For instance in the
  # previous 2 years, the strings that contain pages are formatted as 3-digit integer and
  # a space.
  data <- setdiff(data, data[nchar(data) <= 4])

  if (year == "2013-14"){
  # Clean up text if it is formatted with hard line endings
  short_lines <- order(nchar(data), decreasing=FALSE)[1:15]
  long_lines <- order(nchar(data), decreasing=TRUE)[1:15]

  # Evaluate which of the long lines are immediately followed by a short line
  cut_lines <- short_lines[(short_lines - 1) %in% long_lines]

  # Join the cut off line with the line above
  data[cut_lines - 1] <- paste(data[cut_lines - 1], data[cut_lines], sep=" ")

  # Remove cut off lines
  data <- setdiff(data, data[cut_lines])
  }
  return(data)
}