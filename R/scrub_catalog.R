#' Clean catalog files
#'
#' \code{scrub_catalog} removes all irrelevant data in catalog files.
#'
#' @param year an academic year of which faculty data is desired.
#' @return The output is a character vector that contains one record per element.
#' @examples
#' #scrub_catalog("2011-12")
scrub_catalog <- function(year){
  # Open up a txt file and store lines of information as character strings of the
  # vector 'data'
  flat_file <- paste(year, ".txt", sep = "") %>%
    system.file("extdata", ., package = "ager") %>%
    readLines(skipNul = TRUE)

  # Remove leftovers from the previous page.
  relevant_line <- grep("FACULTY", flat_file)[1]
  flat_file <- flat_file[-seq(1, relevant_line - 1)]

  # Remove all other irrelevant data at the beginning of the document until coming
  # across a line containing a comma
  relevant_line <- grep(",", flat_file)[1]
  flat_file <- flat_file[-seq(1, relevant_line - 1)]

  # Remove the strings which are of length 0 and those containing page numbers. 4 is not
  # chosen arbitrarily but rather based on other years' documents. For instance in the
  # previous 2 years, the strings that contain pages are formatted as 3-digit integer and
  # a space.
  flat_file <- setdiff(flat_file, flat_file[nchar(flat_file) <= 4])

  if (year == "2013-14"){
    # Clean up text if it is formatted with hard line endings
    short_lines <- order(nchar(flat_file), decreasing=FALSE)[1:15]
    long_lines <- order(nchar(flat_file), decreasing=TRUE)[1:15]

    # Evaluate which of the long lines are immediately followed by a short line
    cut_lines <- short_lines[(short_lines - 1) %in% long_lines]

    # Join the cut off line with the line above
    flat_file[cut_lines - 1] <- paste(flat_file[cut_lines - 1], flat_file[cut_lines], sep=" ")

    # Remove cut off lines
    flat_file <- setdiff(flat_file, flat_file[cut_lines])
  }
  return(flat_file)
}