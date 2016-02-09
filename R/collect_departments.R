#' Collect departments
#'
#' \code{collect_departments} identifies departments of faculty members from a selected
#' document and collects them
#'
#' @param flat_file a plain text file that contains one record (i.e., faculty member)
#' per line.
#' @return The output is a character vector that contains the departments of faculty members.
#' @section Note:
#' This function is only applicable to the academic year 2015-16.
#' @examples
#'
#' #departments <- scrub_catalog("2015-16") %>% collect_departments()
collect_departments <- function(flat_file){
  departments <- c()
    for (i in seq_along(flat_file)){
      departments[i] <-
        regexec(".*, (.*)$", flat_file[i]) %>%
        regmatches(flat_file[i], .) %>%
        sapply(function(x) x[2]) %>%
        gsub(" ", "", .) %>%
        sub("Department|Program|Graduate Program-", "", .) %>%
        gsub("(of|in|for|and|&)([A-Z])", " \\1 \\2", .) %>%
        gsub("([a-z])([A-Z])", "\\1 \\2", .)
    }
  return(departments)
}