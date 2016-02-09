#' Extract academic data
#'
#' \code{extract_data} extracts the data that contains academic background of faculty
#' members from (1) a catalog chosen as the data source and (2) Williams' web directory.
#'
#' @param names character vector that contains the names of faculty members whose academic
#' information is desired.
#' @param data_source a plain text file that contains the desired information.
#' @param useInternet logical. If TRUE missing data is extracted from Williams' website,
#' otherwise it is left blank.
#' @return The output is a named vector that contains the graduation year and the degree of
#' faculty members.
#' @section Note:
#' Assigning the TRUE value to \code{useInternet} may result in a code execution that takes a
#' while to finish.
#' @examples
#' #academic_year <- "2015-16"
#' #flat_file <- scrub_catalog(academic_year)
#' #names <- collect_names(flat_file, reformat = TRUE)
#' #data_source <- scrub_catalog("2013-14")
#' #academic_data <- extract_data(names, data_source, useInternet = TRUE)
extract_data <- function(names, data_source, useInternet = FALSE){

  pattern <- "\\d{4}, (B\\w+|AB), (.*?),"
  academic_data <- sapply(names, function(x){
    vector <-
      regexpr(pattern, data_source[grep(x, data_source)]) %>%
      regmatches(data_source[grep(x, data_source)], .)
    if(length(vector) == 0){
      return(NA)
    }else{
      return(vector)
    }
  })

  if (useInternet == TRUE){
  # The carrot makes sure that we only get the first word of each names element.
  first_names <- regmatches(names, regexpr("^\\w+", names))

  # The optional \\w+[-] pattern makes sure that we take hypenated last names such as
  # 'Robert Baker-White' into account.
  last_names <- regmatches(names, regexpr("(\\w+[-])?\\w+$", names))

  # Generate search queries in order to access data on the web directory
  search_queries <- paste(first_names, "+", last_names, sep = "")
  search_links <- paste("http://www.williams.edu/people/?s_directory=",
    search_queries,
    sep = "")

  # Return the indices of the elements with missing data
  missing_data <- unname(which(is.na(academic_data == TRUE)))

    for (i in missing_data){
      tryCatch({
        directory_search <- read_html(search_links[i])
      }, error=function(e){cat("Unidentified error in the first connection during", print(i), "\n")})
      email_nodes <- html_nodes(directory_search, css = ".phone+ .email a")
      if (length(email_nodes) != 0){
        unix_ids <- gsub("mailto:|@williams.edu", "", html_attr(email_nodes, "href"))
        if (length(unix_ids) > 1){
          print(cat("Warning: More than one people with the name", print(names[i]), "\n"))
        }
        tryCatch({
          profile_data <- html_text(
            html_nodes(
              read_html(
                paste("http://www.williams.edu/profile/", unix_ids[1], sep="")),
              css = ".profile-education .profile-subsection"))
        }, error=function(e){cat("Unidentified error in the second connection during", print(i), "\n")})

        pattern <- "(B\\.(.*?)\\.|A\\.B\\.|Diploma) .*? \\(\\d{4}\\)"
        # Catch potential exceptions
        tryCatch({
          academic_data[i] <- regmatches(profile_data, regexpr(pattern, profile_data))
        }, error=function(e){cat("Missing information on", print(names[i]), "\n")})
      }
    }
  }
  return(academic_data)
}
