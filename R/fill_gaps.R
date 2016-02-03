#' Fill missing values
#'
#' \code{fill_gaps} locates NAs in a faculty data frame and attempts to find missing
#' information on Williams' website.
#'
#' @param data_frame a data frame that contains educational information on faculty.
#' @return The output is also a data frame.
#' @examples
#' fill_gaps(faculty_df)
fill_gaps <- function(data_frame){
  # The carrot makes sure that we only get the first word of each names element.
  first_name <- regmatches(names, regexpr("^\\w+", names))

  # The optional \\w+[-] pattern makes sure that we take hypenated last names such as
  # 'Robert Baker-White' into account.
  last_name <- regmatches(names, regexpr("(\\w+[-])?\\w+$", names))

  search_query <- paste(first_name, "+", last_name, sep = "")
  search_link <- paste("http://www.williams.edu/people/?s_directory=", search_query, sep = "")
  for (i in which(is.na(faculty_df[ ,2]) == TRUE)){
    tryCatch({
      directory_search <- read_html(search_link[i])
    }, error=function(e){cat("Unidentified error in the first connection during", print(i), "\n")})
    email_node <- html_nodes(directory_search, css = ".phone+ .email a")
    if (length(email_node) != 0){
      unix_id <- gsub("mailto:|@williams.edu", "", html_attr(email_node, "href"))
      if (length(unix_id) > 1){
        print(cat("Warning: More than one people with the name", print(names[i]), "\n"))
      }
      tryCatch({
        profile_data <- html_text(
          html_nodes(
            read_html(
              paste("http://www.williams.edu/profile/", unix_id[1], sep="")),
            css = ".profile-education .profile-subsection"))
      }, error=function(e){cat("Unidentified error in the second connection during", print(i), "\n")})
      pattern <- "(B\\.(.*?)\\.|A\\.B\\.|Diploma) .*? \\(\\d{4}\\)"

      # Catch potential exceptions
      tryCatch({
        education_info <- regmatches(profile_data, regexpr(pattern, profile_data))
        year <- as.integer(
          gsub("(B\\.(.*?)\\.|A\\.B\\.|Diploma) .* \\(|\\)", "", education_info))
        degree <- gsub(" .*", "", education_info)
        college <- gsub("(B\\.(.*?)\\.|A\\.B\\.|Diploma) | \\(\\d{4}\\)", "", education_info)
        faculty_df[i,2:5] <- c(year, degree, college, 2016-as.integer(year)+22)
      }, error=function(e){cat("Missing information on", print(names[i]), "\n")})
    }
  }
  faculty_df$Degree <- as.factor(faculty_df$Degree)
  return(data_frame)
}