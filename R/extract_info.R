#' Extract educational background of Williams College faculty
#'
#' \code{extract_info} creates a data frame by parsing and giving tabular structure to a
#'  text file that contains raw data. It uses three other functions that are also from this
#'  package: \code{\link{scrub}}, \code{\link{gather_reformatted}}/\code{\link{gather_exactly}} and \code{\link{fill_gaps}}.
#'
#' @param year an academic year of which faculty information is desired.
#' @param source_year an optional academic year that contains desired data. If the catalog of
#' the desired academic year already contains the data, then repeat the first year.
#' @param transformNames logical. If FALSE (the default) names are scraped exactly they
#' appear on the document, otherwise the first and last names are reordered.
#' @param fillGaps logical. If TRUE (the default) missing data is extracted from Williams'
#' website, otherwise it is left blank.
#' @return The output is a data frame that contains the name, graduation year, degree,
#' college and current age of faculty members.
#' @section Note:
#' Assigning the TRUE value to \code{fillGaps} may result in a code execution that takes a
#' while to finish. If needed, this step can be done separately with the
#' \code{\link{fill_gaps}} function that takes and returns a data frame.
#' @examples
#' extract_info("2015-16", "2013-14", transformNames = TRUE, fillGaps = TRUE)
#' extract_info("2013-14", "2013-14", fillGaps = FALSE)
#' extract_info("2014-15", "2013-14", fillGaps = FALSE)
extract_info <- function(year, source_year,
                transformNames = FALSE, fillGaps = TRUE){
  data <- scrub(year)
  if (!missing(source_year)){
    data2 <- scrub(source_year)
  }
  if (transformNames == TRUE){
    names <- gather_reformatted(data)
  }else{
    names <- gather_exactly(data)
  }
  faculty_df <- data.frame(matrix(vector(), length(names), 5,
    dimnames=list(c(), c("Name", "Graduation.Year", "Degree", "College", "Current.Age"))),
    stringsAsFactors=FALSE)
  for (i in seq(along = names)){
    if (length(grep(names[i], data2))==0){
      faculty_df[i, ] <- c(names[i], rep(NA, 4))
    }else{
      pattern <- "\\d{4}, (B\\w+|AB), (.*?),"
      education_info <- regmatches(data2[grep(names[i], data2)], regexpr(pattern, data2[grep(names[i], data2)]))
      if (length(education_info) == 0){
        faculty_df[i, ] <- c(names[i], rep(NA, 4))
      }else{
        year <- gsub(",(.*)", "", education_info)
        degree <- gsub("\\d{4}, |,(.*)", "", education_info)
        college <- gsub("\\d{4}(.*), |,", "", education_info)
        faculty_df[i, ] <- c(names[i], year, degree, college, 2016-as.integer(year)+22)
      }
    }
  }
  by1 <- c("BA", "BS", "BM", "BFA", "BPhil", "AB", "BE")
  by2 <- c("B.A.", "B.S.", "B.M.", "B.F.A.", "B.Phil.", "B.A.", "B.E.")
  for (j in 1:7){
    faculty_df$Degree <- sub(by1[j], by2[j], faculty_df$Degree)
  }

  if (fillGaps == TRUE){
    # The carrot makes sure that we only get the first word of each names element.
    first_name <- regmatches(names, regexpr("^\\w+", names))

    # The optional \\w+[-] pattern makes sure that we take hypenated last names such as
    # 'Robert Baker-White' into account.
    last_name <- regmatches(names, regexpr("(\\w+[-])?\\w+$", names))

    search_query <- paste(first_name, "+", last_name, sep = "")
    search_link <- paste("http://www.williams.edu/people/?s_directory=", search_query, sep = "")
    # Only deal with the ones with a missing value
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
  }
  faculty_df$Degree <- as.factor(faculty_df$Degree)
  faculty_df$Graduation.Year <- as.integer(faculty_df$Graduation.Year)
  faculty_df$Current.Age <- as.factor(faculty_df$Current.Age)
  return(faculty_df)
}