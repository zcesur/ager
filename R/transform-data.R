#' Transform data
#'
#' \code{transform_data} converts extracted data into a structured data frame
#'
#' @param selected_data a named vector that contains the graduation year and the degree of
#' faculty members.
#' 
#' @return The output is a data frame which consists of all relevant data.
#' 
#' @examples
#' #academic_year <- "2015-16"
#' #flat_file <- parse_catalog(academic_year)
#' #names <- collect_names(flat_file, reformat = TRUE)
#' #data_source <- parse_catalog("2013-14")
#' #academic_data <- extract_data(names, data_source, useInternet = TRUE)
#' #data_frame <- transform_data(academic_data)
#' 
#' @export
transform_data <- function(selected_data){

by1 <- c("BA", "BS", "BM", "BFA", "BPhil", "AB", "BE", "A.B.")
by2 <- c("B.A.", "B.S.", "B.M.", "B.F.A.", "B.Phil.", "B.A.", "B.E.", "B.A.")
for (j in 1:8){
  selected_data <- sub(by1[j], by2[j], selected_data)
}

grad_years <- as.numeric(sub(".*(\\d{4}).*", "\\1", selected_data))
degrees <- as.factor(sub(".*(B\\.(.*?)\\.|A\\.B\\.|Diploma).*", "\\1", selected_data))

data_frame <- data.frame(Name = names(selected_data),
                         Graduation.Year = grad_years,
                         Degree = degrees,
                         Age = (2016 + 22 - grad_years),
                         Academic.Year = academic_year,
                         stringsAsFactors = FALSE,
                         row.names = NULL
)
return(data_frame)
}