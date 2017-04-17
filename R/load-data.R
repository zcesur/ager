#' Load data
#'
#' \code{load_data} creates a cumulative data frame which includes data from various
#' academic years.
#'
#' @param df1 a data frame
#' @param df2 a data frame
#' @param ... further data frames to be passed to \code{load_data}
#' 
#' @return The output is a data frame.
#' 
#' @examples
#' #data_warehouse_df <- load_data(faculty1516_df, faculty1415_df)
#' 
#' @export
load_data <- function(df1, df2, ...){
  data_warehouse_df <- rbind(df1, df2, ...) %>%
    na.omit()
  return(data_warehouse_df)
}