#' Get data from a bottom node in PX-WEB API (data query)
#' 
#' This function fetches data (i.e. values) from the url and query supplied.
#' 
#' @param api_url URL to get data from. Needs to be the data url. See details.
#' @param query PXWEB api query, either as an R list or a JSON query. See details.
#' 
#' @details
#' // FIX THIS
#' The \code{api_url} needs to be the 
#'   list of dimensional parameters to filter data by. Note that values \emph{must} be submitted for all dimensions of the data. If you don't want to filter data, submit an asterisk in quotation marks ("*") instead of values for that dimension.
#' There are five documented filter types in the PX-WEB API documentation; "Item", "All", "Top", "Agg" and "Vs". This function currently only supports the "Item" and "All" modes. 
#' To use "Item" selection, simply submit a value or vector of values with each dimensional parameter. To use "All" selection, submit a wildcard asterisk ("*") instead of a value.
#' For detailed examples see the installed example files in the \code{examples} folder of \code{path.package("pxweb")} (these are also viewable on the project's GitHub page).
#' // FIX THIS
#' 
#' @export
#' @examples
#' \dontrun{
#' test_data <- 
#'   pxweb_get_data(api_url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet", 
#'                  query = list(ContentsCode = c('PR0101A1'), Tid = c('*')))
#' }

pxweb_get_data <- function(api_url, query) {
  pxweb_q <- pxweb_query$new(api = api_url, query = query)
  pxweb_q$get_data()
}



