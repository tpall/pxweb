#' A Reference Class to represent an pxweb api query
#' 
#' @field query
#' 
#' @references 
#'   http://www.scb.se/Grupp/OmSCB/API/API-description.pdf
#' 
#' @examples
#'   
#'   
#' @export pxweb_query

pxweb_query <- 
  setRefClass(
    Class = "pxweb_query", 
    fields = list(query = "data.frame"),
    
    methods = list(
      check_query = function(pxweb_api){
        'Check that the call is consistent with a pxweb table url'
        
      },

      initialize = function(query, pxwebapi){
        'Create a new pxwebapi_query'
        .self$parse_query(query)        
        .self$check_pxweb_query(pxwebapi)
      },

      parse_query = function(query){
        'Parse pxweb query'
        if(is.character(query) & length(query) == 1){
          .self$query <- .self$parse_query_json(query)
        } else if(is.list(query)) {
          .self$query <- .self$parse_query_list(query)
        } else {
          .self$error_query()
        }
      },
      
      parse_query_json = function(query){
        'Parse json pxweb query'
        jsonlite::fromJSON(txt = query)$query
      },

      parse_query_list = function(query){
        'Parse list pxweb query'

        filter <- character(length(query))
        values <- list()
        for (i in 1:length(query)) {
          if (length(query[[names(query)[i]]]) == 1) {
            filter[i] <- ifelse(query[[names(query)[i]]] == "*", "all", "item")
          } else {
            filter[i] <- "item"
          }
          values[[i]] <- query[[names(query)[i]]]
        }
        s <- data.frame(filter = filter, stringsAsFactors = FALSE)
        s$values <- values
        q <- data.frame(code = names(query), stringsAsFactors = FALSE)
        q$selection <- s
        q
      },
      
      error_query = function(){
        stop("Not a correct pxweb query.")
      },
      
      check_pxweb_query = function(pxwebapi){
        if(!inherits(pxwebapi, "pxwebapi")) stop("Not a pxweb api!")
        cat("Check check")
      },
      
      get_query = function(){
        'Get data from content.'
        jsonlite::toJSON(list(query=.self$query, response=list(format="json")))
      },
      
      show = function(){
        'Print the pxwebapi object.'
        cat("Query:\n")
        print(.self$get_query())
      }
    )
)        



