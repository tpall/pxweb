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

      initialize = function(query, pxwebapi_obj){
        'Create a new pxwebapi_query'
        .self$parse_query(query)        
        .self$check_pxweb_query(pxwebapi_obj)
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
      
      check_pxweb_query = function(pxwebapi_obj){
        if(!inherits(pxwebapi_obj, "pxwebapi")) stop("Not a pxweb api object!")
        
        dim_names_api <- unlist(lapply(X = pxwebapi_obj$get_metadata()$variables, FUN=function(X) X$code))
        
        # ContentCode is missing (common with json queries)
        if(length(dim_names_api) - 1 == length(.self$query$code) && 
          dim_names_api[!dim_names_api %in% .self$query$code] == "ContentsCode"){
          # Add ContentCode
          ccindex <- which(!dim_names_api %in% .self$query$code)
          q <- .self$query
          cc <- q[1,]
          row.names(cc) <- row.names(cc$selection) <- nrow(q) + 1
          cc$code <- "ContentsCode"
          cc$selection$filter <- "all"
          cc$selection$values <- "*"
          q <- rbind.data.frame(q, cc)
          .self$query <- q
        } 
        
        if(all(dim_names_api %in% .self$query$code) && 
           all(.self$query$code %in% dim_names_api) &&
           any(.self$query$code != dim_names_api)){
          # Reorder query
          q <- .self$query
          rownames(q) <- .self$query$code
          q <- q[dim_names_api,]
          rownames(q) <- NULL
          .self$query <- q 
        }
        
        if(all(dim_names_api %in% .self$query$code) && 
           all(.self$query$code %in% dim_names_api) &&
           all(.self$query$code == dim_names_api)){
          # Query OK 
        } else {
          stop("Not a correct query!")
        }
      },
      
      get_query = function(pretty = FALSE){
        'Get query in json format.'
        jsonlite::toJSON(list(query=.self$query, response=list(format=jsonlite::unbox("json"))), pretty = pretty)
      },
      
      show = function(){
        'Print the pxwebapi object.'
        cat("Query:\n")
        print(.self$get_query(TRUE))
      }
    )
)        



