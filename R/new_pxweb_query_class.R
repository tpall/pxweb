#' A Reference Class to represent an pxweb api query
#' 
#' @field query
#' 
#' @references 
#'   http://www.scb.se/Grupp/OmSCB/API/API-description.pdf
#' 
#' @examples
#'   example_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#'   example_query <- list(Region = c('*'), Civilstand = c('*'), Alder = c('*'), Kon = c('*'), ContentsCode = c('*'), Tid = as.character(1970:1971))
#'   pxweb_q_url <- pxweb_query$new(api = example_url, query = example_query)
#'   example_api <- pxwebapi$new(example_url)
#'   pxweb_q_api <- pxweb_query$new(api = example_api, query = example_query)
#' 
#' @export pxweb_query
pxweb_query <- 
  setRefClass(
    Class = "pxweb_query", 
    fields = list(query = "data.frame",
                  query_dimensions = "numeric",
                  api = "pxwebapi"),
    
    methods = list(

      initialize = function(api, query){
        'Create a new pxwebapi_query'
        # API can be a character/url or a pxwebapi object
        .self$parse_query(query)
        if(class(api) == "character" & length(api) == 1){
          .self$api <- pxwebapi$new(api_url = api)
        } else if(class(api) == "pxwebapi") {
          .self$api <- api
        } else {
          stop("api is not a valid url or pxwebapi object")
        }
        .self$check_pxweb_query()
        .self$set_query_dimensions()

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
      
      set_query_dimensions = function(pxwebapi_obj){
        'Get the size of the query.'
        q_dims <- integer(length(.self$query$code))
        names(q_dims) <- .self$query$code
        
        for(i in seq_along(.self$query$code)){
          if(.self$query$selection$filter[i] == "item"){
            q_dims[i] <- length(.self$query$selection$values[[i]])
          } else if(.self$query$selection$filter[i] == "all") {
            pxwebapi_obj$meta_data
            q_dims[i] <- length(pxwebapi_obj$meta_data$variables[[i]]$values)
          }
        }
        .self$query_dimensions <- q_dims
      },
      
      get_query_dimensions = function(){
        'Get the size of the query.'
        .self$query_dimensions
      },
      
      show = function(){
        'Print the pxwebapi object.'
        cat("PXWEB query:\n")
        print(.self$get_query(TRUE))
        cat("\nQuery dimensions:\n")
        print(.self$get_query_dimensions())
      
      expand_query_selection_values = function(code){
        'Set selection.filter with all values.'
        stopifnot(code %in% .self$query$code)
        index <- which(.self$query$code %in% code)
        if(.self$query$selection$filter[[index]] == "all"){
          .self$query$selection$values[[index]] <- unlist(.self$api$meta_data$variables[[index]]$values)
          .self$query$selection$filter[[index]] <- "item"
        }
      }
    )
)        



