#' A Reference Class to represent an pxweb api query
#' 
#' @field query Can be one of three different types of queries
#' - A list with named list with variable codes as names and a vector of values (or * for all)
#' - A json object query (same as produced at Statistics Swedens homepage)
#' - A txt file with a json query
#' @field api A url or a pxwebapi object
#' 
#' @references 
#'   http://www.scb.se/Grupp/OmSCB/API/API-description.pdf
#' 
#' @examples
#'   example_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#'   example_query <- list(Region = c('*'), Civilstand = c('*'), Alder = c('*'), Kon = c('*'), ContentsCode = c('*'), Tid = '*')
#'   example_query <- list(Region = c('*'), Civilstand = c('*'), Alder = c('*'), Kon = c('*'), ContentsCode = c('*'), Tid = as.character(1970:1971))
#'   pxweb_q_url <- pxweb_query$new(api = example_url, query = example_query)
#'   
#'   example_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet"
#'   example_api <- pxwebapi$new(example_url)
#'   example_query <- list(ContentsCode = c('PR0101A1'),Tid = c('*'))
#'   pxweb_q_api <- pxweb_query$new(api = example_api, query = example_query)
#'   pxweb_q_api$get_data()
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
      
      check_pxweb_query = function(){
        
        dim_names_api <- unlist(lapply(X = .self$api$get_metadata()$variables, FUN=function(X) X$code))
        
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
      
      set_query_dimensions = function(){
        'Get the size of the query.'
        q_dims <- integer(length(.self$query$code))
        names(q_dims) <- .self$query$code
        
        for(i in seq_along(.self$query$code)){
          if(.self$query$selection$filter[i] == "item"){
            q_dims[i] <- length(.self$query$selection$values[[i]])
          } else if(.self$query$selection$filter[i] == "all") {
            .self$api$meta_data
            q_dims[i] <- length(.self$api$meta_data$variables[[i]]$values)
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
        .self$api$show()
        cat("\nPXWEB query:\n")
        print(.self$get_query(TRUE))
        cat("\nQuery dimensions:\n")
        print(.self$get_query_dimensions())
      },
      
      get_data = function(){
        'Get data from the query.'

        query_list <- .self$create_query_body_list()
        big_query <- length(query_list) > 1

        if(big_query) {
          message("This is a big query. Downloading in ", length(query_list), " batches:\n")
          prgs_bar <- msg_progress_bar(length(query_list))
        }
        
        result_list <- list()
        for(q in seq_along(query_list)){
          length(query_list)
          tmp_result <- .self$get_data_batch(query_list[[q]])
          if(big_query) prgs_bar$increment()
          if(q == 1){
            result <- tmp_result
          } else {
            result$data <- rbind(result$data, tmp_result$data)
            result$comments <- c(result$comments, tmp_result$comments)
          }
        }
        
        .self$clean_final_data(result)
      },
      
      clean_final_data = function(result){
        # Add attributes
        result_data <- result$data
        attr(result_data, "variable_description") <- 
          data.frame(variable = unlist(lapply(result$metadata, FUN=function(X) X$code)),
                     description = unlist(lapply(result$metadata, FUN=function(X) X$text)), stringsAsFactors = FALSE)

        meta_data <- .self$api$get_metadata()
        variable_value_labels <- list()
        for(i in seq_along(meta_data$variables)){
          code <- meta_data$variables[[i]]$code
          if(code %in% colnames(result_data)){
          result_data[,code] <- factor(result_data[,code], 
                                       levels = unlist(meta_data$variables[[i]]$values), 
                                       labels = unlist(meta_data$variables[[i]]$valueTexts))
          variable_value_labels[[code]] <- 
            data.frame(values=unlist(meta_data$variables[[i]]$values), 
                       valueTexts=unlist(meta_data$variables[[i]]$valueTexts), stringsAsFactors = FALSE)
          }
        }
        attr(result_data, "variable_value_labels") <- variable_value_labels
        attr(result_data, "comments") <- c(unlist(lapply(result$metadata, FUN=function(X) X$comment)),
                                           unlist(lapply(result$comments, FUN=function(X) paste0(X$variable, " ", X$value, ": ", X$comment))))
        attr(result_data, "data_source") <- pxweb_q_api$api$url
        attr(result_data, "time") <- Sys.time()
        result_data
      },
      
      get_data_batch = function(query_body){
        'Get a batch of data from a pxweb api.'
        
        .self$api$add_call_to_timer()
        response <- try(httr::POST(
          url=.self$api$url,
          body = query_body,
          httr::content_type_json()), silent=TRUE)
        
        if (class(response)=="try-error") stop(paste0("No internet connection to ",.self$url), call.=FALSE)
        
        httr::stop_for_status(response)
        
        .self$clean_and_parse_json_data(json_data = suppressWarnings(httr::content(response)))
        
      },
      
      create_query_body_list = function(){
        'Split up a query into batches.'
        if(prod(.self$query_dimensions) < .self$api$api_limits$max_values){
          return(list(.self$get_query()))
        } else {
          split_variable <- which.max(.self$query_dimensions)
          batch_size <- floor(.self$api$api_limits$max_values / prod(.self$query_dimensions[-split_variable]))
          if(batch_size == 0) stop("Too large query! This should not happen, please file a bug report at github with the api call.", call.=FALSE)
          .self$expand_query_selection_values(code = names(split_variable))
          selection_values <- .self$get_query_selection_values(code = names(split_variable))
          q_list <- list()
          for(batch in 1:ceiling(.self$query_dimensions[split_variable] / batch_size)){
            .self$set_query_selection_values(code = names(split_variable), 
                                             values = selection_values[(batch_size * (batch - 1) + 1):min((batch_size * batch), length(selection_values))])
            q_list[[batch]] <- .self$get_query()
          }
          .self$set_query_selection_values(code = names(split_variable), selection_values)
          return(q_list)
        }
      },
      
      clean_and_parse_json_data = function(json_data){
        
        df <- data.frame(matrix(unlist(json_data$data), ncol=length(json_data$columns), byrow = TRUE), stringsAsFactors = FALSE)
        for(j in seq_along(json_data$columns)){
          if(json_data$columns[[j]]$type == "c") df[,j] <- as.numeric(df[,j])
        }
        colnames(df) <- unlist(lapply(json_data$columns, FUN = function(X) X$code))
        res <- list(data=df,
                    metadata=json_data$columns,
                    comments=json_data$comments)
        
        res
      },
      
      set_query_selection_values = function(code, values){
        'Set selection.filter values.'
        stopifnot(code %in% .self$query$code,
                  is.vector(values))
        index <- which(.self$query$code %in% code)
        stopifnot(all(values %in% unlist(.self$api$meta_data$variables[[index]]$values)))
        .self$query$selection$values[[index]] <- values
        if(length(values) == 1 && values == "*"){
          .self$query$selection$filter[[index]] <- "all"
        } else if (length(values) == 1 && is.numeric(values)){
          .self$query$selection$filter[[index]] <- "top"
        } else if (length(values) > 1 && is.character(values)) {
          .self$query$selection$filter[[index]] <- "item"          
        } else {
          stop(paste0("Incorrect values: ", values))
        }
      },
      
      get_query_selection_values = function(code){
        'Get selection.filter values.'
        stopifnot(code %in% .self$query$code)
        index <- which(.self$query$code %in% code)
        .self$query$selection$values[[index]]
      },
      
      expand_query_selection_values = function(code){
        'Set selection.filter with all values.'
        stopifnot(code %in% .self$query$code)
        index <- which(.self$query$code %in% code)
        if(.self$query$selection$filter[[index]] == "all"){
          .self$set_query_selection_values(code, unlist(.self$api$meta_data$variables[[index]]$values)) 
        }
      }
      
    )
)



