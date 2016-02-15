#' A Reference Class to represent an pxweb api connection 
#' 
#' @field api_name The name of the api (api domain)
#' @field url The URL to the api
#' @field url_type What type of pxweb api url ("database", "level", "table")
#' @field api_limits 
#' The limits of the api:
#' - max_values : maximum number of values to download in each call
#' - max_calls: maximum number of calls per time_window
#' - time_window: time window in seconds
#' @field api_timer_file The file path to api_timer.rdata file to store api_calls
#' @field meta_data Meta data of the file url
#' 
#' @references 
#'   http://www.scb.se/Grupp/OmSCB/API/API-description.pdf
#' 
#' @examples
#'   pxwebapi$new(api_url = "http://api.scb.se/OV0104/v1/doris/sv/")
#'   pxwebapi$new(api_url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/")
#'   pxwebapi$new(api_url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet")
#'   try(pxwebapi$new(api_url = "http://www.dn.se"))
#'   
#' @export pxwebapi

pxwebapi <- 
  setRefClass(
    Class = "pxwebapi", 
    fields = list(api_name = "character",
                  url = "character",
                  url_type = "character",
                  api_limits = "list",
                  api_timer_file = "character",
                  meta_data = "list"),
    
    methods = list(
      fetch_api_config = function(){
        'Get the api configuration from the pxweb api.'
        
        r <- httr::GET(url = paste0(.self$url, "?config"))

        # Stop for status 
        if(!identical(httr::status_code(r), 200L)) {
          stop(paste0("pxweb api version too old (< PX-Web 2014 December R1):\n", .self$url), call. = FALSE)
        }

        # Assert pxweb api
        if(!all(c("maxValues", "maxCalls", "timeWindow") %in% names(httr::content(r)))){
          stop(paste0("This url is not a pxweb api:\n", .self$url), call. = FALSE)
        }

        api_lim <- list(max_values=httr::content(r)$maxValues,
                        max_calls=httr::content(r)$maxCalls,
                        time_window=httr::content(r)$timeWindow)
        .self$api_limits <- api_lim
          
        # Need to be set after api_limits set (is used by add_call_to_timer)
        .self$add_call_to_timer() # call the config is a part of the call cost
      },
      
      fetch_metadata = function(){
        'Fetch metadata from the pxweb api.'
        .self$add_call_to_timer()
        r <- httr::GET(url = .self$url)
        
        # Assert pxweb api
        r_content <- httr::content(r)
        if(class(r_content)[1] != "list") {
          stop(paste0("This url is not a pxweb api:\n", .self$url), call. = FALSE)
        }
        
        # Set meta_data   
        .self$meta_data <- httr::content(r)
        
      },

      set_url_type = function(){
        'Get the url type based on the meta data.'
        if(!is.null(names(.self$meta_data[[1]])) & is.null(names(.self$meta_data))){
          if(names(.self$meta_data[[1]])[1] == "dbid") .self$url_type <- "database"
          if(names(.self$meta_data[[1]])[1] == "id") .self$url_type <- "level"
        } else if(is.null(names(.self$meta_data[[1]])) & !is.null(names(.self$meta_data))){
          if(names(.self$meta_data)[2] == "variables") .self$url_type <- "table"
        } 
        if(is.null(.self$url_type)) stop(paste0("This url is not a pxweb api:\n", .self$url), call. = FALSE)
      },      

      get_metadata = function(){
        'Get the metadata from the object.'
        .self$meta_data
      },
      
      get_data = function(content){
        'Get the api configuration from the api.'
        NULL
      },
      
      add_call_to_timer = function(calls = 1){
        'Add a call to the api to the api timer.'
        if(!file.exists(.self$api_timer_file)){ # File doesn't exist
          api_timer <- list(rep(Sys.time(), calls))
          names(api_timer) <- .self$api_name
          save(api_timer, file=.self$api_timer_file)
        } else { # File exist
          load(.self$api_timer_file)
          api_timer[[.self$api_name]] <- c(rep(Sys.time(), calls), api_timer[[.self$api_name]])
          if(length(api_timer[[.self$api_name]]) >= .self$api_limits$max_calls){
            diff <- as.numeric(api_timer[[.self$api_name]][1] - api_timer[[.self$api_name]][length(api_timer[[.self$api_name]])], units="secs")
            Sys.sleep(time=max(.self$api_limits$time_window - diff,0))
#            print(api_timer[[.self$api_name]])
#            print(as.numeric(Sys.time() - api_timer[[.self$api_name]], units="secs") < .self$api_limits$time_window)
#            print(length(api_timer[[.self$api_name]]))
            api_timer[[.self$api_name]] <- api_timer[[.self$api_name]][as.numeric(Sys.time() - api_timer[[.self$api_name]], units="secs") < .self$api_limits$time_window]
#            print(length(api_timer[[.self$api_name]]))
          }
          save(api_timer, file=.self$api_timer_file)
        }
      },
      
      set_api_timer_file = function(){
        'Set file to use for the api timer.'

        # .self$api_timer_file <- paste0(system.file("extdata/", package = "pxweb"), "api_timer.rdata")       
        pxweb_temp_dir <- paste0(tempdir(), "/pxweb/")
        if(!dir.exists(pxweb_temp_dir)) dir.create(paste0(tempdir(), "/pxweb/"))
        .self$api_timer_file <- paste0(tempdir(), "/pxweb/api_timer.rdata")
      },
      

      initialize = function(api_url){
        'Create a new pxwebapi object.'
        
        .self$url <- api_url
        .self$api_name <- make.names(strsplit(api_url, split = "/")[[1]][3])
        .self$set_api_timer_file()
        .self$fetch_api_config()
        .self$fetch_metadata()
        .self$set_url_type()
        
      },
      
      get_data = function(content){
        'Get data from content.'
        cat("get_data")
      },
      
      show = function(){
        'Print the pxwebapi object.'
        cat("Api:", .self$api_name, "\n")
        cat("Url:", .self$url, "\n")
        cat("Url type:", .self$url_type, "\n")
        cat("Api timer file:", .self$api_timer_file, "\n")
        cat("Limits:\n")
        print(.self$api_limits)
        cat("Meta_data:")
        print(.self$meta_data)
      }
    )
)        


