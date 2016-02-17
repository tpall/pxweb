# Test suite for utils functions

context("new_pxwebapi_class.R")

test_that(desc="pxwebapi_query_class examples work",{  

  basbelopp_api <- pxwebapi$new("http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet")
  
  json_query <-
    "{
  \"query\": [
  {
  \"code\": \"Tid\",
  \"selection\": {
  \"filter\": \"item\",
  \"values\": [
  \"2006\",
  \"2007\",
  \"2008\",
  \"2009\",
  \"2010\",
  \"2011\",
  \"2012\",
  \"2013\",
  \"2014\",
  \"2015\"
  ]
  }
}
  ],
  \"response\": {
  \"format\": \"px\"
  }
}"
  
  list_query <- list(ContentsCode = c('PR0101A1'),
                     Tid = c('*'))

  file_query <- paste0(system.file(package="pxweb"), "/extdata/test_files/json_query.txt")

  expect_that({
    test_query <- 
      pxweb_query$new(json_query, basbelopp_api)
  }, not(throws_error()))

  expect_that({
    test_query <- 
      pxweb_query$new(list_query, basbelopp_api)
  }, not(throws_error()))
  
  expect_that({
    test_query <- 
      pxweb_query$new(file_query, basbelopp_api)
  }, not(throws_error()))
  
})

test_that(desc="pxwebapi_class input tests",{  
  
  expect_that({
    test_query <- 
      pxweb_query$new(1, basbelopp_api)
  }, throws_error())
  
  expect_that({
    test_query <- 
      pxweb_query$new(list_query, "url")
  }, throws_error())
  
})


