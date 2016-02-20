# Test suite for utils functions

context("new_pxwebapi_class.R")

test_that(desc="pxwebapi_class examples work",{  
  
  expect_that({
    test_api <- 
      pxwebapi$new(api_url = "http://api.scb.se/OV0104/v1/doris/sv/")
  }, not(throws_error()))

  expect_that({
    test_api <- 
      pxwebapi$new(api_url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/")
  }, not(throws_error()))
  
  expect_that({
    test_api <- 
      pxwebapi$new(api_url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet")
  }, not(throws_error()))
  
  expect_that({
    test_api <- 
      pxwebapi$new(api_url = "http://www.dn.se")
  }, throws_error())

})

test_that(desc="pxwebapi_class input tests",{  
  
  expect_that({
    test_api <- 
      pxwebapi$new(api_url = 1)
  }, throws_error())
  
  expect_that({
    test_api <- 
      pxwebapi$new(api_url = c("http://api.scb.se/OV0104/v1/doris/sv/ssd/",
                               "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet"))
  }, throws_error())

})

test_that(desc="pxwebapi_class get_data()",{  

  pxwebapi_obj <- pxwebapi$new("http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet")
  query <- pxweb_query$new(list(ContentsCode = c('PR0101A1'),Tid = c('*')), pxwebapi_obj)
  
  expect_that({
    test_data <- 
      pxwebapi_obj$get_data(query)
  }, not(throws_error()))

  expect_is(test_data, "list")

  # Big query
  pxwebapi_obj <- pxwebapi$new("http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy")
  query <- pxweb_query$new(list(Region = c('*'), Civilstand = c('*'),  Alder = c('*'), Kon = c('*'), ContentsCode = c('*'), Tid = as.character(1970:1971)), pxwebapi_obj)
  
  expect_that({
    test_data <- 
      pxwebapi_obj$get_data(query)
  }, not(throws_error()))

  expect_that({
    test_data <- 
      pxwebapi_obj$get_data_batch(query)
  }, throws_error())
  
  expect_is(test_data, "list")
  
})

