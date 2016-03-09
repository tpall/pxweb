if(!exists("context")) library(testthat)

context("new_pxwebapi_query_class.R")

test_that(desc="pxwebapi_query_class examples work",{  

  api_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet"
  pxwebapi_obj <- pxwebapi$new(api_url)

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
      pxweb_query$new(query = json_query, api = pxwebapi_obj)
  }, not(throws_error()))

  expect_that({
    test_query <- 
      pxweb_query$new(query = list_query, api = pxwebapi_obj)
  }, not(throws_error()))
  
  expect_that({
    test_query <- 
      pxweb_query$new(query = file_query, api = api_url)
  }, not(throws_error()))
  
})

test_that(desc="pxwebapi_class input tests",{  
  
  expect_that({
    test_query <- 
      pxweb_query$new(query = 1, api = api_url)
  }, throws_error())
  
  expect_that({
    test_query <- 
      pxweb_query$new(query = list_query, api = "url")
  }, throws_error())
  
})


test_that(desc="Bug in order of dims (#84 at github)",{  

  pxwebapi_obj <- pxwebapi$new("http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy")
  
  query <-
    structure(
      list(
        Civilstand = "*", ContentsCode = "BE0101N1", Alder = c(
          "0",
          "1", "10", "11", "12", "14", "15", "16", "18", "19", "2", "20",
          "21", "22", "23", "24", "25", "26", "27", "28", "29", "3", "30",
          "31", "32", "33", "34", "35", "36", "37", "38", "39", "4", "40",
          "41", "42", "43", "44", "45", "46", "47", "48", "49", "5", "50",
          "51", "52", "53", "54", "55", "56", "57", "58", "59", "6", "60",
          "61", "62", "63", "64", "65", "66", "67", "68", "69", "7", "70",
          "71", "72", "73", "74", "75", "76", "77", "78", "79", "8", "80",
          "81", "82", "84", "85", "9"
        ), Kon = c("1", "2"), Tid = c(
          "2005",
          "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013",
          "2014"
        ), Region = c(
          "1382", "1383", "1384", "1401", "1402", "1407",
          "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439",
          "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447",
          "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470",
          "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485",
          "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493",
          "1494", "1495", "1496", "1497", "1498", "1499"
        )
      ), .Names = c("Civilstand",
                    "ContentsCode", "Alder", "Kon", "Tid", "Region")
    )  
  
  expect_that({
    test_query <- 
      pxweb_query$new(query = query, api = pxwebapi_obj)
  }, not(throws_error()))
  
})



test_that(desc="pxwebapi_query examples",{  
  
  example_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet"
  example_query <- list(ContentsCode = c('PR0101A1'),Tid = c('*'))
  pxweb_q_api <- pxweb_query$new(api = example_url, query = example_query)

  expect_that({
    test_data <- 
      pxweb_q_api$get_data()
  }, not(throws_error()))
  
  expect_is(test_data, "data.frame")
  
  # Big query
  example_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  example_query <- list(Region = c('*'), Civilstand = c('*'), Alder = c('*'), Kon = c('*'), ContentsCode = c('*'), Tid = as.character(1970:1971))
  pxweb_q_api <- pxweb_query$new(api = example_url, query = example_query)  

  expect_that({
    test_data <- 
      pxweb_q_api$get_data()
  }, not(throws_error()))
  
  expect_is(test_data, "data.frame")
  
})



test_that(desc="pxwebapi_query change query",{  
  
  # Test expand_query_selection_values method
  example_url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet"
  example_query <- list(ContentsCode = c('PR0101A1'),Tid = c('*'))
  pxweb_q_api <- pxweb_query$new(api = example_url, query = example_query)

  expect_true(length(pxweb_q_api$query$selection$values[[2]]) == 1)
  expect_true(pxweb_q_api$get_query_selection_values("Tid") == "*")
  
  pxweb_q_api$expand_query_selection_values(code = "Tid")
  
  expect_true(length(pxweb_q_api$query$selection$values[[2]]) == 57)
  expect_true(length(pxweb_q_api$get_query_selection_values("Tid")) == 57)
  
})

