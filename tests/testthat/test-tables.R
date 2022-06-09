# Test table creation

schema <- yaml::read_yaml("test_yaml/schema_single.yaml", readLines.warn = FALSE)

test_that("Test if a table can be created",{

  parsed_list <- create_tables(schema)

  expect_equal( typeof(parsed_list), "list" )
  expect_equal( length(parsed_list[[1]]), 7 )
  expect_equal( nrow(parsed_list[[1]]), 100 )

})
