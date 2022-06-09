# Test for the functions in schema.R

## verify_schema
cor_schema <- yaml::read_yaml("test_yaml/schema.yaml", readLines.warn = FALSE)
wrg_schema <- yaml::read_yaml("test_yaml/wrong.yaml", readLines.warn = FALSE)

test_that("Test if verify_schema parses the correct schema",{
  expect_equal(verify_schema(cor_schema), TRUE)
  expect_error(verify_schema(wrg_schema))
})

## import_schema
test_that("Test if import_schema imports the yaml file",{
  expect_equal(import_schema(testthat::test_path("test_yaml/schema.yaml")) |> typeof() , "list")
})
