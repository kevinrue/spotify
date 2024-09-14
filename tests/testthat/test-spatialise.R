test_that("it works", {
  
  kevin <- spatialise(
    path = system.file(package = "spatialist", "Kevin.jpg")
  )
  
  expect_false(is.null(kevin))
  
})