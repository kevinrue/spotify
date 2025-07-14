test_that("it works", {
  
  kevin <- spotify(
    path = system.file(package = "spotify", "Kevin.jpg")
  )
  
  expect_false(is.null(kevin))
  
})