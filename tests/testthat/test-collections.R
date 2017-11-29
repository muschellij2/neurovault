test_that("Simple Collections Call", {

  res = nv_collection(doi = "10.1016/j.neurobiolaging.2012.11.002")
  expect_true(res$response$status_code == 200)

})
