test_that("SDI call with a non-graph", {
  expect_error(SDI(data.frame()), "Not an igraph object")
})

test_that("SDI call with empty graph", {
  tg<-igraph::make_empty_graph(n = 0, directed = TRUE)
  expect_error(SDI(tg), "Call on an empty graph")
})
