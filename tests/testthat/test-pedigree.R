test_that("inbreeding matches hand calculated full-sibling mating", {
  # Unrelated founders A,B; their full siblings C,D produce E: F(E)=1/4.
  ids <- c("A", "B", "C", "D", "E")
  sire <- c("0", "0", "A", "A", "C")
  dam <- c("0", "0", "B", "B", "D")
  expect_equal(as.numeric(fast_inbreeding_cpp(ids, sire, dam)), c(0,0,0,0,.25))
  idx <- c(5,3,1,4,2)
  expect_equal(as.numeric(fast_inbreeding_cpp(ids[idx], sire[idx], dam[idx])), c(.25,0,0,0,0))
  expect_error(fast_inbreeding_cpp(c("A","A"), c("0","0"), c("0","0")), "Duplicate")
  expect_error(fast_inbreeding_cpp(c("A","B"), c("B","A"), c("0","0")), "[Cc]ycle|[Ll]oop|[Cc]yclic")
  expect_equal(as.numeric(fast_inbreeding_cpp("A", "unknown", "0")), 0)
})
