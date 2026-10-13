test_that("all functions in SticsOnR can be deparsed by rlang", {
  ns <- asNamespace("SticsOnR")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns))
  bad <- names(Filter(function(f) {
    inherits(try(rlang::expr_deparse(body(f)), silent = TRUE), "try-error")
  }, fns))
  expect(
    length(bad) == 0,
    paste0("Functions that cannot be deparsed:\n",
            paste0("  - ", names(bad), " : ", bad, collapse = "\n"))
  )
})

test_that("all functions in CroPlotR can be deparsed by rlang", {
  ns <- asNamespace("CroPlotR")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns))
  bad <- names(Filter(function(f) {
    inherits(try(rlang::expr_deparse(body(f)), silent = TRUE), "try-error")
  }, fns))
  expect(
    length(bad) == 0,
    paste0("Functions that cannot be deparsed:\n",
            paste0("  - ", names(bad), " : ", bad, collapse = "\n"))
  )
})

test_that("all functions in SticsRFiles can be deparsed by rlang", {
  ns <- asNamespace("SticsRFiles")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns))
  bad <- names(Filter(function(f) {
    inherits(try(rlang::expr_deparse(body(f)), silent = TRUE), "try-error")
  }, fns))
  expect(
    length(bad) == 0,
    paste0("Functions that cannot be deparsed:\n",
            paste0("  - ", names(bad), " : ", bad, collapse = "\n"))
  )
})

test_that("all functions in CroptimizR can be deparsed by rlang", {
  ns <- asNamespace("CroptimizR")
  fns <- Filter(is.function, mget(ls(ns, all.names = TRUE), envir = ns))
  bad <- names(Filter(function(f) {
    inherits(try(rlang::expr_deparse(body(f)), silent = TRUE), "try-error")
  }, fns))
  expect(
    length(bad) == 0,
    paste0("Functions that cannot be deparsed:\n",
            paste0("  - ", names(bad), " : ", bad, collapse = "\n"))
  )
})