context('inject_inputs')

# This is a hack. We don't want to define new reference classes in a 
# test, so we mock `<<-` to behave like `<-`.
local({
  `<<-` <- `<-`

  test_that('it correctly injects a sample environment', {
    inputs <- list()
    fn <- function() {}
    inject_inputs(fn)
    expect_true('inputs' %in% ls(environment(fn)))
  })

  test_that('it maintains the debug flag on a function', {
    inputs <- list()
    fn <- function() {}
    debug(fn)
    inject_inputs(fn)
    expect_true(isdebugged(fn))
  })

})
