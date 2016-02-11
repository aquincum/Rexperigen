library(Rexperigen)
context("Utility functions")

test_that("URL preparation works", {
    expect_equal(prepare.server.URL("http://alma.com/"), "http://alma.com/")
    expect_equal(prepare.server.URL("https://alma.com/"), "https://alma.com/")
    expect_equal(prepare.server.URL("http://alma.com"), "http://alma.com/")
    expect_equal(prepare.server.URL("alma.com"), "http://alma.com/")
    expect_equal(prepare.server.URL("alma.com/"), "http://alma.com/")
    expect_equal(create.API.request.URL("alma.com", "print", list(a=1, b="korte")),
                 "http://alma.com/print?a=1&b=korte")
})
