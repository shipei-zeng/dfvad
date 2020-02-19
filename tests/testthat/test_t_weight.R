context("Testing weigted average aggregation")

test_that("Output columns", {
        table1 <- t_weight("y", "p", "industry", "year", "alpha", "beta", "gamma", "epsilon", "tau", sector)[[1]]
        expect_equal(ncol(table1), 8)
        table2 <- t_weight("y", "p", "industry", "year", "alpha", "beta", "gamma", "epsilon", "tau", sector)[[2]]
        expect_equal(ncol(table2), 8)
})
