context("Testing weigted average aggregation")

test_that("Output columns", {
        table1 <- t_weight("y", "p", "industry", "year", "alpha", "beta", "gamma", "epsilon", "tau", sector)[[1]]
        expect_equal(ncol(table1), 8)
        table2 <- t_weight("y", "p", "industry", "year", "alpha", "beta", "gamma", "epsilon", "tau", sector)[[2]]
        expect_equal(ncol(table2), 8)
})

test_that("Growth values and level values", {
        list_test <- readRDS(system.file("extdata", "test_t_weight.rds", package = "dfvad"))
        table1 <- t_weight("y", "p", "industry", "year", "alpha", "beta", "gamma", "epsilon", "tau", sector)[[1]]
        table2 <- t_weight("y", "p", "industry", "year", "alpha", "beta", "gamma", "epsilon", "tau", sector)[[2]]
        expect_equal(table1, list_test[[1]])
        expect_equal(table2, list_test[[2]])
})
