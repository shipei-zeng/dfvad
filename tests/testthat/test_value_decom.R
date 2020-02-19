context("Testing value added decomposition outputs")

test_that("Output columns", {
        table1 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[1]]
        expect_equal(ncol(table1), 9)
        table2 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[2]]
        expect_equal(ncol(table2), 8)
})

test_that("Growth values and level values", {
        table1 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[1]]
        table2 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[2]]
        expect_equal(roll_div(table2[, "TFP"]), table1[, "TFPG"])
})
