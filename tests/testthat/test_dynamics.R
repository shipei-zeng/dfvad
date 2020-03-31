context("Testing firm dynamics")

test_that("Firm dynamics with different methods", {
        list_test <- readRDS(system.file("extdata", "test_dynamics.rds", package = "dfvad"))
        df <- list_test[[1]]
        for (i in c("bhc", "gr", "fhk", "bg", "df", "mp")) {
                dym <- dynamics(df, "tfp", "share", "firm", "period", typ = i)
                i <- toupper(i)
                expect_equal(dym, list_test[[2]][[i]])
        }
})
