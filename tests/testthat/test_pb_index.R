context("Testing bilateral price indexes")

test_that("Price indexes for matched items", {
        list_test <- readRDS(system.file("extdata", "test_pb_index.rds", package = "dfvad"))
        list_p <- list()
        df <- prices[[1]]
        df <- df[order(df[,"t"]),]
        for (typ in c("l", "p", "f", "t")) {
                for (bsk in c("flx", "cst")) {
                        for (seq in c("fb","ch")) {
                                temp <- pb_index(df, "p", "q", "id", "t", typ, seq, bsk)
                                temp <- temp[[2]]
                                list_p[[paste(typ, seq, bsk, sep="_")]] <- temp
                        }
                }
        }
        expect_equal(list_p, list_test[[1]])
})

test_that("Price indexes for unmatched items", {
        list_test <- readRDS(system.file("extdata", "test_pb_index.rds", package = "dfvad"))
        list_p <- list()
        df <- prices[[2]]
        df <- df[order(df[,"t"]),]
        for (typ in c("l", "p", "f", "t")) {
                for (bsk in c("flx", "cst")) {
                        for (seq in c("fb","ch")) {
                                temp <- pb_index(df, "p", "q", "id", "t", typ, seq, bsk)
                                temp <- temp[[2]]
                                list_p[[paste(typ, seq, bsk, sep="_")]] <- temp
                        }
                }
        }
        expect_equal(list_p, list_test[[2]])
})
