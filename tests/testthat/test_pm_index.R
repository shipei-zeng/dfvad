context("Testing multilateral price indexes")

test_that("Price indexes for matched items", {
        list_test <- readRDS(system.file("extdata", "test_pm_index.rds", package = "dfvad"))
        list_test <- list_test[[1]]
        index_name <- c("wtpd", "gk", "geks", "ccdi")
        list_p <- list()
        for (i in 1:length(prices)) {
                df <- prices[[i]]
                df <- df[order(df[,"t"]),]
                p_temp <- lapply(index_name, function(ind) {
                        temp <- pm_index(df, "p", "q", "id", "t", ind, bsk="cst")
                        return(temp)
                })
                p_temp <- do.call(cbind, p_temp)
                p_temp <- p_temp[, -c(3, 5, 7)]
                colnames(p_temp) <- c("t", index_name)
                expect_equal(round(p_temp[,-1], digits = 4), list_test[[i]][, index_name])
        }
})

test_that("Price indexes for unmatched items", {
        list_test <- readRDS(system.file("extdata", "test_pm_index.rds", package = "dfvad"))
        list_test <- list_test[[1]]
        index_name <- c("wtpd", "gk", "geks", "ccdi")
        list_p <- list()
        for (i in 1:length(prices)) {
                df <- prices[[i]]
                # add unmatched items
                df_add <- matrix(c(1, 6, 12, 5, 6, 7, 0.5, 0.5, 0.5, 9, 9, 9), nrow=3)
                df_add <- as.data.frame(df_add)
                colnames(df_add) <- colnames(df)
                df <- rbind(df, df_add)
                df <- df[order(df[,"t"]),]
                # test constant baskets
                p_temp <- lapply(index_name, function(ind) {
                        temp <- pm_index(df, "p", "q", "id", "t", ind, bsk="cst")
                        return(temp)
                })
                p_temp <- do.call(cbind, p_temp)
                p_temp <- p_temp[, -c(3, 5, 7)]
                colnames(p_temp) <- c("t", index_name)
                expect_equal(round(p_temp[,-1], digits = 4), list_test[[i]][, index_name])
                # test flexible baskets
                p_temp <- lapply(index_name, function(ind) {
                        ind_bsk <- "flx"
                        if (ind == "gk" | ind == "wtpd") {
                                ind_bsk <- "cst"
                        }
                        temp <- pm_index(df, "p", "q", "id", "t", ind, bsk = ind_bsk)
                        return(temp)
                })
                p_temp <- do.call(cbind, p_temp)
                p_temp <- p_temp[, -c(3, 5, 7)]
                colnames(p_temp) <- c("t", index_name)
                expect_equal(round(p_temp[,-1], digits = 4), list_test[[i]][, index_name])
        }
})

test_that("Linking positions in rolling windows", {
        list_test <- readRDS(system.file("extdata", "test_pm_index.rds", package = "dfvad"))
        df_test <- list_test[[2]]
        link_list <- as.list(1:10)
        link_list[[11]] <- "mean"
        len_val <- 11
        p_diff_list <- lapply(1:(length(prices)-1), function(i) {
                df <- prices[[i]]
                df <- df[order(df[, "t"]), ]
                # sales adjusted
                q2_adjust <- (df[, "t"] %in% c(3, 9)) & df[, "id"]=="2"
                df[q2_adjust, "q"] <- df[q2_adjust, "q"]/2
                q3_adjust <- (df[, "t"] %in% c(6, 11)) & df[,"id"]=="3"
                df[q3_adjust, "q"] <- df[q3_adjust, "q"]/2
                # single window CCDI
                p_sgl <- pm_index(df, "p", "q", "id", "t", "ccdi")
                p_sgl_tail <- tail(p_sgl, 1)[,-1]
                # rolling windows CCDI
                p_wd_tail <- lapply(link_list, function(link_val) {
                        p_wd <- pm_index(df, "p", "q", "id", "t", "ccdi", len_val, link_val)
                        p_temp <- tail(p_wd, 1)[,-1]
                        return(p_temp)
                })
                p_wd_tail <- unlist(p_wd_tail)
                p_diff <- p_wd_tail-p_sgl_tail
                return(p_diff)
        })
        p_temp <- data.frame(c(2:11,"mean"), do.call(cbind, p_diff_list))
        colnames(p_temp) <- colnames(df_test)
        p_temp[, -1] <- round(p_temp[, -1], digits = 5)
        expect_equal(p_temp[, -1], df_test[, -1])
})
