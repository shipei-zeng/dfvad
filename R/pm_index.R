#' @title
#' Multilateral Price Indexes
#'
#' @description
#' Multilateral indexes refer to the case when more than two periods
#' are compared each time. \code{pm_index()} computes price indexes in a
#' multilateral approach.
#'
#' @param df A data frame sorted by the time period column.
#' @param p A string indicating the price column.
#' @param qty A string indicating the quantity column.
#' @param id A string indicating the identity column.
#' @param tm A string indicating the time period column. Each period must
#' contain two observations at least.
#' @param typ Relevant types of price indexes. Options include "geks"
#' for GEKS price indexes (by default), "ccdi" for CCDI price indexes,
#' "wtpd" for the weighted time product dummy method, and "gk" for the
#' Geary-Khamis method.
#' @param len Window length for linked indexes using rolling windows. A
#' single window is set as NULL (by default).
#' @param lnk Linking position in rolling windows, effective when `len`
#' is not NULL. If no linking position is provided, it should be set as NULL
#' (by default). Other options include "mean" for mean splices and numbers
#' for specific cases.
#' @param bsk The choice of baskets when items are not matched over
#' multiple periods. Options include "flx" (by default) for a flexible basket
#' that varies depending on the maximal number of matched items in
#' two periods each time, and "cst" for a constant basket
#' that takes the maximal number of matched items across all periods.
#' @param wd The choice of windows when items are not matched over multiple
#' windows. Options include "flx" (by default) for a flexible window that allows for
#' different items in two windows each time, and "cst" for a constant window
#' that takes the maximal number of matched items across all windows.
#'
#' @return
#' A data frame consisting of the time period and price indexes.
#'
#' @usage
#' pm_index(df, p, qty, id, tm, typ = "geks", len = NULL, lnk = NULL, bsk = "flx", wd = "flx")
#'
#' @export
pm_index <- function(df, p, qty, id, tm, typ="geks", len=NULL, lnk=NULL, bsk="flx", wd="flx") {
        # data preparation
        df <- df[, c(p, qty, id, tm)]
        tm_vec <- unique(df[, tm])
        # window filter
        if (wd == "cst") {
                count_id <- table(df[, id])
                match_id <- count_id[count_id==length(tm_vec)]
                df <- df[as.character(df[, id]) %in% names(match_id), ]
        }
        # construct a window
        if (is.null(len)) {
                len <- length(tm_vec)
        }
        wd_tick <- 1
        df_wd <- df[df[, tm] %in% tm_vec[wd_tick:(wd_tick+len-1)], ]
        p_fun <- switch (typ, "geks"=geks_index,
                         "ccdi"=ccdi_index,
                         "wtpd"=wtpd_index,
                         "gk"=gk_index
        )
        p_out <- p_fun(df_wd, p, qty, id, tm, bsk)
        # iterative update
        while (length(p_out) < length(tm_vec)) {
                wd_tick <- wd_tick+1
                df_wd <- df[df[, tm] %in% tm_vec[wd_tick:(wd_tick+len-1)], ]
                p_temp <- p_fun(df_wd, p, qty, id, tm, bsk)
                # append the linked indexes
                if (is.numeric(lnk)) {
                        # for link as numbers
                        lnk_obs_1 <- p_temp[len]
                        lnk_obs_0 <- p_temp[lnk]
                        lnk_rate <- lnk_obs_1/lnk_obs_0
                        lnk_fill <- wd_tick+lnk-1
                        lnk_obs_fill <- p_out[lnk_fill]*lnk_rate
                } else if (lnk=="mean") {
                        # for link as "mean"
                        lnk_obs_1 <- p_temp[len]
                        lnk_obs_0 <- p_temp[1:(len-1)]
                        lnk_rate <- lnk_obs_1/exp(mean(log(lnk_obs_0)))
                        lnk_mean <- p_out[wd_tick:(wd_tick+len-2)]
                        lnk_obs_fill <- exp(mean(log(lnk_mean)))*lnk_rate
                } else {
                        stop("The linking position is not provided.")
                }
                p_out <- c(p_out, lnk_obs_fill)
        }
        df_p <- data.frame(tm_vec, p_out)
        colnames(df_p) <- c(tm, "index")
        return(df_p)
}

#' @title
#' GEKS indexes
#'
#' @keywords internal
#'
#' @noRd
geks_index <- function(df, p, qty, id, tm, bsk) {
        # rearrange time order
        tm_vec <- unique(df[, tm])
        p_loop <- lapply(tm_vec, function(x) {
                tm_x <- which(df[, tm]==x)
                tm_y <- (1:nrow(df))[-tm_x]
                df <- df[c(tm_x, tm_y),]
                pindex_fb <- pb_index(df, p, qty, id, tm, "f", "fb", bsk)[, 2]
                return(pindex_fb)
        })
        # back to the original time order
        p_loop <- lapply(1:length(p_loop), function(x) {
                vec_x <- p_loop[[x]]
                if (x>1) {
                        vec_x[1:x] <- vec_x[c(2:x, 1)]
                }
                return(vec_x)
        })
        p_loop <- Reduce("*", p_loop)
        p_loop <- p_loop^(1/length(tm_vec))
        # normalised
        p_out <- p_loop/p_loop[1]
        return(p_out)
}

#' @title
#' CCDI indexes
#'
#' @keywords internal
#'
#' @noRd
ccdi_index <- function(df, p, qty, id, tm, bsk) {
        # rearrange time order
        tm_vec <- unique(df[, tm])
        p_loop <- lapply(tm_vec, function(x) {
                tm_x <- which(df[, tm]==x)
                tm_y <- (1:nrow(df))[-tm_x]
                df <- df[c(tm_x, tm_y),]
                pindex_fb <- pb_index(df, p, qty, id, tm, "t", "fb", bsk)[, 2]
                return(pindex_fb)
        })
        # back to the original time order
        p_loop <- lapply(1:length(p_loop), function(x) {
                vec_x <- p_loop[[x]]
                if (x>1) {
                        vec_x[1:x] <- vec_x[c(2:x, 1)]
                }
                return(vec_x)
        })
        p_loop <- Reduce("*", p_loop)
        p_loop <- p_loop^(1/length(tm_vec))
        # normalised
        p_out <- p_loop/p_loop[1]
        return(p_out)
}

#' @title
#' Weighted time product dummy method
#'
#' @keywords internal
#'
#' @noRd
wtpd_index <- function(df, p, qty, id, tm, bsk) {
        # data preparation
        tm_vec <- unique(df[, tm])
        # basket filter
        if (bsk == "flx") {
                warning("The weighted time product dummy method excludes flexible baskets. Constant baskets apply automatically.")
                bsk <- "cst"
        }
        if (bsk == "cst") {
                count_id <- table(df[, id])
                match_id <- count_id[count_id==length(tm_vec)]
                df <- df[as.character(df[, id]) %in% names(match_id), ]
        }
        # value shares
        value_vec <- df[, p] * df[, qty]
        df_temp <- data.frame(value_vec, df[, tm])
        names(df_temp) <- c("value", "period")
        df_temp <- merge(df_temp, aggregate(value ~ period, data = df_temp, sum),
                         by = "period", suffixes = c("", "sum"))
        share <- value_vec/df_temp[, "valuesum"]
        # value share matrix
        s_mat <- matrix(share, byrow=TRUE, nrow=length(tm_vec))
        w_tnj_list <- lapply(as.data.frame(t(s_mat)), function(x) {
                s_nj <- x %*% t(x)
                diag(s_nj) <- 0
                return(s_nj)
        })
        w_nj_mat <- Reduce("+", w_tnj_list)
        f_nj_mat <- t(apply(w_nj_mat, 1, function(x) {x/sum(x)}))
        w_nk_sum <- apply(w_nj_mat, 1, function(x) {sum(x)})
        f_tnj_list <- lapply(w_tnj_list, function(x) {
                apply(x, 2, function(y) {y/w_nk_sum})
        })
        # I_N, F and f used to solve equations
        # I_N: diag_mat
        # F: f_nj_mat
        # f: f_n_vec
        n_id <- ncol(s_mat)
        diag_mat <- diag(nrow=n_id)
        y_mat <- log(matrix(df[, p], byrow=TRUE, nrow=length(tm_vec)))
        y_tnj_list <- lapply(as.data.frame(t(y_mat)), function(x) {
                y_nj <- matrix(rep(x, n_id), nrow=n_id)
                y_jn <- matrix(rep(x, n_id), nrow=n_id, byrow=TRUE)
                temp <- y_nj-y_jn
                return(temp)
        })
        fy_tnj_list <- Map("*", f_tnj_list, y_tnj_list)
        f_n_vec <-  apply(Reduce("+", fy_tnj_list), 1, function(x) {sum(x)})
        # solve the system of equations
        i_n_f <- diag_mat-f_nj_mat
        beta <- c(solve(i_n_f[-n_id, -n_id], f_n_vec[-n_id]),0)
        # weighted time product dummy price level
        b <- exp(beta)
        p_temp <- lapply(tm_vec, function(x) {
                p_t <- df[df[,tm]==x, p]
                s_t <- share[df[,tm]==x]
                a_t <- prod((p_t/b)^(s_t))
        })
        pindex_wtpd <- do.call(rbind, p_temp)
        # normalised
        p_out <- pindex_wtpd/pindex_wtpd[1]
        return(p_out)
}

#' @title
#' Geary-Khamis method
#'
#' @keywords internal
#'
#' @noRd
gk_index <- function(df, p, qty, id, tm, bsk) {
        # data preparation
        tm_vec <- unique(df[, tm])
        # basket filter
        if (bsk == "flx") {
                warning("The Geary-Khamis method excludes flexible baskets. Constant baskets apply automatically.")
                bsk <- "cst"
        }
        if (bsk == "cst") {
                count_id <- table(df[, id])
                match_id <- count_id[count_id==length(tm_vec)]
                df <- df[as.character(df[, id]) %in% names(match_id), ]
        }
        # value shares
        value_vec <- df[, p] * df[, qty]
        df_temp <- data.frame(value_vec, df[, tm])
        names(df_temp) <- c("value", "period")
        df_temp <- merge(df_temp, aggregate(value ~ period, data = df_temp, sum),
                    by = "period", suffixes = c("", "sum"))
        share <- value_vec/df_temp[, "valuesum"]
        # matrices required to solve the equation system
        q_mat <- matrix(df[,qty], byrow=TRUE, nrow=length(tm_vec))
        q_hat_inv <-  solve(diag(apply(q_mat, 2, sum)))
        s_mat <- matrix(share, byrow=TRUE, nrow=length(tm_vec))
        q_list <- as.list(as.data.frame(t(q_mat)))
        s_list <- as.list(as.data.frame(t(s_mat)))
        sq_list <- Map(function(x,y) {x %*% t(y)}, s_list, q_list)
        sq_sum <- Reduce("+", sq_list)
        # I_N, C and 0_N used to solve equations
        # I_N: diag_mat
        # C: c_mat
        # 0_N: vec_0
        n_id <- ncol(s_mat)
        diag_mat <- diag(nrow=n_id)
        c_mat <- q_hat_inv %*% sq_sum
        i_n_c <- diag_mat-c_mat
        vec_part <- c_mat[-nrow(c_mat), ncol(c_mat)]
        b_vec <- c(solve(i_n_c[-n_id, -n_id], vec_part), 1)
        # GK indexes
        p_temp <- lapply(tm_vec, function(x) {
                p_1 <- df[df[,tm]==x, p]
                q_1 <- df[df[,tm]==x, qty]
                gk_p <- crossprod(p_1, q_1)/crossprod(b_vec, q_1)
                return(gk_p)
        })
        pindex_gk <- do.call(rbind, p_temp)
        # normalised
        p_out <- pindex_gk/pindex_gk[1]
        return(p_out)
}
