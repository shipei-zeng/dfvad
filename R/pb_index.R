#' @title
#' Bilateral Price Indexes
#'
#' @description
#' Bilateral indexes refer to the case when only two periods are
#' compared each time. \code{pb_index()} computes price indexes in a
#' bilateral approach.
#'
#' @param df A data frame sorted by the time period column.
#' @param p A string indicating the price column.
#' @param qty A string indicating the quantity column.
#' @param id A string indicating the identity column.
#' @param tm A string indicating the time period column. Each period must
#' contain two observations at least.
#' @param typ Relevant types of price indexes. Options include "f"
#' for Fisher price indexes (by default), "t" for Tornqvist price indexes,
#' "l" for Laspeyres price indexes, and "p" for Paasche price indexes.
#' @param seq Index construction sequences when the number of periods
#' is larger than 2. Options include "ch" for chained indexes (by default),
#' and "fb" for fixed base indexes.
#' @param bsk The choice of baskets when items are not matched over
#' multiple periods. Options include "flx" (by default) for a flexible basket
#' that varies depending on the maximal number of matched items in
#' two periods each time, and "cst" for a constant basket
#' that takes the maximal number of matched items across all periods.
#'
#' @return
#' A data frame consisting of the time period and price indexes.
#'
#' @export
#'
#' @usage
#' pb_index(df, p, qty, id, tm, typ = "f", seq = "ch", bsk = "flx")
#'
#' @examples
#' # Use the built-in data set "prices"
#' # Laspeyres fixed base indexes with a constant basket
#' df <- prices[[1]]
#' df <- df[order(df[,"t"]),]
#' index1 <- pb_index(df, "p", "q", "id", "t", typ = "l", seq = "fb", bsk = "cst")
#' # Fisher chained indexes with a flexible basket
#' df <- prices[[2]]
#' df <- df[order(df[,"t"]),]
#' index2 <- pb_index(df, "p", "q", "id", "t")
pb_index <- function(df, p, qty, id, tm, typ="f", seq="ch", bsk="flx") {
        # data preparation
        df <- df[, c(p, qty, id, tm)]
        tm_vec <- unique(df[, tm])
        # basket filter
        if (bsk == "cst") {
                count_id <- table(df[, id])
                match_id <- count_id[count_id==length(tm_vec)]
                df <- df[as.character(df[, id]) %in% names(match_id), ]
        }
        # data segment
        p_out <- lapply(1:(length(tm_vec)-1), function(x) {
                # chain method
                tm_base <- switch (seq, "ch" = x, "fb" = 1)
                df_seg <- df[df[, tm]==tm_vec[tm_base] | df[, tm]==tm_vec[x+1], ]
                # basket filter
                if (bsk == "flx") {
                        match_id_1 <- duplicated(df_seg[, id])
                        match_id_0 <- duplicated(df_seg[, id], fromLast = TRUE)
                        match_id <- match_id_0 | match_id_1
                        df_seg <- df_seg[match_id, ]
                }
                # base period and current period
                p_0 <- df_seg[df_seg[, tm]==tm_vec[tm_base], p]
                p_1 <- df_seg[df_seg[, tm]==tm_vec[x+1], p]
                q_0 <- df_seg[df_seg[, tm]==tm_vec[tm_base], qty]
                q_1 <- df_seg[df_seg[, tm]==tm_vec[x+1], qty]
                # compute bilateral indexes
                p_index <- switch (typ, "f" = fis_index,
                                   "l" = las_index,
                                   "p" = paa_index,
                                   "t" = tor_index
                )
                p_temp <- p_index(p_0, p_1, q_0, q_1)
                return(p_temp)
        })
        p_out <- c(1, unlist(p_out))
        # chain method
        if (seq == "ch") {
                p_out <- cumprod(p_out)
        }
        df_p <- data.frame(tm_vec, p_out)
        colnames(df_p) <- c(tm, "index")
        return(df_p)
}

#' @title
#' Laspeyres indexes
#'
#' @keywords internal
#'
#' @noRd
las_index <- function(p_0, p_1, q_0, q_1) {
        p_out <- crossprod(p_1, q_0)/crossprod(p_0, q_0)
        return(p_out)
}

#' @title
#' Paasche indexes
#'
#' @keywords internal
#'
#' @noRd
paa_index <- function(p_0, p_1, q_0, q_1) {
        p_out <- crossprod(p_1, q_1)/crossprod(p_0, q_1)
        return(p_out)
}

#' @title
#' Fisher indexes
#'
#' @keywords internal
#'
#' @noRd
fis_index <- function(p_0, p_1, q_0, q_1) {
        p_las <- crossprod(p_1, q_0)/crossprod(p_0, q_0)
        p_paa <- crossprod(p_1, q_1)/crossprod(p_0, q_1)
        p_out <- sqrt(p_las*p_paa)
        return(p_out)
}

#' @title
#' Tornqvist indexes
#'
#' @keywords internal
#'
#' @noRd
tor_index <- function(p_0, p_1, q_0, q_1) {
        s_0 <- p_0*q_0/sum(p_0*q_0)
        s_1 <- p_1*q_1/sum(p_1*q_1)
        p_out <- prod((p_1/p_0)^(0.5*(s_0+s_1)))
        return(p_out)
}
