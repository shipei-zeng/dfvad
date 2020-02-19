#' @title
#' Aggregation over Sectors with a Weighted Average Approach
#'
#' @description
#' This "bottom up" approach uses weighted averages of the
#' sectoral decompositions to provide an approximate
#' decomposition into explanatory components at the aggregate
#' level. Specifically, the Tornqvist index is adopted in the
#' aggregation.
#'
#' @param y A string (or a vector of strings) indicating the output quantity columns.
#' @param p A string (or a vector of strings) indicating the output price columns.
#' @param id A string indicating the industry column.
#' @param t A string indicating the time period column.
#' @param alpha A string indicating net output price indexes.
#' @param beta A string indicating input quantity indexes.
#' @param gamma A string indicating input mixe indexes.
#' @param epsilon A string indicating value added efficiency indexes.
#' @param tau A string indicating technical progress indexes.
#' @param data A data frame containing input prices, input quantities,
#' industry identities, the time period, and explanatory factors of
#' value added growth.
#'
#' @importFrom stats aggregate
#'
#' @export
#'
#' @return
#' A list containing a growth-value table and a level-value table of
#' explanatory factors for value added growth decomposition. It is
#' sorted by the time period.
#'
#' @references
#' Diewert, W. E. and Fox, K. J. (2018). Decomposing value added growth into explanatory
#' factors. In The Oxford Handbook of Productivity Analysis, chapter 19,
#' page 625--662. Oxford University Press: New York.
#'
#' @usage
#' t_weight(y, p, id, t, alpha, beta, gamma, epsilon, tau, data)
#'
#' @examples
#' # Use the built-in dataset "sector"
#' table1 <- t_weight("y", "p", "industry", "year", "alpha",
#'         "beta", "gamma", "epsilon", "tau", sector)[[1]]
#' table2 <- t_weight("y", "p", "industry", "year", "alpha",
#'         "beta", "gamma", "epsilon", "tau", sector)[[2]]
# Weighted average aggregation using value added shares
t_weight <- function (y, p, id, t, alpha, beta, gamma, epsilon, tau, data) {
        # data preparation
        df <- data[, c(y, p, id, t, alpha, beta, gamma, epsilon, tau)]
        df$value <- apply(as.matrix(df[, p] * df[, y]), 1, sum)
        names(df)[4] <- "period"
        # value added shares
        df <- merge(df, aggregate(value ~ period, data = df, sum),
                    by = "period", suffixes = c("", "sum"))
        df <- df[order(df[, id], df[, "period"]), ]
        share <- df$value/df$valuesum
        # lagged value added shares
        lagshare <- c(NA, share[-nrow(df)])
        lagshare[which(!duplicated(df[, id]))] <- NA
        # mean shares
        sharemean <- 0.5 * (share + lagshare)
        # use t_index
        t_index <- lapply(c(alpha, beta, gamma, epsilon, tau), function(x) {
                df_new <- df[, c("period", x)]
                df_new$power <- df_new[, x]^sharemean
                tindex <- aggregate(power ~ period, data = df_new, prod, na.rm = TRUE)
                tindex <- tindex$power
                return(tindex[-1])
        })
        t_index <- do.call(cbind, t_index)
        # compose factors
        valuesum <- df[which(!duplicated(df$period)), "valuesum"]
        vgrow <- roll_div(valuesum)
        new_period <- unique(df$period)
        grow_table <- data.frame(new_period[-1], vgrow, t_index)
        names(grow_table) = c("period", "value", "alpha", "beta",
                              "gamma", "epsilon", "tau")
        grow_table$TFPG <- grow_table$value/(grow_table$alpha * grow_table$beta)
        level_table <- apply(grow_table[, -1], 2, cumprod)
        level_table <- data.frame(rbind(rep(1, ncol(level_table)), level_table))
        level_table <- cbind(new_period, level_table)
        names(level_table) = c("period", "value", "A", "B", "C", "E", "T", "TFP")
        tbl_list <- list(grow_table, level_table)
        names(tbl_list) <- c("growth", "level")
        return(tbl_list)
}
