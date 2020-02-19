#' @title
#' Decomposing Value Added Growth into Explanatory Factors
#'
#' @description
#' This method for decomposing nominal value added growth is
#' proposed by Diewert and Fox (2018), which identifies the
#' contributions from efficiency change, growth of primary
#' inputs, changes in output and input prices, technical
#' progress and returns to scale.
#'
#' @param x A string (or a vector of strings) indicating the quantity columns.
#' @param w A string (or a vector of strings) indicating the input price columns.
#' @param y A string (or a vector of strings) indicating the the output quantity columns.
#' @param p A string (or a vector of strings) indicating the the output price columns.
#' @param t A string indicating the time period column.
#' @param data A data frame containing input prices, input quantities, output prices,
#' output quantities, and the time period.
#'
#' @export
#'
#' @return
#' A list containing a growth-value table and a level-value table of explanatory factors
#' for value added growth decomposition. It is sorted by the time period.
#'
#' @references
#' Diewert, W. E. and Fox, K. J. (2018). Decomposing value added growth into explanatory
#' factors. In The Oxford Handbook of Productivity Analysis, chapter 19,
#' page 625--662. Oxford University Press: New York.
#'
#' @usage
#' value_decom(x, w, y, p, t, data)
#'
#' @examples
#' # Use the built-in dataset "mining"
#' table1 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[1]]
#' table2 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[2]]
# Decomposing value added growth
value_decom <- function (x, w, y, p, t, data) {
        # data preparation
        df <- data[, c(x, w, y, p, t)]
        df <- df[order(df[, t]), ]
        df_mat <- as.matrix(df[, c(x, w, y, p)])
        len_df <- nrow(df)
        # use the function value_added()
        value_added <- function(tech, wage, price, input) {
                # use df_mat
                table_temp <- lapply(2:len_df, function(i) {
                        q_input <- df_mat[1:i - tech, x]
                        p_input <- df_mat[i - wage, w]
                        q_output <- df_mat[1:i - tech, y]
                        p_output <- df_mat[i - price, p]
                        if (length(p) > 1) {
                                c_min <- min((q_input %*% p_input)/(q_output %*% p_output))
                        } else {
                                c_min <- min((q_input %*% p_input)/(q_output * p_output))
                        }

                        q_input_single <- df_mat[i - input, x]
                        v_input <- p_input %*% q_input_single
                        v_added <- v_input/c_min
                        q_output_single <- df_mat[i - price, y]
                        v_output <- p_output %*% q_output_single
                        return(c(v_added, v_input, v_output))
                })
                table_out <- do.call(rbind, table_temp)
                return(table_out)
        }
        # alpha
        alpha_l <- value_added(1, 1, 0, 1)[, 1]/value_added(1, 1, 1, 1)[, 1]
        alpha_p <- value_added(0, 0, 0, 0)[, 1]/value_added(0, 0, 1, 0)[, 1]
        alpha_f <- sqrt(alpha_l * alpha_p)
        # beta
        beta_l <- value_added(0, 1, 0, 0)[, 2]/value_added(0, 1, 0, 1)[, 2]
        beta_p <- value_added(0, 0, 0, 0)[, 2]/value_added(0, 0, 0, 1)[, 2]
        beta_f <- sqrt(beta_l * beta_p)
        # gamma
        gamma_l <- value_added(0, 0, 1, 0)[, 1]/value_added(0, 1, 1, 0)[, 1]
        gamma_p <- value_added(1, 0, 0, 1)[, 1]/value_added(1, 1, 0, 1)[, 1]
        gamma_f <- sqrt(gamma_l * gamma_p)
        # epsilon
        eff <- value_added(0, 0, 0, 0)[, 3]/value_added(0, 0, 0, 0)[, 1]
        epsilon <- roll_div(c(1, eff))
        # tau
        tau_l <- value_added(0, 1, 1, 0)[, 1]/value_added(1, 1, 1, 0)[, 1]
        tau_p <- value_added(0, 0, 0, 1)[, 1]/value_added(1, 0, 0, 1)[, 1]
        tau_f <- sqrt(tau_l * tau_p)
        # productivity growth
        tfpg <- gamma_f * epsilon * tau_f
        # value added growth
        vgrow <- alpha_f * beta_f * tfpg
        # level values of V, A, B, C, E, T and TFP
        growth_mat <- cbind(vgrow, alpha_f, beta_f, gamma_f, eff, epsilon, tau_f, tfpg)
        level_mat <- apply(growth_mat, 2, cumprod)[, -5]
        level_mat <- rbind(rep(1, 7), level_mat)
        # output data frames
        period <- df[, t]
        grow_table <- data.frame(period[-1], growth_mat)
        names(grow_table) = c("period", "value", "alpha", "beta",
                              "gamma", "efficiency", "epsilon", "tau", "TFPG")
        level_table <- data.frame(period, level_mat)
        names(level_table) = c("period", "value", "A", "B", "C", "E", "T", "TFP")
        list_tbl <- list(grow_table, level_table)
        names(list_tbl) <- c("growth", "level")
        return(list_tbl)
}

#' @title
#' Converting Level Values to Growth Values
#'
#' @description
#' \code{roll_div()} converts level values to growth values for a vector.
#'
#' @param x A vector with level values.
#'
#' @return
#' A vector of growth values.
#'
#' @export
#'
#' @usage
#' roll_div(x)
#'
#' @examples
#' table2 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[2]]
#' roll_div(table2[, "TFP"])
roll_div <- function(x) {
        # use a lagged vector
        x0 <- c(NA, x)
        x1 <- c(x, NA)
        x_div <- x1/x0
        x_div <- x_div[-c(1, length(x_div))]
        return(x_div)
}

#' @title
#' Converting Growth Values to Level Values
#'
#' @description
#' \code{roll_prod()} converts growth values to level values for a vector.
#'
#' @param x A vector with growth values.
#'
#' @return
#' A vector of level values.
#'
#' @export
#'
#' @usage
#' roll_prod(x)
#'
#' @examples
#' table1 <- value_decom(c("h2","x2"), c("w2","u2"), "y2", "p2", "year", mining)[[1]]
#' roll_prod(table1[, "TFPG"])
roll_prod <- function(x) {
        x_prod <- lapply(1:length(x), function(y) prod(x[1:y]))
        x_prod <- unlist(x_prod)
        return(x_prod)
}
