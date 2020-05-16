#' @title
#' Productivity Dynamics
#'
#' @description
#' Productivity dynamics reflect firm contributions to
#' productivity growth over periods when firms enter or
#' exit an industry. \code{dynamics} summarises a series
#' of decomposition methods that are centred on the contributions
#' from incumbents, entrants and exits. It applies to other
#' weighted aggregation measures analogous to aggregate productivity
#' as well.
#'
#' @param df A data frame sorted by the time period column.
#' @param x A string indicating the productivity (or analogous measures) column.
#' @param s A string indicating the market share column.
#' @param id A string indicating the identity column.
#' @param tm A string indicating the time period column.
#' @param typ Relevant types of productivity dynamics. Options include "df" for
#' Diewert-Fox decomposition (by default), "bhc" for Baily-Hulten-Campbell, "gr" for
#' Griliches-Regev, "fhk" for Foster-Haltiwanger-Krizan, "bg" for Baldwin-Gu, and
#' "mp" for Melitz-Polanec.
#'
#' @return
#' A data frame consisting of the time period and firm contributions.
#'
#' @usage
#' dynamics(df, x, s, id, tm, typ = "df")
#'
#' @examples
#' # Use the built-in data set "firms"
#' # DF decomposition of firm dynamics
#' dym_df <- dynamics(firms, "tfp", "s", "id", "t")
#' # BG decomposition of firm dynamics
#' dym_bg <- dynamics(firms, "tfp", "s", "id", "t", "bg")
#'
#' @export
dynamics <- function(df, x, s, id, tm, typ = "df") {
        # data preparation
        df <- df[, c(x, s, id, tm)]
        tm_vec <- unique(df[, tm])
        # data segment
        dym_out <- lapply(1:(length(tm_vec)-1), function(y) {
                df_seg <- df[df[, tm]==tm_vec[y] | df[, tm]==tm_vec[y+1], ]
                # label=1 for matched firms, 2 for new, 3 for disappearing
                label <- duplicated(df_seg[, id]) + duplicated(df_seg[, id], fromLast = TRUE)
                label[df_seg[, tm]==tm_vec[y+1] & label==0] <- 2
                label[df_seg[, tm]==tm_vec[y] & label==0] <- 3
                # relative share
                names(df_seg) <- c("tfp", "share", "firm", "period")
                df_seg <- cbind(df_seg, label)
                df_seg <- merge(df_seg, aggregate(share~label+period, data=df_seg, sum), by=c("label", "period"), suffixes = c("", "_sum"))
                df_seg[, "share_relative"] <- df_seg[, "share"]/df_seg[, "share_sum"]
                # rearrange data
                df_seg <- df_seg[order(df_seg[, "firm"], df_seg[, "period"]),]
                if (df_seg[1, "period"] != tm_vec[y]) {
                        df_seg <- df_seg[nrow(df_seg):1, ]
                }
                label <- df_seg[, "label"]
                period <- df_seg[, "period"]==tm_vec[y+1]
                s_1 <- df_seg[, "share"]
                s_re_1 <- df_seg[, "share_relative"]
                p_1 <- df_seg[, "tfp"]
                # lag values of productivity and share
                re_tick <- !duplicated(df_seg[, "firm"])
                s_0 <- c(NA, s_1[-nrow(df_seg)])
                s_0[re_tick] <- NA
                p_0 <- c(NA, p_1[-nrow(df_seg)])
                p_0[re_tick] <- NA
                s_re_0 <- c(NA, s_re_1[-nrow(df_seg)])
                s_re_0[re_tick] <- NA
                # firm dynamics with different methods
                p_dym <- switch(typ, "bhc" = bhc_dym(p_0, p_1, s_0, s_1, label),
                                "gr" = gr_dym(p_0, p_1, s_0, s_1, label, period),
                                "fhk" = fhk_dym(p_0, p_1, s_0, s_1, label, period),
                                "bg" = bg_dym(p_0, p_1, s_0, s_1, label, period),
                                "df" = df_dym(p_0, p_1, s_0, s_1, s_re_0, s_re_1, label, period),
                                "mp" = mp_dym(p_0, p_1, s_0, s_1, s_re_0, s_re_1, label, period)
                )
                return(p_dym)
        })
        dym_out <- do.call(rbind, dym_out)
        dym_out <- data.frame(tm_vec[-1], dym_out)
        colnames(dym_out) <- switch(typ, "bhc" = c(tm, "within", "between", "entry", "exit"),
                                    "gr" = c(tm, "within", "between", "entry", "exit"),
                                    "fhk" = c(tm, "within", "between", "cross", "entry", "exit"),
                                    "bg" = c(tm, "within", "between", "entry", "exit"),
                                    "df" = c(tm, "within", "between", "entry", "exit"),
                                    "mp" = c(tm, "mean", "covariance", "entry", "exit")
        )
        dym_out[, "total"] <- apply(dym_out[, -1], 1, sum)
        return(dym_out)
}

#' @title
#' Baily-Hulten-Campbell decomposition
#'
#' @keywords internal
#'
#' @noRd
bhc_dym <- function(p_0, p_1, s_0, s_1, label) {
        # within effect
        tfp_diff_lagshare <- (p_1-p_0)*s_0
        wi <- sum(tfp_diff_lagshare[label==1], na.rm = TRUE)
        # between effect
        share_diff_tfp <- (s_1-s_0)*p_1
        bt <- sum(share_diff_tfp[label==1],na.rm = TRUE)
        # entry effect
        tfp_times_share <- p_1*s_1
        ent <- sum(tfp_times_share[label==2],na.rm = TRUE)
        # exit effect
        ex <- -sum(tfp_times_share[label==3],na.rm = TRUE)
        # summary
        p_out <- c(wi, bt, ent, ex)
        return(p_out)
}

#' @title
#' Griliches-Regev decomposition
#'
#' @keywords internal
#'
#' @noRd
gr_dym <- function(p_0, p_1, s_0, s_1, label, period) {
        # within effect
        tfp_diff_avgshare <- (p_1-p_0)*(s_0+s_1)*0.5
        wi <- sum(tfp_diff_avgshare[label==1],na.rm = TRUE)
        # between effect
        tfp_times_share <- p_1*s_1
        avg_sum <- (sum(tfp_times_share[period==1],na.rm = TRUE)
                       +sum(tfp_times_share[period==0],na.rm = TRUE))*0.5
        share_diff_avgtfp <- (s_1-s_0)*(p_1+p_0-2*avg_sum)*0.5
        bt <- sum(share_diff_avgtfp[label==1],na.rm = TRUE)
        # entry effect
        deltatfp_times_share <- (p_1-avg_sum)*s_1
        ent <- sum(deltatfp_times_share[label==2],na.rm = TRUE)
        # exit effect
        ex <- -sum(deltatfp_times_share[label==3],na.rm = TRUE)
        # summary
        p_out <- c(wi, bt, ent, ex)
        return(p_out)
}

#' @title
#' Foster-Haltiwanger-Krizan decomposition
#'
#' @keywords internal
#'
#' @noRd
fhk_dym <- function(p_0, p_1, s_0, s_1, label, period) {
        # within effect
        tfp_diff_lagshare <- (p_1-p_0)*s_0
        wi <- sum(tfp_diff_lagshare[label==1],na.rm = TRUE)
        # between effect
        tfp_times_share <- p_1*s_1
        lag_sum <- sum(tfp_times_share[period==0],na.rm = TRUE)
        share_diff_deltalagtfp <- (s_1-s_0)*(p_0-lag_sum)
        bt <- sum(share_diff_deltalagtfp[label==1],na.rm = TRUE)
        # cross effect
        tfp_diff_share_diff <- (p_1-p_0)*(s_1-s_0)
        crs <- sum(tfp_diff_share_diff[label==1],na.rm = TRUE)
        # entry effect
        deltalagtfp_times_share <- (p_1-lag_sum)*s_1
        ent <- sum(deltalagtfp_times_share[label==2],na.rm = TRUE)
        # exit effect
        ex <- -sum(deltalagtfp_times_share[label==3],na.rm = TRUE)
        # summary
        p_out <- c(wi, bt, crs, ent, ex)
        return(p_out)
}

#' @title
#' Baldwin-Gu decomposition
#'
#' @keywords internal
#'
#' @noRd
bg_dym <- function(p_0, p_1, s_0, s_1, label, period) {
        # within effect
        tfp_diff_avgshare <- (p_1-p_0)*(s_0+s_1)*0.5
        wi <- sum(tfp_diff_avgshare[label==1],na.rm = TRUE)
        # between effect
        tfp_times_share <- p_1*s_1
        exit_sum <- sum(tfp_times_share[label==3],na.rm = TRUE)/
                sum(s_1[label==3],na.rm = TRUE)
        share_diff_deltaexittfp <- (s_1-s_0)*(p_1+p_0-2*exit_sum)*0.5
        bt <- sum(share_diff_deltaexittfp[label==1],na.rm = TRUE)
        # entry effect
        deltaexittfp_times_share <- (p_1-exit_sum)*s_1
        ent <- sum(deltaexittfp_times_share[label==2],na.rm = TRUE)
        # exit effect
        ex <- -sum(deltaexittfp_times_share[label==3],na.rm = TRUE)
        # summary
        p_out <- c(wi, bt, ent, ex)
        return(p_out)
}

#' @title
#' Diewert-Fox decomposition
#'
#' @keywords internal
#'
#' @noRd
df_dym <- function(p_0, p_1, s_0, s_1, s_re_0, s_re_1, label, period) {
        # within effect
        tfp_diff_newshare <- (p_1-p_0)*(s_re_1+s_re_0)*0.5
        wi <- sum(tfp_diff_newshare[label==1],na.rm = TRUE)
        # bewteen effect
        newshare_diff_tfp <- (s_re_1-s_re_0)*(p_1+p_0)*0.5
        bt <- sum(newshare_diff_tfp[label==1],na.rm = TRUE)
        # entry effect
        tfp_times_share <- p_1*s_1
        match_sum <- sum(tfp_times_share[label==1 & period==1],na.rm = TRUE)/
                sum(s_1[label==1 & period==1],na.rm = TRUE)
        newshare_diff_deltamatchtfp <- s_re_1*(p_1-match_sum)
        ent <- sum(newshare_diff_deltamatchtfp[label==2],na.rm = TRUE)*
                sum(s_1[label==2],na.rm = TRUE)
        # exit effect
        lagmatch_sum <- sum(tfp_times_share[label==1 & period==0],na.rm = TRUE)/
                sum(s_1[label==1 & period==0],na.rm = TRUE)
        newshare_diff_deltalagmatchtfp <- s_re_1*(p_1-lagmatch_sum)
        ex <- -sum(newshare_diff_deltalagmatchtfp[label==3],na.rm = TRUE)*
                sum(s_1[label==3],na.rm = TRUE)
        # summary
        p_out <- c(wi, bt, ent, ex)
        return(p_out)
}

#' @title
#' Melitz-Polanec decomposition
#'
#' @keywords internal
#'
#' @noRd
mp_dym <- function(p_0, p_1, s_0, s_1, s_re_0, s_re_1, label, period) {
        # mean effect
        avg_match <- mean(p_1[label==1 & period==1],na.rm = TRUE)
        avg_lagmatch <- mean(p_1[label==1 & period==0],na.rm = TRUE)
        mean <- avg_match-avg_lagmatch
        # covariance effect
        avg_share <- 2/sum(label==1)
        cov_diff <- (s_re_1-avg_share)*(p_1-avg_match)-
                (s_re_0-avg_share)*(p_0-avg_lagmatch)
        cov <- sum(cov_diff[label==1],na.rm = TRUE)
        # entry effect
        tfp_times_share <- p_1*s_1
        match_sum <- sum(tfp_times_share[label==1 & period==1],na.rm = TRUE)/
                sum(s_1[label==1 & period==1],na.rm = TRUE)
        newshare_diff_deltamatchtfp <- s_re_1*(p_1-match_sum)
        ent <- sum(newshare_diff_deltamatchtfp[label==2],na.rm = TRUE)*
                sum(s_1[label==2],na.rm = TRUE)
        # exit effect
        lagmatch_sum <- sum(tfp_times_share[label==1 & period==0],na.rm = TRUE)/
                sum(s_1[label==1 & period==0],na.rm = TRUE)
        newshare_diff_deltalagmatchtfp <- s_re_1*(p_1-lagmatch_sum)
        ex <- -sum(newshare_diff_deltalagmatchtfp[label==3],na.rm = TRUE)*
                sum(s_1[label==3],na.rm = TRUE)
        # summary
        p_out <- c(mean, cov, ent, ex)
        return(p_out)
}
