#' @title Sample data for weighted average aggregation
#'
#' @description Explanatory factors of value added decomposition
#' adopted to demonstrate the aggregation over industries.
#'
#' @references
#' Zeng, S., Parsons, S., Diewert, W. E. and Fox, K. J. (2018).
#' Industry and state level value added and productivity
#' decompositions. Presented in EMG Worshop 2018, Sydney.
#'
#' @format A data frame with the following columns:
#' \describe{
#' \item{year}{A time period column.}
#' \item{p}{Output prices.}
#' \item{y}{Output quantities.}
#' \item{alpha}{Net output price indexes.}
#' \item{beta}{Input quantity indexes}
#' \item{gamma}{Input mix indexes.}
#' \item{epsilon}{Value added efficiency indexes.}
#' \item{tau}{Technical progress indexes.}
#' \item{industry}{Industry codes.}
#' }
"sector"
