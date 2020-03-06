#' @title Sample Data for Price Indexes
#'
#' @description  Prices, quantities, identities and time periods
#' adopted to demonstrate the computation of price indexes.
#'
#' @references
#' Diewert, W.E., and Fox, K. J. 2018. Substitution bias in
#' multilateral methods for CPI construction using scanner
#' data. Discussion Papers 2018-13, School of Economics,
#' the University of New South Wales.
#'
#' @format A list of data frames with the following columns:
#' \describe{
#' \item{t}{A time period column.}
#' \item{id}{An identity column.}
#' \item{p}{A price column.}
#' \item{q}{A quantity column.}
#' }
#' These data frames are produced with different elasticities
#' that can be specified by the sub-list names.
"prices"
