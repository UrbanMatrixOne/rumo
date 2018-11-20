#' Annuity Payment Calculator
#' This function calculates the annuity payment whose present value matches a given PV at a given Discount Rate
#' @param npv The NPV of the annuity stream
#' @param discount_rate The rate for discounting the annuity stream
#' @param number_years How many years the annuity will be paid
#' @export
#' @examples
#' Annuity_Payment()
Annuity_Payment <- function(npv,discount_rate, number_years)
{
  discount_rate*npv/ (1-(1+discount_rate)**(-number_years))

}

#' Discounted Payback Period
#' This function calculates the Discounted Payback Period of a stream of cashflows
#' @param cashflows the cashflows to evaluate
#' @param npv the upfront cost of the investment
#' @param discount_rate the discount rate
#' @export
#' @examples
#' DPBP()
DPBP <- function(cashflows,npv, discount_rate){
  discount_factors <- (1+discount_rate) ^-(1:length(cashflows))
  PBP(discount_factors * cashflows, npv)
}
#' Payback Period (Non discounted)
#' This function calculates the Payback Period of a stream of cashflows
#' @param cashflows the cashflows to evaluate
#' @param npv the upfront cost of the investment
#' @export
#' @examples
#' PBP()
PBP <- function(cashflows,npv){
  which(cumsum(cashflows) > npv)[1] /12
}
#' IRR
#' This function calculates the Internal Rate of Return
#' @param cashflows the cashflows to evaluate
#' @param npv the upfront cost of the investment
#' @export
#' @examples
#' IRR()
IRR <- function(cashflows, npv) {
  #could replace this with RCPP implementation to be faster.
  FinCal::irr(c(-npv,cashflows))
}

#! This function would calculate the levelized cost of electricity (not yet implemented)
# TODO
LCOE <-  function(){}

#' NPV
#' This function calculates the Internal Rate of Return
#' @param cashflows the cashflows to evaluate
#' @param npv the upfront cost of the investment
#' @export
#' @examples
#' NPV()
NPV <- function(cashflows, discount_rate  ) {
#  print(discount_rate)
  discount_factors <- (1+discount_rate) ^-(1:length(cashflows))
  return(sum(discount_factors * cashflows))
}
