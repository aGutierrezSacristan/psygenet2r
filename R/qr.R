#' \code{DataGeNET.Psy} obtained from quering PsyGeNET for gene '4852'.
#'
#' A dataset obtained from PsyGeNET after being queried with 
#' \code{psygenetGene} usig the term '4852' on \code{"ALL"} databse.
#'
#' @usage data("qr")
#' @format   The format is:
#' Formal class 'DataGeNET.Psy' [package "psygenet2r"] with 5 slots
#'   .. type    : chr "gene"
#'   .. search  : chr ""
#'   .. database: chr "ALL"
#'   .. term    : chr "4852"
#'   .. qresult :'data.frame'
#' @return A \code{DataGeNET.Psy} object.
#' @examples
#' if (require(psygenet2r)){
#'  ngene(qr)
#'  ndisease(qr)
#' }
#' @source \url{http://psygenet.org}
"qr"