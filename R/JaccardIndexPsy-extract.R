#' Raw data from \code{DataGeNET.Psy} and \code{JaccardIndexPsy}.
#' 
#' Obtain the raw data from a PsyGeNET's query  stored in a 
#' \code{DataGeNET.Psy} object or the raw data with all the Jaccard Index for 
#' the disease of interest of an \code{JaccardIndexPsy} object.
#'
#' @name extract
#' @rdname extract-methods
#' @aliases extract
#' @param object Object of class \code{JaccardIndexPsy}
#' @return A \code{data.frame} with the result Jaccard Index for each disease.
#' @examples
#' \dontrun{
#' #Being x an JaccardIndexPsy
#' extract(x)
#' }
#' @export
setMethod( "extract",
  signature = "JaccardIndexPsy",
  definition = function( object ) {
    tt <- object@table
    tt$JaccardIndex <- as.numeric(as.character(tt$JaccardIndex))
    tt$pval <- as.numeric(as.character(tt$pval))
    return( tt )
  }
)
