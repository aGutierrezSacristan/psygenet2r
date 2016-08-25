#' @describeIn extract Extract function for JacardIndexPsy
#' @param order.cl Order resulting \code{data.frame} by the name of this column.
#' @return A \code{data.frame} with the result Jaccard Index for each disease.
#' @examples
#' \dontrun{
#' #Being x an JaccardIndexPsy
#' extract(x)
#' }
#' @export
setMethod( "extract",
  signature = "JaccardIndexPsy",
  definition = function( object, order.cl = "pval", ... ) {
    tt <- object@table
    tt$JaccardIndex <- as.numeric(as.character(tt$JaccardIndex))
    tt$pval <- as.numeric(as.character(tt$pval))
    tt <- tt[order(tt[ , order.cl]), ]
    return( tt )
  }
)
