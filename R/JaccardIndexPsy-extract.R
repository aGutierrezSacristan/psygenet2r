#' @describeIn extract Extract function for JacardIndexPsy
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
