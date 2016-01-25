#' Raw data from \code{DataGeNET.Psy} and \code{JaccardIndexPsy}.
#' 
#' Obtain the raw data from a PsyGeNET's query  stored in a 
#' \code{DataGeNET.Psy} object or the raw data with all the Jaccard Index for 
#' the disease of interest of an \code{JaccardIndexPsy} object.
#'
#' @name extract
#' @rdname extract-methods
#' @aliases extract
#' @param object Object of class \code{DataGeNET.Psy}
#' @return A \code{data.frame} containing the raw result from PsyGeNET
#' @examples
#' if (require(psygenet2r)){
#'  extract(qr)[1:2, ] # get internat data.frame
#' }
#' @export
setMethod( "extract",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( object@qresult )
    }
)
