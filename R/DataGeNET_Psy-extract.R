#' @describeIn DataGeNET.Psy extract raw data
#' @aliases DataGeNET.Psy-methods
#' @param object Object of class \code{DataGeNET.Psy}
#' @return A \code{data.frame} containing the raw result from PsyGeNET
#' @examples
#' extract(qr)[1:2, ] # get internat data.frame
#' @export
setMethod( "extract",
    signature = "DataGeNET.Psy",
    definition = function( object ) {
        return( object@qresult )
    }
)
